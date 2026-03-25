#' Retrieve MODIS Land Cover (MCD12Q1) Version 6.1
#'
#' Access and process the MODIS/Terra+Aqua Land Cover Type Yearly Global 500m 
#' dataset. This function identifies required tiles for a region, facilitates 
#' browser-based authentication, and returns a seamless raster of the IGBP 
#' land cover classification.
#'
#' @param where A numeric vector of length 4 (bounding box) or a matrix/data.frame 
#' with two columns (longitude, latitude) specifying the spatial region of interest. 
#' Bounding box format: c(xmin, ymin, xmax, ymax).
#'
#' @param year An integer specifying the year (2001 to 2023). Default is 2023.
#'
#' @param downloaddir A character string specifying the directory where HDF tiles 
#' are stored. Defaults to a "MODIS_Year" folder in your system Downloads.
#'
#' @details
#' The function uses the NASA CMR API to find the exact sinusoidal tiles (e.g., h10v04) 
#' needed for your area. Because MODIS data is behind NASA Earthdata authentication, 
#' the function will open your browser for the first missing tile to initiate a session.
#'
#' The returned raster uses the IGBP (International Geosphere-Biosphere Programme) 
#' classification (LC_Type1):
#' - 11: Permanent Wetlands
#' - 12: Croplands
#' - 17: Water Bodies
#'
#' @return If 'where' is a bounding box, returns a SpatRaster cropped to the region. 
#' If 'where' is a coordinate set, returns a data frame with extracted LC values.
#'
#' @author Abdollah Jalilian
#' 
#' @export
get_landcover <- function(where,
                          year = 2023,
                          downloaddir = NULL) 
{
  # validate input: bounding box or coordinate matrix/data frame
  if (is.numeric(where) && length(where) == 4) 
  {
    bbox <- where
  } else if (inherits(where, c("matrix", "data.frame")) && ncol(where) == 2) 
  {
    bbox <- c(min(where[, 1]) - 0.2, min(where[, 2]) - 0.2,
              max(where[, 1]) + 0.2, max(where[, 2]) + 0.2)
  } else {
    stop("'where' must be a numeric vector (bbox) or a coordinate matrix/df.")
  }
  
  # validate bounding box format
  if (bbox[1] >= bbox[3] || bbox[2] >= bbox[4])
    stop("Bounding box must be in the format c(xmin, ymin, xmax, ymax) with valid coordinates.")
  
  
  # set the default download directory
  if (is.null(downloaddir))
  {
    downloaddir <- file.path(Sys.getenv("HOME"), "Downloads", paste0("MODIS_", year))
    if (Sys.info()["sysname"] == "Windows")
      downloaddir <- gsub("\\\\", "/", file.path(Sys.getenv("USERPROFILE"), 
                                                 "Downloads", paste0("MODIS_", year)))
  }
  if (!dir.exists(downloaddir)) 
    dir.create(downloaddir, recursive=TRUE)
  
  # query NASA CMR API for specific HDF links
  message("Querying NASA CMR API for year ", year, "...")
  q <- list(collection_concept_id="C2484079608-LPCLOUD",
            temporal=paste0(year, "-01-01T00:00:00Z,", year, "-12-31T23:59:59Z"), 
            bounding_box=paste(bbox, collapse = ","), 
            page_size=100)
  res <- httr::GET("https://cmr.earthdata.nasa.gov/search/granules.json", query=q)
  httr::stop_for_status(res)
  entries <- httr::content(res, "parsed")$feed$entry
  if (length(entries) == 0) 
    stop("No MODIS tiles found for this year/region.")
  
  hdf_links <- sapply(entries, function(x) {
    data_links <- Filter(function(l) l$rel == "http://esipfed.org/ns/fedsearch/1.1/data#", x$links)
    hrefs <- sapply(data_links, function(l) l$href)
    return(grep("\\.hdf$", hrefs, value = TRUE)[1])
  })
  hdf_links <- as.character(na.omit(unique(hdf_links)))
  
  # check for missing files and trigger Browser authentication
  tile_files <- file.path(downloaddir, basename(hdf_links))
  missing_links <- hdf_links[!file.exists(tile_files)]
  if (length(missing_links) > 0)
  {
    utils::browseURL(missing_links[1])
    message("Found ", length(missing_links), " missing tiles.\n",
            "1) Log in to NASA Earthdata in the browser.\n",
            "2) Download the tiles to: ", downloaddir)
    cat("Press [Enter] once all downloads are complete...")
    readline()
  }
  # re-check after download
  tile_files <- file.path(downloaddir, basename(hdf_links))
  if (!all(file.exists(tile_files)))
    stop("Some required tiles are still missing in: ", downloaddir)
  if (length(tile_files) == 0) 
    stop("No HDF files found in: ", downloaddir)
  
  # load and process raster data
  message("Mosaicing and reprojecting...")
  # extract LC_Type1 (Layer 1) from each HDF
  rast_list <- lapply(tile_files, function(f) {
    r <- terra::rast(f)
    lname <- grep("LC_Type1", names(r), value = TRUE)
    if (length(lname) == 0) 
      stop("LC_Type1 layer not found in: ", f)
    r[[lname]]
  })
  mcd_mosaic <- do.call(terra::merge, rast_list)
  
  # target extent in WGS84
  wgs84_ext <- terra::as.polygons(
    terra::ext(bbox[1], bbox[3], bbox[2], bbox[4]), crs="EPSG:4326")
  
  # if input was points, extract and return
  if (inherits(where, c("matrix", "data.frame"))) 
  {
    pts_vec <- terra::vect(as.data.frame(where), geom=c(1,2), crs="EPSG:4326")
    pts_proj <- terra::project(pts_vec, terra::crs(mcd_mosaic))
    extracted <- terra::extract(mcd_mosaic, pts_proj, ID=FALSE)
    return(cbind(where, extracted))
  } else {
    # if input was bbox, crop and reproject for the user
    poly_sinu <- terra::project(wgs84_ext, terra::crs(mcd_mosaic))
    r_crop <- terra::crop(mcd_mosaic, poly_sinu)
    # project to WGS84 to match your other datasets
    return(terra::project(r_crop, "EPSG:4326", method="near"))
  }
}
