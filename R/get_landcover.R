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
#' @param year An integer specifying the year (2001 to 2024). Default is 2024.
#'
#' @param downloaddir A character string specifying the directory where HDF tiles
#' are stored. Defaults to your system Downloads.
#'
#' @details
#' The function uses the NASA CMR API to find the exact sinusoidal tiles (e.g., h10v04)
#' needed for your area. Because MODIS data is behind NASA Earthdata authentication,
#' the function will open your browser for the first missing tile to initiate a session.
#'
#' The returned raster uses the IGBP (International Geosphere-Biosphere Programme)
#' classification (LC_Type1). The 17 categories are:
#' \itemize{
#'   \item \strong{1:} Evergreen Needleleaf Forests
#'   \item \strong{2:} Evergreen Broadleaf Forests
#'   \item \strong{3:} Deciduous Needleleaf Forests
#'   \item \strong{4:} Deciduous Broadleaf Forests
#'   \item \strong{5:} Mixed Forests
#'   \item \strong{6:} Closed Shrublands
#'   \item \strong{7:} Open Shrublands
#'   \item \strong{8:} Woody Savannas
#'   \item \strong{9:} Savannas
#'   \item \strong{10:} Grasslands
#'   \item \strong{11:} \strong{Permanent Wetlands} (Land with a permanent mixture of vegetation and water)
#'   \item \strong{12:} \strong{Croplands} (Lands covered with temporary crops)
#'   \item \strong{13:} Urban and Built-up Lands
#'   \item \strong{14:} Cropland/Natural Vegetation Mosaics
#'   \item \strong{15:} Permanent Snow and Ice
#'   \item \strong{16:} Barren (Sparsely vegetated)
#'   \item \strong{17:} \strong{Water Bodies} (Oceans, seas, lakes, reservoirs, rivers)
#' }
#'
#' @return If 'where' is a bounding box, returns a SpatRaster cropped to the region.
#' If 'where' is a coordinate set, returns a data frame with extracted LC values.
#'
#' @references
#' Friedl, M. A., & Sulla-Menashe, D. (2021). MODIS/Terra+Aqua Land Cover Type
#' Yearly L3 Global 500m SIN Grid V061 (MCD12Q1). NASA LP DAAC.
#' https://doi.org/10.5067/MODIS/MCD12Q1.061
#'
#' @examples
#' \dontrun{
#'   # Retrieve land cover for a bounding box
#'   lc_raster <- get_landcover(c(47, 34, 48, 35), year=2024)
#'   terra::plot(lc_raster)
#'
#'   # Retrieve land cover for specific coordinates
#'   coords <- cbind(runif(n=100, 47, 48), runif(n=100, 34, 35))
#'   lc_points <- get_landcover(coords, year=2024)
#'   print(lc_points)
#' }
#'
#' @author Abdollah Jalilian
#'
#' @export
get_landcover <- function(where,
                          year=2024,
                          downloaddir=NULL)
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

  # validate year
  if (year < 2001 || year > 2024)
    stop("Year must be between 2001 and 2024.")

  # set the default download directory
  if (is.null(downloaddir))
  {
    downloaddir <- file.path(Sys.getenv("HOME"), "Downloads")
    if (Sys.info()["sysname"] == "Windows")
      downloaddir <- gsub("\\\\", "/",
                          file.path(Sys.getenv("USERPROFILE"), "Downloads"))
  }
  if (!dir.exists(downloaddir))
    dir.create(downloaddir, recursive=TRUE)

  # query NASA CMR API for specific HDF links
  message("Querying NASA CMR API for year ", year, "...")
  q <- list(collection_concept_id="C2484079608-LPCLOUD",
            temporal=paste0(year, "-01-01T00:00:00Z,", year, "-12-31T23:59:59Z"),
            bounding_box=paste(bbox, collapse=","),
            page_size=2000)
  res <- httr::GET("https://cmr.earthdata.nasa.gov/search/granules.json", query=q)
  httr::stop_for_status(res)
  entries <- httr::content(res, "parsed")$feed$entry
  if (length(entries) == 0)
    stop("No MODIS tiles found for this year/region.")

  hdf_links <- sapply(entries, function(x) {
    data_links <- Filter(function(l) l$rel == "http://esipfed.org/ns/fedsearch/1.1/data#", x$links)
    hrefs <- sapply(data_links, function(l) l$href)
    h <- grep("\\.hdf$", hrefs, value=TRUE)
    if (length(h) > 0)
      return(h[1])
    else
      return(NA_character_)
  })
  hdf_links <- as.character(na.omit(unique(hdf_links)))

  # check for missing files and trigger Browser authentication
  tile_id <- function(x) { sub(".*(h[0-9]{2}v[0-9]{2}).*", "\\1", x) }
  expected_tiles <- unique(tile_id(hdf_links))
  existing_files <- list.files(downloaddir,
                               pattern=paste0("A", year, ".*\\.hdf$"),
                               full.names=TRUE)
  existing_tiles <- unique(tile_id(existing_files))
  missing_links <- hdf_links[tile_id(hdf_links) %in%
                               setdiff(expected_tiles, existing_tiles)]
  if (length(missing_links) > 0)
  {
    sapply(missing_links, utils::browseURL)
    message("Found ", length(missing_links), " missing tiles.\n",
            "1) Log in to NASA Earthdata in the browser.\n",
            "2) Download the tiles to: ", downloaddir)
    cat("Press [Enter] once all downloads are complete...")
    readline()
  }
  # re-check after download
  existing_files <- list.files(downloaddir,
                               pattern=paste0("A", year, ".*\\.hdf$"),
                               full.names=TRUE)
  existing_tiles <- unique(tile_id(existing_files))
  if (length(setdiff(expected_tiles, existing_tiles)) > 0)
    stop("Some required tiles are still missing in: ", downloaddir)

  tile_files <- existing_files[tile_id(existing_files) %in% expected_tiles]
  tile_files <- tile_files[!duplicated(tile_id(tile_files))]
  if (length(tile_files) == 0)
    stop("No HDF files found in: ", downloaddir)

  # load and process raster data
  message("Mosaicing and reprojecting...")
  rast_list <- lapply(tile_files, function(f) {
    r <- terra::rast(f)
    lname <- grep("LC_Type1", names(r), value=TRUE)
    if (length(lname) == 0)
      stop("LC_Type1 layer not found in: ", f)
    r[[lname]]
  })

  if (length(rast_list) == 0)
    stop("No raster tiles could be loaded.")
  # mosaic tiles
  if (length(rast_list) == 1)
  {
    rdata <- rast_list[[1]]
  } else {
    rdata <- do.call(terra::mosaic, c(rast_list, list(fun="first")))
  }
  # project to  WGS84 and then crop to the area
  rdata <- terra::project(rdata, "EPSG:4326", method="near")
  rdata <- terra::crop(rdata, terra::as.polygons(
    terra::ext(bbox[1], bbox[3], bbox[2], bbox[4]), crs="EPSG:4326"))

  # if input was points, extract and return
  if (inherits(where, c("matrix", "data.frame")))
  {
    where <- data.frame(where)
    extracted <- terra::extract(rdata, where, ID=FALSE)
    rdata <- cbind(where, extracted)
  }

  return(rdata)
}
