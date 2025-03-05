#' Retrieve 10m Annual Land Use Land Cover from 2017 to 2023
#'
#' This function retrieves global land use and land cover (LULC) maps derived from
#' ESA Sentinel-2 imagery at a 10m resolution for the years 2017-2023. The dataset
#' includes 9 LULC classes and is created using a deep learning model trained on
#' billions of human-labeled pixels curated by the National Geographic Society.
#'
#' @param where A numeric vector of length 4 representing the bounding box in the
#'        form c(xmin, ymin, xmax, ymax) or a matrix/data.frame with two columns
#'      (longitude, latitude) representing points of interest.
#' @param year A numeric value specifying the year of interest (default is 2023).
#'        Valid values are between 2017 and 2023.
#' @param labels A logical value indicating whether to return land use and land
#'        cover class names (default is FALSE). If TRUE, the function returns
#'        descriptive labels for each class.
#' @param fact A numeric factor for aggregation. If greater than 0, the output raster
#'        will be aggregated by the specified factor using the modal function.
#' @param output_dir A character string specifying the output directory for storing
#'        downloaded data (default is a temporary directory).
#' @param clean_dir A logical value indicating whether to clean up the output
#' directory after the function has completed (default is FALSE).
#'
#' @return A raster or data frame containing the land use and land cover data for
#'         the specified area and year. If `where` is a matrix or data frame, the
#'         function returns a data frame with coordinates and corresponding LULC values.
#'
#' @details
#' The function first validates the input, including the `where` parameter
#'  (bounding box or coordinates) and the `year` parameter. It then uses the Microsoft
#'  Planetary Computer's STAC API to search and download LULC data tiles, which are
#'  cropped to the specified bounding box. The function returns a raster object with
#'  merged LULC data or a data frame with LULC values for the specified coordinates.
#'
#' The LULC classes are represented by integer values:
#' - 0: No Data
#' - 1: Water
#' - 2: Trees
#' - 4: Flooded vegetation
#' - 5: Crops
#' - 7: Built area
#' - 8: Bare ground
#' - 9: Snow/ice
#' - 10: Clouds
#' - 11: Rangeland
#'
#' @examples
#' \dontrun{
#' # Retrieve LULC for a bounding box in 2023 and include class labels
#' lulc_data <- get_lulc(c(47, 34, 47.5, 35), year = 2023, labels = TRUE)
#'
#' # Retrieve LULC for a set of coordinates with aggregation
#' coordinates <- data.frame(lon = runif(50, 47, 47.5), lat=runif(50, 34, 35))
#' lulc_data <- get_lulc(coordinates, year = 2023, fact = 2)
#' }
#'
#' @references
#' Impact Observatory, Microsoft, and Esri. (2023). Global Land Use Land Cover (LULC) Dataset, 10m Resolution (2017-2023).
#' ESA Sentinel-2 Imagery. Available at: https://planetarycomputer.microsoft.com/
#'
#' @author Abdollah Jalilian
#'
#' @export
get_lulc <- function(where, year=2023,
                     labels=FALSE, fact=0,
                     output_dir=tempdir(),
                     clean_dir=FALSE)
{
  # validate input: bounding box or coordinate matrix/data frame
  if (is.numeric(where) && length(where) == 4)
  {
    bbox <- where
  } else if (inherits(where, c("matrix", "data.frame")) && ncol(where) == 2)
  {
    bbox <- c(
      min(where[, 1]) - 0.15, min(where[, 2]) - 0.15,
      max(where[, 1]) + 0.15, max(where[, 2]) + 0.15
    )
  } else {
    stop("'where' must be a numeric vector of length 4 (bounding box) or a matrix/data.frame with two columns (longitude, latitude).")
  }

  # validate bounding box format
  if (bbox[1] >= bbox[3] || bbox[2] >= bbox[4])
    stop("Bounding box must be in the format c(xmin, ymin, xmax, ymax) with valid coordinates.")

  # validate year
  if (!(year %in% 2017:2023))
    stop("'year' must be a numeric value between 2017 to 2023")

  # retrieve DEM tiles from Microsoft Planetary Computer
  items <- rstac::stac(
    "https://planetarycomputer.microsoft.com/api/stac/v1"
  ) |>
    # STAC search API
    rstac::stac_search(
      # collection IDs to include in the search for items
      collections = "io-lulc-annual-v02",
      # bounding box (xmin, ymin, xmax, ymax) in  WGS84 longitude/latitude
      bbox = bbox,
      # maximum number of results
      limit = 999
    ) |>
    # HTTP GET requests to STAC web services
    rstac::get_request() |>
    # allow access assets from Microsoft's Planetary Computer
    rstac::items_sign(sign_fn=rstac::sign_planetary_computer()) |>
    # fetch all STAC Items
    rstac::items_fetch()

  ids <- sapply(items$features,
                function(o) strsplit(o$id, "-")[[1]])

  ok <- as.numeric(ids[2, ]) == year
  items$features <- items$features[ok]
  # download items
  items <- items |>
    rstac::assets_download(asset_names="data",
                           overwrite=TRUE,
                           output_dir=output_dir)

  # load and crop tiles
  rdata <- lapply(items$features,
                  function(o)
                  {
                    r <- terra::rast(o$assets$data$href)
                    # project the extent to match the raster's CRS
                    w <- terra::project(terra::ext(bbox, xy=TRUE), "EPSG:4326", terra::crs(r))
                    w <- terra::intersect(terra::ext(r), w)
                    terra::crop(r, w)
                  })

  if (length(rdata) == 1)
  {
    rdata <- rdata[[1]]
  } else{
    # merge raster tiles into a single raster,  if more than one tile
    rdata <- do.call(terra::mosaic, c(rdata, list(fun="modal")))
  }

  # if 'labels' argument is TRUE, return class names
  if (labels)
  {
    # land cover classification levels
    levs <- c("Water", "Trees", NA, "Flooded vegetation",
              "Crops", NA, "Built area", "Bare ground",
              "Snow/ice", "Clouds", "Rangeland")
    v <- terra::values(rdata)
    # non-finite or integer values to NA
    v[(!is.finite(v))] <- NA
    v[v %% 1 != 0] <- NA
    terra::values(rdata) <- levs[v]
  }

  # aggregation if factor 'fact' is greater than 0
  if (fact > 0)
    rdata <- terra::aggregate(rdata, fact=fact, fun="modal", na.rm=TRUE)

  # projection to EPSG:4326 (World Geodetic System 1984, WGS84)
  rdata <- terra::project(rdata, "EPSG:4326")

  # if 'where' is a matrix/data frame, extract elevation values for points
  if (inherits(where, c("matrix", "data.frame")))
  {
    where <- data.frame(where)
    rdata <- terra::extract(rdata, where, ID=FALSE)
    names(rdata) <- "lulc"
    rdata <- data.frame(where, rdata)
  }

  return(rdata)
}
