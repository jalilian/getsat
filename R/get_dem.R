#' Retrieve Copernicus Digital Elevation Model (DEM) Data
#'
#' This function downloads and mosaics Copernicus DEM data from the
#' Microsoft Planetary Computer. Users can specify a bounding box
#' or a set of coordinates to retrieve elevation values.
#'
#' @param where Either:
#'   - A numeric vector of length 4 specifying the bounding box
#'     in longitude/latitude format: \code{c(xmin, ymin, xmax, ymax)}.
#'   - A matrix or data frame with two columns representing
#'     longitude (first column) and latitude (second column) of points.
#'     All coordinates must be in the WGS84 coordinate reference system.
#' @param res Numeric. Resolution of the DEM data in meters.
#'   Must be either \code{30} or \code{90}.
#' @param output_dir Character. Directory to store the downloaded DEM tiles.
#'   Defaults to a temporary directory.
#'
#' @return If `where` is a bounding box (numeric vector of length four),
#'   returns a `SpatRaster` object representing the merged DEM data.
#'   If `where` is a matrix or data frame, returns a `data.frame` with
#'   coordinates and their corresponding DEM values.
#'
#' @details
#' **Important Note:**
#' Any use of data provided by the Copernicus program must include proper
#' citation and acknowledgement of the data sources. Users are required to
#' follow the license and terms of use specified by Copernicus and European
#' Space Agency. Failure to do so may violate the data usage policies.
#'
#' @references
#' - Copernicus DEM: European Space Agency (ESA) and the European Commission.
#'    + Copernicus Digital Elevation Model (Copernicus DEM), distributed by the European Space Agency under the Copernicus Programme.
#'    + Available at: https://spacedata.copernicus.eu/ and https://doi.org/10.5069/G9028PQB
#'
#' - Microsoft Planetary Computer: Microsoft AI for Earth.
#'    + Microsoft Planetary Computer.
#'    + Available at: https://planetarycomputer.microsoft.com/
#'
#' @examples
#' \dontrun{
#'   # Retrieve DEM for a bounding box
#'   dem_raster <- get_dem(c(-3, 5, -2, 6), res = 90)
#'   plot(dem_raster)
#'
#'   # Retrieve DEM for specific coordinates
#'   coords <- cbind(runif(n = 100, -3, -2), runif(n = 100, 5, 6))
#'   dem_points <- get_dem(coords, res = 90)
#'   print(dem_points)
#' }
#'
#' @seealso \link[terra]{rast}
#'
#' @author Abdollah Jalilian
#'
#' @export
get_dem <- function(where,
                    res=30,
                    output_dir=tempdir())
{
  message("For citation and terms of use, see\n<https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM>")

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

  # valid geographical boundaries
  if (any(bbox[1] <= -180 | bbox[3] >= 180 |
          bbox[2] <= 90 | bbox[4] >= 90))
    stop("The specified area is outside the valid data coverage region.")

  # validate resolution
  if (!res %in% c(30, 90))
    stop("'res' must be either 30 or 90 meters.")

  # attempt to connect to the API
  attempt <- 1
  repeat {
    # check API connectivity
    status <- httr::GET("https://planetarycomputer.microsoft.com/api/stac/v1")
    status <- httr::status_code(status)

    if (status == 200)
    {
      # retrieve DEM tiles from Microsoft Planetary Computer
      items <- rstac::stac(
        "https://planetarycomputer.microsoft.com/api/stac/v1"
      ) |>
        # STAC search API
        rstac::stac_search(
          # collection IDs to include in the search for items
          collections = paste0("cop-dem-glo-", res),
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
        rstac::items_fetch() |>
        # download items
        rstac::assets_download(asset_names="data",
                               overwrite=TRUE,
                               output_dir=output_dir)

      break
    } else if (attempt >= 5) {
      stop("Failed to connect to the API after ", 5,
           " attempts. Last status code: ", status)
    } else {
      message("Attempt ", attempt, " failed with status code ", status,
              ". Retrying in ", 2, " seconds...")
      Sys.sleep(2)
      attempt <- attempt + 1
    }
  }

  # load and crop DEM tiles
  rdata <- lapply(items$features,
                  function(o)
                  {
                    r <- terra::rast(o$assets$data$href)
                    w <- terra::ext(bbox, xy=TRUE)
                    w <- terra::intersect(terra::ext(r), w)
                    terra::crop(r, w)
                  })

  if (length(rdata) == 1)
  {
    rdata <- rdata[[1]]
  } else{
    # merge raster tiles into a single raster,  if more than one tile
    rdata <- do.call(terra::mosaic, c(rdata, list(fun="mean")))
  }

  # projection to EPSG:4326 (World Geodetic System 1984, WGS84)
  rdata <- terra::project(rdata, "EPSG:4326")

  # if 'where' is a matrix/data frame, extract elevation values for points
  if (inherits(where, c("matrix", "data.frame")))
  {
    where <- data.frame(where)
    rdata <- terra::extract(rdata, where, ID=FALSE)
    names(rdata) <- "elevation"
    rdata <- data.frame(where, rdata)
  }

  return(rdata)
}
