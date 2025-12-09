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
#'
#' @param res Numeric. Resolution of the DEM data in meters.
#'   Must be either \code{30} or \code{90}.
#'
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
#'   dem_raster <- get_dem2(c(6, 35, 19, 47), res=0.5)
#'   plot(dem_raster)
#'
#'   # Retrieve DEM for specific coordinates
#'   coords <- cbind(runif(n=100, 6, 19), runif(n=100, 35, 47))
#'   dem_points <- get_dem2(coords, res=0.5)
#'   print(dem_points)
#' }
#'
#' @seealso \link[terra]{rast}
#'
#' @author Abdollah Jalilian
#'
#' @export
get_dem2 <- function(where,
                    res=0.0625,
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
      min(where[, 1]) - 0.1, min(where[, 2]) - 0.1,
      max(where[, 1]) + 0.1, max(where[, 2]) + 0.1
    )
  } else {
    stop("'where' must be a numeric vector of length 4 (bounding box) or a matrix/data.frame with two columns (longitude, latitude).")
  }

  # validate bounding box format
  if (bbox[1] >= bbox[3] || bbox[2] >= bbox[4])
    stop("Bounding box must be in the format c(xmin, ymin, xmax, ymax) with valid coordinates.")

  # valid geographical boundaries
  if (any(bbox[1] <= -180 | bbox[3] >= 180 |
          bbox[2] <= -90 | bbox[4] >= 90))
    stop("The specified area is outside the valid data coverage region.")

  # validate resolution
  res_idx <- which(res == c(0.0625, 0.125, 0.250, 0.500, 0.750, 1.000))
  if (length(res_idx) != 1)
    stop("'res' must be either 0.0625, 0.125, 0.250, 0.500, 0.750 or 1.000 degrees.")

  url <- c(
    "https://d1qb6yzwaaq4he.cloudfront.net/data/gmted2010/GMTED2010_15n015_00625deg.nc",
    "https://d1qb6yzwaaq4he.cloudfront.net/data/gmted2010/GMTED2010_15n030_0125deg.nc",
    "https://d1qb6yzwaaq4he.cloudfront.net/data/gmted2010/GMTED2010_15n060_0250deg.nc",
    "https://d1qb6yzwaaq4he.cloudfront.net/data/gmted2010/GMTED2010_15n120_0500deg.nc",
    "https://d1qb6yzwaaq4he.cloudfront.net/data/gmted2010/GMTED2010_15n180_0750deg.nc",
    "https://d1qb6yzwaaq4he.cloudfront.net/data/gmted2010/GMTED2010_15n240_1000deg.nc")

  # ensure directory ends with '/'
  output_dir <- gsub("\\\\", "/", output_dir)
  if (!grepl("/$", output_dir))
    output_dir <- paste0(output_dir, "/")
  destfile <- paste0(output_dir, "GMTED2010_",
                     gsub("\\.", "_", sprintf("%.4f", res)), "_deg.nc")
  if (!file.exists(destfile))
  {
    # attempt to connect to the API
    attempt <- 1
    repeat {
      # check API connectivity
      status <- httr::GET(url[res_idx])
      status <- httr::status_code(status)

      if (status == 200)
      {
        download.file(url[res_idx], destfile=destfile, method="libcurl", mode="wb")
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
  }
  rdata <- terra::rast(destfile)
  rdata <- rdata[["elevation"]]
  w <- terra::ext(bbox, xy=TRUE)
  w <- terra::intersect(terra::ext(rdata), w)
  rdata <- terra::crop(rdata, w)

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
