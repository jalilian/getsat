#' Retrieve TerraClimate Data
#'
#' Downloads and returns TerraClimate data, a high-resolution global dataset
#' of monthly climate and water balance variables from 1958 to the present.
#' The dataset is widely used for ecological, hydrological, and climate studies.
#' It has a spatial resolution of ~4 km (1/24°) and monthly temporal resolution.
#' Data are accessed via the Northwest Knowledge Network (NKN) and are publicly
#' available under the Creative Commons Public Domain Dedication (CC0).
#'
#' @param where Numeric vector of length 4 specifying a bounding box
#'        in the form `c(xmin, ymin, xmax, ymax)`
#'        or a matrix/data.frame with two columns (longitude, latitude)
#'        representing points of interest.
#' @param var Character string specifying the TerraClimate variable to retrieve.
#'        Supported values include:
#'        \describe{
#'     \item{\code{"aet"}}{Actual Evapotranspiration (monthly total)}
#'     \item{\code{"def"}}{Climate Water Deficit (monthly total)}
#'     \item{\code{"pet"}}{Potential Evapotranspiration (monthly total; reference evapotranspiration)}
#'     \item{\code{"ppt"}}{Precipitation (monthly total)}
#'     \item{\code{"q"}}{Runoff (monthly total)}
#'     \item{\code{"soil"}}{Soil Moisture (total column, end of month)}
#'     \item{\code{"srad"}}{Downward Surface Shortwave Radiation (monthly mean)}
#'     \item{\code{"swe"}}{Snow Water Equivalent (end of month)}
#'     \item{\code{"tmax"}}{Maximum Temperature (monthly average)}
#'     \item{\code{"tmin"}}{Minimum Temperature (monthly average)}
#'     \item{\code{"vap"}}{Vapor Pressure (monthly average)}
#'     \item{\code{"vpd"}}{Vapor Pressure Deficit (monthly average)}
#'     \item{\code{"ws"}}{Wind Speed (monthly average)}
#'     \item{\code{"PDSI"}}{Palmer Drought Severity Index (end of month)}
#'   }
#' @param maxattempts Integer, default `5`. Maximum number of attempts to connect
#'        to the server before failing.
#' @param delay Numeric, default `2`. Initial delay in seconds between retries,
#'        which increases exponentially.
#'
#' @return  If `where` is a bounding box (vector of length four), a `SpatRaster`
#'         object (from the `terra` package) containing the requested variable
#'         over the specified area. The raster has time information stored. If
#'         `where` is a matrix or data frame, a `data.frame` containing
#'         coordinates and corresponding values.
#'
#' @details TerraClimate is a high-resolution global dataset of monthly climate
#'          and water balance variables covering terrestrial surfaces from 1958
#'          to the present. It is maintained by the Climatology Lab at the
#'          University of California, Merced, led by Professor John T. Abatzoglou,
#'          and is designed to support research and practical applications
#'          in environmental management, agriculture, and water resources.
#'          Data access is provided through the Northwest Knowledge Network
#'          (University of Idaho) via their THREDDS Data Server:
#'          \url{https://climate.northwestknowledge.net/TERRACLIMATE/}.
#'
#' @examples
#' \dontrun{
#' # Bounding box
#' bbox <- c(44, 25, 63, 40)
#' pet_raster <- get_terraclimate(bbox, var = "pet")
#'
#' # Using coordinates matrix
#' coords <- cbind(runif(n=10, 44, 63), runif(n=10, 25, 40))
#' pet_raster <- get_terraclimate(coords)
#' }
#'
#' @seealso \link[terra]{rast}, \link[ncdf4]{nc_open}
#' @references
#' - Abatzoglou, J.T., Dobrowski, S.Z., Parks, S.A., Hegewisch, K.C., 2018.
#'   TerraClimate, a high-resolution global dataset of monthly climate and
#'   climatic water balance from 1958-2015. *Scientific Data*.
#'   Available at: \url{https://www.climatologylab.org/terraclimate.html}
#'
#' @author Abdollah Jalilian
#'
#' @export
get_terraclimate <- function(where,
                             var = "pet",
                             maxattempts = 5,
                             delay = 2)
{
  # validate input: bounding box or coordinates matrix/data.frame
  if (is.numeric(where) && length(where) == 4)
  {
    bbox <- where
  } else if (inherits(where, c("matrix", "data.frame")) && ncol(where) == 2) {
    bbox <- c(
      min(where[, 1]) - 0.2, min(where[, 2]) - 0.2,
      max(where[, 1]) + 0.2, max(where[, 2]) + 0.2
    )
  } else {
    stop("'where' must be a numeric vector of length 4 or a 2-column matrix/data.frame.")
  }

  # validate bounding box coordinates
  if (bbox[1] >= bbox[3] || bbox[2] >= bbox[4])
    stop("Bounding box must be in the form c(xmin, ymin, xmax, ymax).")

  if (any(bbox[1] < -180 | bbox[3] > 180 | bbox[2] < -90 | bbox[4] > 90))
    stop("Bounding box coordinates are outside valid ranges.")

  if (!is.character(var) || length(var) != 1)
    stop("'var' must be a single character string.")

  message("Connecting to the server to download TerraClimate data...\n")
  url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                "agg_terraclimate_", var, "_1958_CurrentYear_GLOBE.nc")

  # retry mechanism for remote access
  nc <- NULL
  for (attempt in 1:maxattempts)
  {
    nc <- tryCatch(ncdf4::nc_open(url), error = function(e) {
      message(sprintf("Attempt %d failed: %s", attempt, e$message))
      return(NULL)
    })
    if (!is.null(nc)) break
    Sys.sleep(delay * 2^(attempt - 1))  # exponential backoff
  }
  if (is.null(nc))
    stop("Failed to connect to NetCDF after retries.")

  # read longitude, latitude, and time
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  time <- ncdf4::ncvar_get(nc, "time")
  dates <- as.Date(time, origin = "1900-01-01")

  # subset indices for requested area
  lon_idx <- which(lon >= bbox[1] & lon <= bbox[3])
  lat_idx <- which(lat >= bbox[2] & lat <= bbox[4])

  # read each time slice and create SpatRaster
  n_time <- length(time)
  layers <- vector("list", n_time)
  for (i in 1:n_time)
  {
    slice <- ncdf4::ncvar_get(nc, var,
                       start = c(min(lon_idx), min(lat_idx), i),
                       count = c(length(lon_idx), length(lat_idx), 1))

    # transpose
    r <- terra::rast(t(slice))
    terra::ext(r) <- c(min(lon[lon_idx]), max(lon[lon_idx]),
                min(lat[lat_idx]), max(lat[lat_idx]))
    terra::crs(r) <- "EPSG:4326"

    layers[[i]] <- r
  }

  ncdf4::nc_close(nc)

  # combine layers into a single SpatRaster and set time
  rdata <- terra::rast(layers)
  terra::time(rdata) <- dates

  # if 'where' is a matrix/data frame, extract var values for points
  if (inherits(where, c("matrix", "data.frame")))
  {
    where <- data.frame(where)
    rdata <- terra::extract(rdata, where, ID=FALSE)
    names(rdata) <- paste(var, names(rdata), sep="_")
    rdata <- cbind(where, rdata)
  }

  return(rdata)
}
