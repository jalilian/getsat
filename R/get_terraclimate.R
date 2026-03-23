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
#'     \item{\code{"aet"}}{Actual Evapotranspiration (monthly total; mm)}
#'     \item{\code{"def"}}{Climate Water Deficit (monthly total; mm)}
#'     \item{\code{"pet"}}{Potential Evapotranspiration (monthly total; mm)}
#'     \item{\code{"ppt"}}{Precipitation (monthly total; mm)}
#'     \item{\code{"q"}}{Runoff (monthly total; mm)}
#'     \item{\code{"soil"}}{Soil Moisture (total column, end of month; mm)}
#'     \item{\code{"srad"}}{Downward Surface Shortwave Radiation (monthly mean; W m^-2)}
#'     \item{\code{"swe"}}{Snow Water Equivalent (end of month; mm)}
#'     \item{\code{"tmax"}}{Maximum Temperature (monthly average; °C)}
#'     \item{\code{"tmin"}}{Minimum Temperature (monthly average; °C)}
#'     \item{\code{"vap"}}{Vapor Pressure (monthly average; kPa)}
#'     \item{\code{"vpd"}}{Vapor Pressure Deficit (monthly average; kPa)}
#'     \item{\code{"ws"}}{Wind Speed (monthly average; m s^-1)}
#'     \item{\code{"PDSI"}}{Palmer Drought Severity Index (end of month; unitless)}
#'   }
#'
#' @param start_date Optional start date for subsetting the time series.
#'        Must be coercible to `Date` (e.g., `"YYYY-MM-DD"`).
#'        If `NULL` (default), data are returned from the earliest available date (1958-01-01).
#'
#' @param end_date Optional end date for subsetting the time series.
#'        Must be coercible to `Date` (e.g., `"YYYY-MM-DD"`).
#'        If `NULL` (default), data are returned up to the most recent available date.
#'
#' @param maxattempts Integer, default `5`. Maximum number of attempts to connect
#'        to the server before failing.
#'
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
#' pet_raster <- get_terraclimate(bbox, var = "pet", start_date="2010-01-01", end_date="2024-12-31")
#'
#' # Using coordinates matrix
#' coords <- cbind(runif(n=10, 44, 63), runif(n=10, 25, 40))
#' pet_mat <- get_terraclimate(coords)
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
                             start_date = NULL,
                             end_date = NULL,
                             maxattempts = 5,
                             delay = 2)
{
  # validate input: bounding box or coordinates matrix/data.frame
  if (is.numeric(where) && length(where) == 4)
  {
    bbox <- where
  } else if (inherits(where, c("matrix", "data.frame")) && ncol(where) == 2) {
    bbox <- c(min(where[, 1]) - 0.2, min(where[, 2]) - 0.2,
              max(where[, 1]) + 0.2, max(where[, 2]) + 0.2)
  } else {
    stop("'where' must be a numeric vector of length 4 or a 2-column matrix/data.frame.")
  }

  # validate bounding box coordinates
  if (bbox[1] >= bbox[3] || bbox[2] >= bbox[4])
    stop("Bounding box must be in the form c(xmin, ymin, xmax, ymax).")

  if (any(bbox[1] < -180 | bbox[3] > 180 | bbox[2] < -90 | bbox[4] > 90))
    stop("Bounding box coordinates are outside valid ranges.")

  # Validate 'var'
  if (!is.character(var) || length(var) != 1)
    stop("'var' must be a single character string.")

  allowed_vars <- c("aet", "def", "pet", "ppt", "q", "soil", "srad",
                    "swe", "tmax", "tmin", "vap", "vpd", "ws", "PDSI")
  if (!(var %in% allowed_vars))
    stop("Invalid 'var'. Must be one of: ", paste(allowed_vars, collapse=", "))


  message("Connecting to TerraClimate server...")
  url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                "agg_terraclimate_", var, "_1958_CurrentYear_GLOBE.nc")

  # retry mechanism for remote access
  nc <- NULL
  for (attempt in 1:maxattempts)
  {
    nc <- tryCatch(ncdf4::nc_open(url), error = function(e) NULL)
    if (!is.null(nc)) break
    message(sprintf("Attempt %d failed. Retrying in %.1f sec...",
                    attempt, delay * 2^(attempt - 1)))
    Sys.sleep(delay * 2^(attempt - 1))
  }
  if (is.null(nc))
    stop("Failed to connect to TerraClimate server.")

  # read longitude, latitude and time
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  time <- ncdf4::ncvar_get(nc, "time")
  origin <- sub("days since ", "", ncdf4::ncatt_get(nc, "time", "units")$value)
  dates <- as.Date(time, origin=origin)

  # subset indices for requested area
  lon_idx <- which(lon >= bbox[1] & lon <= bbox[3])
  lat_idx <- which(lat >= bbox[2] & lat <= bbox[4])
  if (length(lon_idx) == 0 || length(lat_idx) == 0)
    stop("Bounding box does not overlap dataset.")

  # subset time
  keep <- rep(TRUE, length(dates))

  if (!is.null(start_date))
    keep <- keep & (dates >= as.Date(start_date))
  if (!is.null(end_date))
    keep <- keep & (dates <= as.Date(end_date))
  time_idx <- which(keep)
  if (length(time_idx) == 0)
    stop("No data available for requested time range.")

  # read data in one call
  message("Downloading data (this may take a moment)...")
  arr <- ncdf4::ncvar_get(nc, var,
                          start=c(min(lon_idx), min(lat_idx), min(time_idx)),
                          count=c(length(lon_idx), length(lat_idx), length(time_idx)))
  ncdf4::nc_close(nc)
  # fix dimension order
  dim_names <- sapply(nc$var[[var]]$dim, function(x) x$name)
  # expect lon, lat, time
  if (!all(c("lon", "lat", "time") %in% dim_names))
    stop("Unexpected dimension order in NetCDF")
  # reorder dynamically
  arr <- aperm(arr,  match(c("lat", "lon", "time"), dim_names))

  # create SpatRaster
  r <- terra::rast(arr, extent=c(min(lon[lon_idx]), max(lon[lon_idx]),
                                 min(lat[lat_idx]), max(lat[lat_idx])),
                   crs="EPSG:4326")
  # assign time + names
  terra::time(r) <- dates[time_idx]
  names(r) <- paste0(var, "_", format(dates[time_idx], "%Y_%m"))

  # if 'where' is a matrix/data frame, extract var values for points
  if (inherits(where, c("matrix", "data.frame")))
  {
    where <- data.frame(where)
    vals <- terra::extract(r, where, ID=FALSE)
    names(vals) <- names(r)
    return(cbind(where, vals))
  }

  return(r)
}
