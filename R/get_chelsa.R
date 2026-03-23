#' Retrieve Monthly CHELSA Data
#'
#' Downloads and returns CHELSA (Climatologies at high resolution for the
#' earth’s land surface areas) data, a high-resolution global dataset
#' of climate variables. It features topographically informed downscaling
#' that accounts for effects such as windward/leeward exposure and
#' atmospheric boundary layer dynamics. It has a high spatial resolution
#' of ~1 km (30 arc-sec) and is widely used for species distribution
#' modeling and ecological research. Data are accessed via the Swiss
#' Federal Institute for Forest, Snow and Landscape Research (WSL)
#' and the EnviDat platform.
#'
#' @param where Numeric vector of length 4 specifying a bounding box
#'        in the form `c(xmin, ymin, xmax, ymax)`
#'        or a matrix/data.frame with two columns (longitude, latitude)
#'        representing points of interest.
#'
#' @param var Character string specifying the CHELSA variable to retrieve.
#'        Supported values include:
#'        \describe{
#'          \item{\code{"clt"}}{Total Cloud Cover (monthly average; \%), available for both daily and monthly datasets}
#'          \item{\code{"hurs"}}{Near-Surface Relative Humidity (monthly average; \%), both daily and monthly}
#'          \item{\code{"pr"}}{Precipitation (monthly total; mm), both daily and monthly}
#'          \item{\code{"prec"}}{Precipitation (alternative version; mm), both daily and monthly}
#'          \item{\code{"ps"}}{Surface Air Pressure (monthly average; Pa), both daily and monthly}
#'          \item{\code{"rsds"}}{Surface Downwelling Shortwave Radiation (monthly mean; W m^-2), both daily and monthly}
#'          \item{\code{"sfcWind"}}{Near-Surface Wind Speed (monthly average; m s^-1), both daily and monthly}
#'          \item{\code{"tas"}}{Mean Near-Surface Air Temperature (monthly average; K), both daily and monthly}
#'          \item{\code{"tasmax"}}{Maximum Near-Surface Air Temperature (monthly average; K), both daily and monthly}
#'          \item{\code{"tasmin"}}{Minimum Near-Surface Air Temperature (monthly average; K), both daily and monthly}
#'          \item{\code{"tz"}}{Temperature Lapse Rate (K m^-1), both daily and monthly}
#'          \item{\code{"we"}}{Wind Energy / Wind Power Density (daily specific; W m^-2), daily-only (year >= 2022)}
#'        }
#'
#' @param year Integer (1979-2025).
#'
#' @param month Integer (1-12).
#'
#' @return  If `where` is a bounding box (vector of length four), a `SpatRaster`
#'         object (from the `terra` package) containing the requested variable
#'         over the specified area. The raster has time information stored. If
#'         `where` is a matrix or data frame, a `data.frame` containing
#'         coordinates and corresponding values.
#'
#' @examples
#' \dontrun{
#' # Bounding box
#' bbox <- c(44, 25, 63, 40)
#' tas_raster <- get_chelsa(bbox, var="tas", year=2020, month=6)
#'
#' # Using coordinates matrix
#' coords <- cbind(runif(n=10, 44, 63), runif(n=10, 25, 40))
#' tas_mat <- get_chelsa(coords, var="tas", year=2020, month=6)
#' }
#'
#' @seealso \link[terra]{rast}
#'
#' @references
#' - Karger, D.N., Conrad, O., Bohner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W.,
#'   Zimmermann, N.E., Linder, H.P., Kessler, M., 2017.
#'   Climatologies at high resolution for the earth’s land surface areas.
#'   Scientific Data, 4, 170122.
#'   Available at: \url{https://chelsa-climate.org/}
#' - Karger, D.N., Conrad, O., Bohner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W.,
#'   Zimmermann, N.E., Linder, P., Kessler, M., 2021.
#'   CHELSA v2.1: Climatologies at high resolution for the earth’s land surface areas.
#'   EnviDat. \doi{10.16904/envidat.228}
#'
#' @author Abdollah Jalilian
#'
#' @export
get_chelsa <- function(where, var="clt",  year=2023, month=1)
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
  if (bbox[1] >= bbox[3] || bbox[2] >= bbox[4])
    stop("Invalid bounding box: xmin < xmax and ymin < ymax must hold.")
  ext_crop <- terra::ext(c(bbox[1], bbox[3], bbox[2], bbox[4]))

  year <- as.integer(year)
  if (is.na(year) || year < 1979 || year > 2025)
    stop("Invalid year: CHELSA data is only available from 1979 onwards.")
  month <- as.integer(month)
  if (any(is.na(month)) || any(month < 1) || any(month > 12))
    stop("Invalid month: Please provide an integer between 1 and 12.")

  month <- sprintf("%02d", month)
  rdata <- list()
  for (m in month)
  {
    # monthly data
    url_month <- sprintf(
      "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/monthly/%s/%d/CHELSA_%s_%s_%d_V.2.1.tif",
      var, year, var, m, year)
    message("Attempting monthly file: ", year, "-", m)
    rout <- tryCatch({
      terra::crop(terra::rast(paste0("/vsicurl/", url_month)), ext_crop)
    }, error = function(e) NULL)
    if (is.null(rout))
    {
      message("Monthly file not found. Aggregating daily files for ", year, "-", m)
      # determine number of days in the requested month
      days_in_month <- lubridate::days_in_month(as.Date(paste0(year, "-", m, "-01")))
      daily_layers <- list()
      for (d in 1:days_in_month)
      {
        d_str <- sprintf("%02d", d)
        url_day <- sprintf(
          "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/daily/%s/%d/CHELSA_%s_%s_%s_%d_V.2.1.tif",
          var, year, var, d_str, m, year)

        r_day <- tryCatch({
          terra::crop(terra::rast(paste0("/vsicurl/", url_day)), ext_crop)
        }, error = function(e) {
          warning("Day ", d, " missing. Skipping.")
          return(NULL)
        })

        if (!is.null(r_day)) daily_layers[[d]] <- r_day
      }

      if (length(daily_layers) == 0)
        stop("No daily files found for month ", m, " in year ", year)
      # stack all daily layers and calculate the Mean (aggregate to monthly)
      monthly_stack <- terra::rast(daily_layers)
      fun <- if (var %in% c("pr", "prec")) "sum" else "mean"
      rout <- terra::app(monthly_stack, fun=fun, na.rm=TRUE)
    }
    rdata[[m]] <- rout
  }
  rdata <- if (length(rdata) > 1) terra::rast(rdata) else rdata[[1]]
  names(rdata) <- sprintf("%s_%d_%s", var, year, month)
  terra::time(rdata) <- as.Date(sprintf("%d-%02d-01", year, as.integer(month)))

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
