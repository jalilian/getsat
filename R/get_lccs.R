#'' Retrieve 300m Annual ESA CCI Land Cover (1992–2020)
#'
#' Downloads and subsets global land cover maps from the ESA Climate Change
#' Initiative (CCI) Land Cover dataset at 300m resolution for the years 1992–2020.
#' The dataset is based on the UN FAO Land Cover Classification System (LCCS).
#'
#' @param where Either:
#'   * A numeric vector of length 4 defining a bounding box as
#'     \code{c(xmin, ymin, xmax, ymax)}, or
#'   * A two-column matrix/data.frame of coordinates (\code{longitude, latitude}).
#' @param year Integer. Year of interest (1992–2020). Default: 2020.
#' @param simplify Logical. If \code{FALSE} (default), returns fractional cover
#'   for each land cover class (multi-layer \code{SpatRaster}).
#'   If \code{TRUE}, returns a single categorical raster with the dominant
#'   land cover class in each pixel.
#' @param fact Numeric. Aggregation factor. If \code{>0}, the raster is aggregated
#'   by this factor using \code{\link[terra]{aggregate}} with either the
#'   modal (simplified case) or mean (fractional case) function.
#' @param maxattempts Integer. Maximum number of connection attempts. Default: 5.
#' @param delay Numeric. Initial delay (in seconds) between retries. Doubled at
#'   each attempt (exponential backoff). Default: 2.
#'
#' @return If \code{where} is a bounding box, returns a \code{SpatRaster}.
#' If \code{where} is a set of points, returns a \code{data.frame} with land
#' cover values at those points.
#'
#' @details
#' This function connects to the Centre for Environmental Data Analysis (CEDA)
#' servers to access ESA CCI Land Cover Plant Functional Types (PFT) data,
#' crops to the specified extent, and optionally aggregates or simplifies
#' to dominant land cover.
#'
#' Land cover classes include:
#' \enumerate{
#'   \item WATER
#'   \item BARE
#'   \item BUILT
#'   \item GRASS-MAN
#'   \item GRASS-NAT
#'   \item SHRUBS-BD
#'   \item SHRUBS-BE
#'   \item SHRUBS-ND
#'   \item SHRUBS-NE
#'   \item WATER_INLAND
#'   \item SNOWICE
#'   \item TREES-BD
#'   \item TREES-BE
#'   \item TREES-ND
#'   \item TREES-NE
#'   \item LAND
#'   \item WATER_OCEAN
#' }
#'
#' @examples
#' \dontrun{
#' # Bounding box example
#' lccs_data1 <- get_lccs(c(47, 34, 47.5, 35), year = 2020)
#' terra::plot(lccs_data1)
#'
#' lccs_data2 <- get_lccs(c(47, 34, 47.5, 35), year = 2020, simplify = TRUE)
#' terra::plot(lccs_data2)
#'
#' # Points example with aggregation
#' coords <- data.frame(lon = runif(10, 47, 47.5),
#'                      lat = runif(10, 34, 35))
#' lccs_points <- get_lccs(coords, year = 2020, fact = 2, simplify = TRUE)
#' }
#'
#' @seealso \link[terra]{rast}, \link[terra]{aggregate}, \link[terra]{extract}
#'
#' @references
#' Harper, K.L. et al. (2023). ESA Land Cover Climate Change Initiative (Land_Cover_cci):
#' Global Plant Functional Types (PFT) Dataset, v2.0.8. NERC EDS Centre for
#' Environmental Data Analysis. \doi{10.5285/26a0f46c95ee4c29b5c650b129aab788}
#'
#' @author Abdollah Jalilian
#'
#' @export
get_lccs <- function(where, year=2020,
                     simplify=FALSE, fact=0,
                     maxattempts=5,
                     delay=2)
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
  if (!(year %in% 1992:2020))
    stop("'year' must be a numeric value between 1992 to 2020")

  message("Connecting to the CEDA server to download land cover CCI data...\n")
  url <- paste0("https://dap.ceda.ac.uk/thredds/dodsC/neodc/esacci/land_cover/",
                "data/pft/v2.0.8/ESACCI-LC-L4-PFT-Map-300m-P1Y-",
                year, "-v2.0.8.nc")
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

  # subset indices for requested area
  lon_idx <- which(lon >= bbox[1] & lon <= bbox[3])
  lat_idx <- which(lat >= bbox[2] & lat <= bbox[4])

  vars <- c("WATER", "BARE", "BUILT", "GRASS-MAN", "GRASS-NAT",
            "SHRUBS-BD", "SHRUBS-BE", "SHRUBS-ND", "SHRUBS-NE",
            "WATER_INLAND", "SNOWICE", "TREES-BD", "TREES-BE",
            "TREES-ND", "TREES-NE", "LAND", "WATER_OCEAN")

  layers <- vector("list", length(vars))
  for (i in 1:length(vars))
  {
    slice <- ncdf4::ncvar_get(nc, vars[i],
                              start = c(min(lon_idx), min(lat_idx), 1),
                              count = c(length(lon_idx), length(lat_idx), 1))

    # transpose
    r <- terra::rast(t(slice))
    terra::ext(r) <- c(min(lon[lon_idx]), max(lon[lon_idx]),
                       min(lat[lat_idx]), max(lat[lat_idx]))
    terra::crs(r) <- "EPSG:4326"

    layers[[i]] <- r
  }

  ncdf4::nc_close(nc)

  # combine layers into a single SpatRaster
  rdata <- terra::rast(layers)
  names(rdata) <- vars

  if (simplify)
  {
    dominant <- terra::app(rdata, which.max)
    levels(dominant) <- data.frame(value=1:terra::nlyr(rdata),
                                   class=names(rdata))
    rdata <- dominant
  }

  # aggregation if factor 'fact' is greater than 0
  if (fact > 0)
  {
    rdata <- terra::aggregate(rdata, fact=fact,
                              fun=ifelse(simplify, "modal", "mean"), na.rm=TRUE)
  }

  # if 'where' is a matrix/data frame, extract var values for points
  if (inherits(where, c("matrix", "data.frame")))
  {
    where <- data.frame(where)
    rdata <- terra::extract(rdata, where, ID=FALSE)
    rdata <- cbind(where, rdata)
  }

  return(rdata)
}
