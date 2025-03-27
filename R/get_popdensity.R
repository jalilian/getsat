#' Retrieve Gridded Population of the World, Version 4:
#'
#' Access and download the Population Density Adjusted to Match 2015 Revision
#' UN WPP Country Totals, Revision 11, provided by the Socioeconomic Data and
#' Applications Center (SEDAC) at NASA. The function returns population density
#' data adjusted to match the 2015 Revision of the United Nations World Population
#' Prospects (UN WPP) country totals. The dataset represents estimates of human
#' population density (people per square kilometer) for various years, using a
#' proportional allocation gridding algorithm that assigns UN WPP-adjusted
#' population counts to global grid cells.
#'
#' @param where A numeric vector of length 4 (bounding box) or a matrix/data.frame with two columns (longitude, latitude) specifying the spatial region of interest. If a bounding box is provided, the coordinates should be in the format: c(xmin, ymin, xmax, ymax).
#'
#' @param year An integer specifying the year for which the population density data is required. Valid values are: 2000, 2005, 2011, 2015, and 2020. The default is 2020.
#'
#' @param res A character string specifying the resolution of the population density data. Options include:
#' - "30_sec" for 30 arc-seconds (~1 km at the equator),
#' - "2pt5_min" for 2.5 arc-minutes,
#' - "15_min" for 15 arc-minutes,
#' - "30_min" for 30 arc-minutes,
#' - "1_deg" for 1 degree resolution.
#' The default is "30_sec".
#'
#' @param downloaddir A character string specifying the directory where the data file should be downloaded. If not specified, the system's default download directory is used (Windows: `file.path(Sys.getenv("USERPROFILE"), "Downloads")`, macOS/Linux: `file.path(Sys.getenv("HOME"), "Downloads")`).
#'
#' @details
#' The available years are 2000, 2005, 2011, 2015, and 2020.
#'
#' The data are provided as global rasters at the following resolutions:
#' - 30 arc-second (~1 km resolution at the equator),
#' - 2.5 arc-minute (~5 km resolution at the equator),
#' - 15 arc-minute (~30 km resolution at the equator),
#' - 30 arc-minute (~55 km resolution at the equator), and
#' - 1 degree (~110 km resolution at the equator),
#'
#' These population density estimates are based on national and sub-national
#' population counts and spatial distribution information, adjusted to match t
#' he UN WPP country totals. The rasters are available for download and can be
#' cropped to a specified bounding box or set of coordinates.
#'
#' For more information on the dataset and access to the data, visit:
#' \url{https://www.earthdata.nasa.gov/data/projects/gpw}
#'
#' @return If the input `where` is a bounding box, the function returns a raster
#' object cropped to that region. If `where` is a set of coordinates
#' (matrix/data.frame), the function returns the population density values at
#' those specific locations.
#'
#' @examples
#' \dontrun{
#'  # Get population density for a bounding box
#'  pd1 <- get_popdensity(c(-5, 4, 1, 10), year=2020, res="1_deg")
#'  terra::plot(pd1)
#'
#'  # Get population density for specific coordinates
#'  coords <- cbind(runif(n = 100, -5, -1), runif(n = 100, 4, 10))
#'  pd2 <- get_popdensity(coords, year=2020, res="1_deg")
#'  print(pd2)
#' }
#'
#' @references
#' - Center for International Earth Science Information Network-CIESIN-Columbia University. (2018).
#'   Gridded Population of the World, Version 4 (GPWv4): Population Density
#'   Adjusted to Match 2015 Revision UN WPP Country Totals, Revision 11
#'   (Version 4.11) Data set. Palisades, NY: NASA Socioeconomic Data and
#'   Applications Center (SEDAC). \url{https://doi.org/10.7927/H4F47M65}
#' - NASA Earth Data: \url{https://www.earthdata.nasa.gov/data/projects/gpw}
#'
#' @author Abdollah Jalilian
#'
#' @export
get_popdensity <- function(where,
                           year=2020,
                           res="30_sec",
                           downloaddir = NULL)
{
  # validate input: bounding box or coordinate matrix/data frame
  if (is.numeric(where) && length(where) == 4)
  {
    bbox <- where
  } else if (inherits(where, c("matrix", "data.frame")) && ncol(where) == 2)
  {
    bbox <- c(
      min(where[, 1]) - 0.2, min(where[, 2]) - 0.2,
      max(where[, 1]) + 0.2, max(where[, 2]) + 0.2
    )
  } else {
    stop("'where' must be a numeric vector of length 4 (bounding box) or a matrix/data.frame with two columns (longitude, latitude).")
  }

  # validate bounding box format
  if (bbox[1] >= bbox[3] || bbox[2] >= bbox[4])
    stop("Bounding box must be in the format c(xmin, ymin, xmax, ymax) with valid coordinates.")

  # valid year and resolution options
  valid_years <- seq(2000, 2020, by = 5)
  valid_ress <- c("30_sec", "2pt5_min", "15_min", "30_min", "1_deg")

  # validate year
  if (!(is.numeric(year) && length(year) == 1 && year %in% valid_years))
    stop(sprintf("'year' must be a numeric value and one of the following: %s",
                 paste(valid_years, collapse = ", ")))


  # validate res
  if (!(is.character(res) && length(res) == 1 && res %in% valid_ress)) {
    stop(sprintf("'res' must be a character string and one of the following: %s",
                 paste(valid_ress, collapse = ", ")))
  }

  # construct zip file name and the URL
  zipfilename <- paste0(
    "gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11_",
    year, "_", res, "_tif.zip"
  )
  url <- paste0(
    "https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/",
    "gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11/",
    zipfilename
  )

  # set the default download directory
  if (is.null(downloaddir))
  {
    downloaddir <- file.path(Sys.getenv("HOME"), "Downloads")
    if (Sys.info()["sysname"] == "Windows")
      downloaddir <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
  }

  # check the specified download directory exits
  if (!dir.exists(downloaddir))
    stop("'downloaddir' does not exist or is not a directory.")

  # ensure directory ends with '/'
  if (!grepl("/$", downloaddir))
    downloaddir <- paste0(downloaddir, "/")

  # full path to the ZIP file in the download directory
  zipfilename <- paste0(downloaddir, zipfilename)


  if (!file.exists(zipfilename))
  {
    # open browser for authentication and downloading the file
    utils::browseURL(url)

    # reminder to manually download the ZIP file
    message("From the opened browser, please\n   ",
            "1) enter your NASA Earthdata username and password for authentication\n   ",
            "2) make sure the downloaded file is in the directory: ",
            downloaddir, "\n")
    cat("If the download is complete, press Enter to continue...\n")
    readline()
  }

  # check successful download and correct path
  if (!file.exists(zipfilename))
    stop("The dowlonaded file cannot be find. Make sure it is at hte following path:\n",
         zipfilename)

  # extraction directory for the downloaded zip file
  exdir <- paste0(downloaddir, "popdensity/")
  if (!dir.exists(exdir))
    dir.create(exdir)

  # extract the contents of the ZIP file
  utils::unzip(zipfilename, exdir=exdir)

  # load raster data
  rdata <- terra::rast(list.files(exdir, pattern = "\\.tif$", full.names = TRUE))
  w <- terra::ext(c(bbox[1], bbox[3], bbox[2], bbox[4]))
  rdata <- terra::crop(rdata, w)

  # if 'where' is a matrix/data frame, extract pop-density values for points
  if (inherits(where, c("matrix", "data.frame")))
  {
    where <- data.frame(where)
    rdata <- terra::extract(rdata, where, ID=FALSE)
    rdata <- cbind(where, rdata)
  }

  return(rdata)
}
