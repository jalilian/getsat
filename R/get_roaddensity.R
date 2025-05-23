#' Retrieve Road Density from the Global Roads Inventory Project (GRIP), Version 4
#'
#' This function accesses and downloads road density data from the Global Roads Inventory Project (GRIP), version 4.
#' GRIP provides a consistent and up-to-date global roads dataset at approximately 10 km resolution,
#' which is widely used in environmental and biodiversity assessments.
#'
#' The function returns road density (measured as meters of road per square kilometer)
#' either as a raster (for a bounding box) or as values extracted at specific coordinate locations.
#' GRIP4 is available under a Creative Commons License (CC0) and is free for public use.
#'
#' @param where A numeric vector of length 4 specifying a bounding box (`c(xmin, ymin, xmax, ymax)`),
#' or a matrix/data frame with two columns (longitude, latitude) indicating point locations for extraction.
#'
#' @param type An integer indicating the road type to be retrieved. Options include:
#' \itemize{
#'   \item 0: All road types combined (default)
#'   \item 1: Highways
#'   \item 2: Primary roads
#'   \item 3: Secondary roads
#'   \item 4: Tertiary roads
#'   \item 5: Local roads
#' }
#'
#' @param downloaddir A character string specifying the local directory to store downloaded data.
#' If `NULL`, the default download location is used:
#' \itemize{
#'   \item Windows: \code{file.path(Sys.getenv("USERPROFILE"), "Downloads")}
#'   \item macOS/Linux: \code{file.path(Sys.getenv("HOME"), "Downloads")}
#' }
#'
#' @details
#' The GRIP dataset and its projections under global Shared Socioeconomic Pathways (SSPs)
#' are described in Meijer et al. (2018). For more information and data access, visit:
#' \url{https://www.globio.info/download-grip-dataset}
#'
#' @return A \code{SpatRaster} object (if `where` is a bounding box) or a data frame with road density
#' values at the given coordinates (if `where` is a matrix/data frame).
#'
#' @examples
#' \dontrun{
#' # Retrieve primary road density within a bounding box over Iran
#' rd1 <- get_roaddensity(c(44, 25, 63, 40), type = 2)
#' terra::plot(log(1 + rd1))  # plot log-transformed values
#'
#' # Retrieve total road density at specific coordinate points
#' coords <- cbind(runif(100, -5, -1), runif(100, 4, 10))
#' rd2 <- get_roaddensity(coords, type = 0)
#' head(rd2)
#' }
#'
#' @references
#' Meijer, J. R., Huijbregts, M. A., Schotten, K. C., & Schipper, A. M. (2018).
#' Global patterns of current and future road infrastructure. *Environmental Research Letters*, 13(6), 064006.
#' \url{https://doi.org/10.1088/1748-9326/aabd42}
#' GRIP Data: \url{https://www.globio.info/download-grip-dataset}
#'
#' @author Abdollah Jalilian
#' @export
get_roaddensity <- function(where,
                            type=0,
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

  # validate type
  valid_type <- 0:5
  if (!(is.numeric(type) && length(type) == 1 && type %in% valid_type))
    stop(sprintf("'type' must be a numeric value and one of the following: %s",
                 paste(valid_years, collapse = ", ")))

  # construct zip file name and the URL

  zipfilename <- paste0("GRIP4_density_",
                        ifelse(type == 0, "total", paste0("tp", type)),
                        ".zip")
  url <- paste0("https://dataportaal.pbl.nl/downloads/GRIP4/",
                zipfilename)

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
    # download the ZIP file
    message("Downloading from: ", url)
    utils::download.file(url, destfile = zipfilename)
  }

  # check successful download and correct path
  if (!file.exists(zipfilename))
    stop("The dowlonaded file cannot be find. Make sure it is at hte following path:\n",
         zipfilename)

  # extraction directory for the downloaded zip file
  exdir <- paste0(downloaddir, "raoddensity", type, "/")
  if (!dir.exists(exdir))
    dir.create(exdir)

  # extract the contents of the ZIP file
  utils::unzip(zipfilename, exdir=exdir)

  # load raster data
  rdata <- terra::rast(list.files(exdir, pattern = "dens_m_km2.asc$",
                                  full.names = TRUE))

  # if 'where' is a matrix/data frame, extract pop-density values for points
  if (inherits(where, c("matrix", "data.frame")))
  {
    where <- data.frame(where)
    rdata <- terra::extract(rdata, where, ID=FALSE)
    rdata <- cbind(where, rdata)
  } else{
    w <- terra::ext(c(bbox[1], bbox[3], bbox[2], bbox[4]))
    rdata <- terra::crop(rdata, w)
  }

  return(rdata)
}
