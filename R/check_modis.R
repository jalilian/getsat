#' Check for MODIS Collections in Microsoft's Planetary Computer STAC API
#'
#' This function connects to the Microsoft Planetary Computer's STAC API, retrieves
#' all available collections, filters the ones related to MODIS (Moderate Resolution
#' Imaging Spectroradiometer), and returns a data frame containing the collection
#' `id`, `title`, and available asset variables for each MODIS collection. Please
#' follow the license and terms of use from NASA and Microsoft. Failure to comply
#' may result in data usage policy violations.
#'
#' @details
#' The function uses the `rstac` package to interface with the STAC API provided by
#' Microsoft's Planetary Computer. It retrieves a list of all collections and filters
#' those whose `id` starts with "modis". It then returns a data frame containing
#' the `id`, `title`, and asset variable names of the filtered collections.
#'
#' **Important Note:**
#' MODIS data are provided by NASA LP DAAC at the USGS EROS Center (producer, licensor, processor)
#' and Microsoft (host, processor). Users are required to follow the license and
#' terms of use specified by NASA and Microsoft. Failure to do so may violate
#' the data usage policies.
#'
#' @return A data frame with the following columns:
#' \item{id}{The unique identifier of the MODIS collection.}
#' \item{title}{The title or name of the MODIS collection.}
#' \item{vars}{A list of asset variable names available for each MODIS collection.}
#'
#' @examples
#' \dontrun{
#'   # Get a data frame of MODIS collections and their available variables
#'   modis_collections <- check_modis()
#'   print(modis_collections)
#' }
#'
#' @references
#' Data Use Guidelines for NASA Terra and Aqua MODIS, Suomi NPP, and other Collections.
#' Avilable at https://modaps.modaps.eosdis.nasa.gov/services/faq/LAADS_Data-Use_Citation_Policies.pdf
#'
#' Microsoft Planetary Computer. Available at https://planetarycomputer.microsoft.com/
#'
#' @seealso \link[rstac]{stac}, \link[rstac]{collections}, \link[rstac]{get_request}
#'
#' @author Abdollah Jalilian
#'
#' @export
check_modis <- function()
{
  # attempt to connect to the API
  attempt <- 1
  repeat {
    # check API connectivity
    status <- httr::GET("https://planetarycomputer.microsoft.com/api/stac/v1")
    status <- httr::status_code(status)

    if (status == 200)
    {
      # Connect to the Microsoft Planetary Computer STAC API
      collecs <- rstac::stac(
        "https://planetarycomputer.microsoft.com/api/stac/v1"
      ) |>
        # Retrieve all collections
        rstac::collections() |>
        # HTTP GET requests to STAC web services
        rstac::get_request() |>
        # allow access assets from Microsoft's Planetary Computer
        rstac::items_sign(sign_fn=rstac::sign_planetary_computer())

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

  # extract id, title and description of collections
  collecs_modis <- do.call(rbind, lapply(collecs$collections, function(o) {
    data.frame(id = o$id, title = o$title, description=o$description,
               stringsAsFactors=FALSE)
  }))

  # filter MODIS collections
  idx <- grepl("^modis", collecs_modis$id)
  if (!any(idx))
    stop("No MODIS collections found.")
  collecs_modis <- collecs_modis[idx, ]

  # extract available variables
  collecs_modis$vars <- lapply(collecs$collections[idx],
                               function(o) names(o$item_assets))
  return(collecs_modis)
}
