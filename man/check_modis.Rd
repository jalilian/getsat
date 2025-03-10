% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_modis.R
\name{check_modis}
\alias{check_modis}
\title{Check for MODIS Collections in Microsoft's Planetary Computer STAC API}
\usage{
check_modis()
}
\value{
A data frame with the following columns:
\item{id}{The unique identifier of the MODIS collection.}
\item{title}{The title or name of the MODIS collection.}
\item{vars}{A list of asset variable names available for each MODIS collection.}
}
\description{
This function connects to the Microsoft Planetary Computer's STAC API, retrieves
all available collections, filters the ones related to MODIS (Moderate Resolution
Imaging Spectroradiometer), and returns a data frame containing the collection
\code{id}, \code{title}, and available asset variables for each MODIS collection. Please
follow the license and terms of use from NASA and Microsoft. Failure to comply
may result in data usage policy violations.
}
\details{
The function uses the \code{rstac} package to interface with the STAC API provided by
Microsoft's Planetary Computer. It retrieves a list of all collections and filters
those whose \code{id} starts with "modis". It then returns a data frame containing
the \code{id}, \code{title}, and asset variable names of the filtered collections.

\strong{Important Note:}
MODIS data are provided by NASA LP DAAC at the USGS EROS Center (producer, licensor, processor)
and Microsoft (host, processor). Users are required to follow the license and
terms of use specified by NASA and Microsoft. Failure to do so may violate
the data usage policies.
}
\examples{
\dontrun{
  # Get a data frame of MODIS collections and their available variables
  modis_collections <- check_modis()
  print(modis_collections)
}

}
\references{
Data Use Guidelines for NASA Terra and Aqua MODIS, Suomi NPP, and other Collections.
Avilable at https://modaps.modaps.eosdis.nasa.gov/services/faq/LAADS_Data-Use_Citation_Policies.pdf

Microsoft Planetary Computer. Available at https://planetarycomputer.microsoft.com/
}
\seealso{
\link[rstac]{stac}, \link[rstac]{collections}, \link[rstac]{get_request}
}
\author{
Abdollah Jalilian
}
