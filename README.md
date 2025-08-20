
# getsat: Retrieving Commonly Used Satellite Data

<!-- badges: start -->

[![R Build
Status](https://github.com/jalilian/getsat/workflows/R-CMD-check/badge.svg)](https://github.com/jalilian/getsat/actions)
<!-- badges: end -->

`getsat` is an R package designed to simplify access to commonly used
satellite data on environmental variables. It provides an easy way to
retrieve, process, and integrate satellite-based data from sources such
as:

- [Copernicus Climate Data Store](https://cds.climate.copernicus.eu)
- [Copernicus Atmosphere Data
  Store](https://ads.atmosphere.copernicus.eu)
- [Microsoft Planetary
  Computer](https://planetarycomputer.microsoft.com)
- [Climatology Lab](https://www.climatologylab.org/terraclimate.html)

Please note that users must acknowledge the original data sources
appropriately. Any use of data from these platforms requires clear and
visible attribution, including proper citation and referencing of the
datasets, as specified by their licenses and terms of use.

Development of this package was supported by the Lancaster Ecology and
Epidemiology Group (LEEG) and funded by Biotechnology and Biological
Sciences Research Council (BB/Y514238/1) and the Wellcome Trust
(220870/Z/20/Z).

## Installation

To install the development version of `getsat` from
[GitHub](https://github.com/), use the
[remotes](https://CRAN.R-project.org/package=remotes) package:

``` r
# Install pak if not already installed
install.packages("remotes")

# Install getsat from GitHub
remotes::install_github("jalilian/getsat")
```

## Example 1: Retrieve elevation data

The following example shows how to retrieve elevation data from the
[Copernicus Digital Elevation Model
(DEM)](https://doi.org/10.5069/G9028PQB). You can specify a bounding box
for an area of interest or provide specific coordinates.

``` r
library("getsat")

# Retrieve elevation data for a bounding box (longitude/latitude)
elev1 <- get_dem(c(47, 34, 47.5, 35), res=90)
#> For citation and terms of use, see
#> <https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM>
#> Getting Copernicus DEM data tiles:
#>   Copernicus_DSM_COG_30_N34_00_E047_00_DEM
#>   Copernicus_DSM_COG_30_N33_00_E047_00_DEM
#>   |                                                                                                                                                        |                                                                                                                                                |   0%  |                                                                                                                                                        |========================================================================                                                                        |  50%  |                                                                                                                                                        |================================================================================================================================================| 100%
# plot retrived elevation data
terra::plot(elev1)
```

<img src="man/figures/README-example1-1.png" width="100%" />

``` r

# Retrieve elevation for specific coordinate points
coords <- cbind(runif(n = 100, min = 47, max = 47.5), 
                runif(n = 100, min = 34, max = 35))
elev2 <- get_dem(coords, res = 90)
#> For citation and terms of use, see
#> <https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM>
#> Getting Copernicus DEM data tiles:
#>   Copernicus_DSM_COG_30_N35_00_E047_00_DEM
#>   Copernicus_DSM_COG_30_N35_00_E046_00_DEM
#>   Copernicus_DSM_COG_30_N34_00_E047_00_DEM
#>   Copernicus_DSM_COG_30_N34_00_E046_00_DEM
#>   Copernicus_DSM_COG_30_N33_00_E047_00_DEM
#>   Copernicus_DSM_COG_30_N33_00_E046_00_DEM
#>   |                                                                                                                                                        |                                                                                                                                                |   0%  |                                                                                                                                                        |========================                                                                                                                        |  17%  |                                                                                                                                                        |================================================                                                                                                |  33%  |                                                                                                                                                        |========================================================================                                                                        |  50%  |                                                                                                                                                        |================================================================================================                                                |  67%  |                                                                                                                                                        |========================================================================================================================                        |  83%  |                                                                                                                                                        |================================================================================================================================================| 100%
# Print retrieved elevation values
head(elev2)
#>         X1       X2 elevation
#> 1 47.43021 34.85579  1753.981
#> 2 47.36344 34.27102  1333.240
#> 3 47.19036 34.60832  1658.178
#> 4 47.04935 34.10538  1480.269
#> 5 47.15122 34.10269  1499.957
#> 6 47.23885 34.34738  1387.083
```

## Example 2: Retrieve temprature from MODIS

This example shows how to retrieve 8-day daytime temperature data from
the [Moderate Resolution Imaging Spectroradiometer
(MODIS)](https://planetarycomputer.microsoft.com/dataset/group/modis).
Make sure to properly cite MODIS data following [NASA’s
guidelines](https://modaps.modaps.eosdis.nasa.gov/services/faq/LAADS_Data-Use_Citation_Policies.pdf).
You can specify a bounding box “c(xmin, ymin, xmax, ymax)” or
coordinates (longitude, latitude), and provide the desired time period
in the format “YYYY-MM-DD/YYYY-MM-DD”.

``` r
# Retrieve temperature data for a bounding box (longitude/latitude) and specific time period
temp <- get_modis(c(47, 34, 47.5, 35), var = "LST_Day_1KM",
                  datetime = "2025-01-01/2025-03-09")
#> See 'Data Use Guidelines for NASA Terra and Aqua MODIS, Suomi NPP, and other Collections.'
#> - Available at <https://modaps.modaps.eosdis.nasa.gov/services/faq/LAADS_Data-Use_Citation_Policies.pdf>
#> [32mLST_Day_1KM[0m has been found in collection(s):
#>                                                                         
#> 55 modis-21A2-061 MODIS Land Surface Temperature/3-Band Emissivity 8-Day
#> -- Collection description  ------------------------ 
#>  A suite of Moderate Resolution Imaging Spectroradiometer (MODIS) Land Surface Temperature and Emissivity (LST&E) products are available in Collection 6.1. The MOD21 Land Surface Temperatuer (LST) algorithm differs from the algorithm of the MOD11 LST products, in that the MOD21 algorithm is based on the ASTER Temperature/Emissivity Separation (TES) technique, whereas the MOD11 uses the split-window technique. The MOD21 TES algorithm uses a physics-based algorithm to dynamically retrieve both the LST and spectral emissivity simultaneously from the MODIS thermal infrared bands 29, 31, and 32. The TES algorithm is combined with an improved Water Vapor Scaling (WVS) atmospheric correction scheme to stabilize the retrieval during very warm and humid conditions. This dataset is an 8-day composite LST product at 1,000 meter spatial resolution that uses an algorithm based on a simple averaging method. The algorithm calculates the average from all the cloud free 21A1D and 21A1N daily acquisitions from the 8-day period. Unlike the 21A1 data sets where the daytime and nighttime acquisitions are separate products, the 21A2 contains both daytime and nighttime acquisitions as separate Science Dataset (SDS) layers within a single Hierarchical Data Format (HDF) file. The LST, Quality Control (QC), view zenith angle, and viewing time have separate day and night SDS layers, while the values for the MODIS emissivity bands 29, 31, and 32 are the average of both the nighttime and daytime acquisitions. Additional details regarding the method used to create this Level 3 (L3) product are available in the Algorithm Theoretical Basis Document (ATBD). 
#>  --------------------------------------------------
#> Connecting to the Microsoft Planetary Computer STAC API...
#> Getting MODIS data 
#>   tiles: h21v05
#>   dates: 2025-03-06, 2025-02-26, 2025-02-18, 2025-02-10, 2025-02-02, 2025-01-25, 2025-01-17, 2025-01-09, 2025-01-01
#>   |                                                                                                                                                        |                                                                                                                                                |   0%  |                                                                                                                                                        |========                                                                                                                                        |   6%  |                                                                                                                                                        |================                                                                                                                                |  11%  |                                                                                                                                                        |========================                                                                                                                        |  17%  |                                                                                                                                                        |================================                                                                                                                |  22%  |                                                                                                                                                        |========================================                                                                                                        |  28%  |                                                                                                                                                        |================================================                                                                                                |  33%  |                                                                                                                                                        |========================================================                                                                                        |  39%  |                                                                                                                                                        |================================================================                                                                                |  44%  |                                                                                                                                                        |========================================================================                                                                        |  50%  |                                                                                                                                                        |================================================================================                                                                |  56%  |                                                                                                                                                        |========================================================================================                                                        |  61%  |                                                                                                                                                        |================================================================================================                                                |  67%  |                                                                                                                                                        |========================================================================================================                                        |  72%  |                                                                                                                                                        |================================================================================================================                                |  78%  |                                                                                                                                                        |========================================================================================================================                        |  83%  |                                                                                                                                                        |================================================================================================================================                |  89%  |                                                                                                                                                        |========================================================================================================================================        |  94%  |                                                                                                                                                        |================================================================================================================================================| 100%
# plot retrived temperature data
terra::plot(temp)
```

<img src="man/figures/README-example2-1.png" width="100%" />
