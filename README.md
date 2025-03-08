
# getsat: Retrieving Commonly Used Satellite Data

<!-- badges: start -->
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

Please note that users must acknowledge the original data sources
appropriately. Any use of data from these platforms requires clear and
visible attribution, including proper citation and referencing of the
datasets, as specified by their licenses and terms of use.

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
#>   |                                                                              |                                                                      |   0%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================================================| 100%
# plot retrived elevation data
terra::plot(elev1)
```

<img src="man/figures/README-example1-1.png" width="100%" />

``` r

# Retrieve elevation for specific coordinate points
coords <- cbind(runif(n = 100, min = 47, max = 47.5), 
                runif(n = 100, min = 34, max = 35))
elev2 <- get_dem(coords, res = 90)
#>   |                                                                              |                                                                      |   0%  |                                                                              |============                                                          |  17%  |                                                                              |=======================                                               |  33%  |                                                                              |===================================                                   |  50%  |                                                                              |===============================================                       |  67%  |                                                                              |==========================================================            |  83%  |                                                                              |======================================================================| 100%
# Print retrieved elevation values
head(elev2)
#>         X1       X2 elevation
#> 1 47.26226 34.49864  2100.793
#> 2 47.03771 34.53155  1335.661
#> 3 47.12847 34.18226  1715.443
#> 4 47.46421 34.09917  1612.152
#> 5 47.25686 34.81869  2334.115
#> 6 47.04509 34.82563  1959.957
```

## Example 2: Retrieve temprature from MODIS

This example shows how to retrieve 8-day daytime temperature data from
the [Moderate Resolution Imaging Spectroradiometer
(MODIS)](https://planetarycomputer.microsoft.com/dataset/group/modis).
You can specify a bounding box or coordinates, and provide the desired
time period in the format “YYYY-MM-DD/YYYY-MM-DD”.

``` r
# Retrieve temperature data for a bounding box (longitude/latitude) and specific time period
temp <- get_modis(c(47, 34, 47.5, 35), var = "LST_Day_1KM",
                  datetime = "2025-01-01/2025-03-08")
#> LST_Day_1KM has been found in collection(s): modis-21A2-061
#>   |                                                                              |                                                                      |   0%  |                                                                              |=====                                                                 |   7%  |                                                                              |==========                                                            |  14%  |                                                                              |===============                                                       |  21%  |                                                                              |====================                                                  |  29%  |                                                                              |=========================                                             |  36%  |                                                                              |==============================                                        |  43%  |                                                                              |===================================                                   |  50%  |                                                                              |========================================                              |  57%  |                                                                              |=============================================                         |  64%  |                                                                              |==================================================                    |  71%  |                                                                              |=======================================================               |  79%  |                                                                              |============================================================          |  86%  |                                                                              |=================================================================     |  93%  |                                                                              |======================================================================| 100%
# plot retrived temperature data
terra::plot(temp)
```

<img src="man/figures/README-example2-1.png" width="100%" />
