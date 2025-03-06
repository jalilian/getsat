
# getsat: Retrieving Commonly Used Satellite Data

<!-- badges: start -->
<!-- badges: end -->

`getsat` is an R package designed to simplify access to commonly used
satellite data on environmental variables. It provides an easy way to
retrieve, process, and integrate satellite-based data from sources such
as: - Copernicus Atmosphere Monitoring Service - Copernicus Climate Data
Store - Microsoft Planetary Computer

## Installation

To install the development version of `getsat` from
[GitHub](https://github.com/), use the
[pak](https://CRAN.R-project.org/package=pak) package:

``` r
# Install pak if not already installed
install.packages("pak")

# Install getsat from GitHub
pak::pak("jalilian/getsat")
```

## Example 1: Retrieve Elevation Data

The following example shows how to retrieve elevation data from the
Copernicus Digital Elevation Model (DEM). You can specify a bounding box
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
#> 1 47.16671 34.65571  1647.810
#> 2 47.01319 34.23099  1730.346
#> 3 47.38901 34.13156  1525.568
#> 4 47.28066 34.54409  2311.858
#> 5 47.03034 34.63642  2183.548
#> 6 47.30659 34.33944  1354.311
```

## Example 2: Retrieve Elevation Data

This example shows how to retrieve 8-day daytime temperature data from
the Moderate Resolution Imaging Spectroradiometer (MODIS). You can
specify a bounding box or coordinates, and provide the desired time
period in the format “YYYY-MM-DD/YYYY-MM-DD”.

``` r
# Retrieve temperature data for a bounding box (longitude/latitude) and specific time period
temp <- get_modis(c(47, 34, 47.5, 35), var = "LST_Day_1KM",
                  datetime = "2024-12-01/2025-01-28")
#> LST_Day_1KM has been found in collection(s): modis-21A2-061
#>   |                                                                              |                                                                      |   0%  |                                                                              |====                                                                  |   6%  |                                                                              |=========                                                             |  12%  |                                                                              |=============                                                         |  19%  |                                                                              |==================                                                    |  25%  |                                                                              |======================                                                |  31%  |                                                                              |==========================                                            |  38%  |                                                                              |===============================                                       |  44%  |                                                                              |===================================                                   |  50%  |                                                                              |=======================================                               |  56%  |                                                                              |============================================                          |  62%  |                                                                              |================================================                      |  69%  |                                                                              |====================================================                  |  75%  |                                                                              |=========================================================             |  81%  |                                                                              |=============================================================         |  88%  |                                                                              |==================================================================    |  94%  |                                                                              |======================================================================| 100%
# plot retrived temperature data
terra::plot(temp)
```

<img src="man/figures/README-example2-1.png" width="100%" />
