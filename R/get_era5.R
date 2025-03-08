#' Download ERA5-Land Climate Data
#'
#' This function retrieves ERA5-Land data from the Copernicus Data Store via the
#' ECMWF API. ERA5-Land provides a detailed, long-term record of land surface
#' variables from 1950 to the present. It is derived from the ERA5 climate
#' reanalysis model, using both model data and observations, and adjusted for
#' elevation.
#'
#' **Important Note:**
#' Any use of data provided by the Copernicus Climate Data Store, including ERA5
#' data, must include proper citation and acknowledgement of the data sources.
#' Users are required to follow the license and terms of use specified by
#' Copernicus and ECMWF. Failure to do so may violate the data usage policies.
#'
#'
#' @param key A character string representing the ECMWF API key associated with your user account.
#'
#' @param vars A character vector specifying the land surface variables to retrieve. Supported values include:
#' \itemize{
#'   \item "2m_dewpoint_temperature", "2m_temperature", "skin_temperature"
#'   \item "soil_temperature_level_1", "soil_temperature_level_2", "soil_temperature_level_3", "soil_temperature_level_4"
#'   \item "lake_bottom_temperature", "lake_ice_depth", "lake_ice_temperature", "lake_mix_layer_depth", "lake_mix_layer_temperature"
#'   \item "snow_albedo", "snow_cover", "snow_density", "snow_depth", "snow_depth_water_equivalent", "snowfall", "snowmelt", "temperature_of_snow_layer"
#'   \item "skin_reservoir_content", "volumetric_soil_water_layer_1", "volumetric_soil_water_layer_2", "volumetric_soil_water_layer_3", "volumetric_soil_water_layer_4"
#'   \item "surface_latent_heat_flux", "surface_net_solar_radiation", "surface_net_thermal_radiation", "surface_sensible_heat_flux"
#'   \item "evaporation_from_bare_soil", "evaporation_from_open_water_surfaces_excluding_oceans", "evaporation_from_the_top_of_canopy", "evaporation_from_vegetation_transpiration"
#'   \item "potential_evaporation", "runoff", "snow_evaporation", "sub_surface_runoff", "surface_runoff", "total_evaporation"
#'   \item "10m_u_component_of_wind", "10m_v_component_of_wind", "surface_pressure", "total_precipitation"
#'   \item "leaf_area_index_high_vegetation", "leaf_area_index_low_vegetation", "high_vegetation_cover", "low_vegetation_cover"
#'   \item "lake_total_depth", "land_sea_mask", "soil_type", "type_of_high_vegetation", "type_of_low_vegetation"
#' }
#'
#' @param area A numeric vector of length 4 defining the bounding box for the region of interest,
#'             in the format `c(North, West, South, East)`. Coordinates must fall within:
#'             - North: 90.0° N
#'             - South: -90.0° N
#'             - West: -180.0° W
#'             - East: 180.0° W
#'
#' @param year A numeric value specifying the year to retrieve data for (from 1950 onwards).
#'
#' @param month A numeric or character vector specifying the months to retrieve (default: all months in the year).
#'              Values are automatically formatted as two-digit strings (e.g., `"01"`, `"02"`).
#'
#' @param day A numeric or character vector specifying the days to retrieve (default: all days for the specified months).
#'            Values are automatically formatted as two-digit strings (e.g., `"01"`, `"02"`).
#'
#' @param time A character vector specifying the hours to retrieve (default: all 24 hours of the day).
#'             The values are automatically formatted as two-digit hour strings (e.g., `"00:00"`, `"01:00"`).
#'
#' @param agglevel A character string specifying the temporal aggregation level.
#'   Options are:
#'     \itemize{
#'       \item `NULL` (no aggregation)
#'       \item `"years"`
#'       \item `"months"`
#'       \item `"yearmonths"`
#'       \item `"dekads"` (10-day periods)
#'       \item `"yeardekads"`
#'       \item `"weeks"` (ISO 8601 week number)
#'       \item `"yearweeks"`
#'       \item `"days"` (default)
#'       \item `"doy"` (day of the year)
#'       \item `"7days"` (seven-day periods starting at Jan 1 of each year)
#'       \item `"10days"`
#'       \item `"15days"`
#'     }
#'
#' @param temp_dir A character string specifying a temporary directory for downloaded
#'   and extracted files. If `NULL` (default), the system's temporary directory
#'   (`tempdir()`) is used.
#'
#' @return A `SpatRaster` object containing the downloaded air quality data.
#'
#' @details This function retrieves data from the ERA5-Land dataset, which covers the period from January 1950 to
#'          approximately 2-3 months before the present. The data is processed into a `SpatRaster` object for spatial analysis.
#'          Temporal aggregation (e.g., by months or years) can be performed after retrieval if needed.
#'
#' @examples
#' \dontrun{
#' # Define the geographical area (North, West, South, East)
#' area <- c(47.1, 6.6, 35.4, 18.6)
#'
#' # Set ECMWF API key (replace with your actual key)
#' key <- "********************************"
#'
#' # Download skin temperature data for 6th of October 2022
#' st <- get_era5_land(key, vars = "skin_temperature", area = area,
#'                     year = 2022, month = 10, day=6)
#'
#' # Plot the retrieved data
#' plot(st)
#' }
#'
#' @references
#' Copernicus Climate Data Store: https://cds.climate.copernicus.eu/
#' ECMWF API Documentation: https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5
#'
#' @author Abdollah Jalilian
#'
#' @export
get_era5_land <- function(key, vars, area, year, month = NULL, day = NULL,
                          time = NULL, agglevel="days", temp_dir = NULL)
{
  # ERA5-Land climate variables
  c_vars <- c(
    "2m_dewpoint_temperature", # Dew point temperature at 2 meters above the surface (K)
    "2m_temperature",         # Air temperature at 2 meters above the surface (K)
    "skin_temperature",        # Temperature of the land surface (K)
    "soil_temperature_level_1", # Soil temperature at level 1 (K)
    "soil_temperature_level_2", # Soil temperature at level 2 (K)
    "soil_temperature_level_3", # Soil temperature at level 3 (K)
    "soil_temperature_level_4", # Soil temperature at level 4 (K)
    "lake_bottom_temperature",  # Lake bottom temperature (K)
    "lake_ice_depth",          # Lake ice depth (m)
    "lake_ice_temperature",    # Lake ice temperature (K)
    "lake_mix_layer_depth",    # Lake mix layer depth (m)
    "lake_mix_layer_temperature",# Lake mix layer temperature (K)
    "lake_shape_factor",       # Lake shape factor (dimensionless)
    "lake_total_layer_temperature", # Lake total layer temperature (K)
    "snow_albedo",             # Snow albedo (dimensionless)
    "snow_cover",              # Snow cover (fraction)
    "snow_density",            # Snow density (kg m-3)
    "snow_depth",              # Snow depth (m)
    "snow_depth_water_equivalent", # Snow depth water equivalent (kg m-2)
    "snowfall",                # Snowfall (m)
    "snowmelt",                # Snowmelt (m)
    "temperature_of_snow_layer", # Temperature of the snow layer (K)
    "skin_reservoir_content",   # Skin reservoir content (kg m-2)
    "volumetric_soil_water_layer_1", # Volumetric soil water at level 1 (m3 m-3)
    "volumetric_soil_water_layer_2", # Volumetric soil water at level 2 (m3 m-3)
    "volumetric_soil_water_layer_3", # Volumetric soil water at level 3 (m3 m-3)
    "volumetric_soil_water_layer_4", # Volumetric soil water at level 4 (m3 m-3)
    "forecast_albedo",          # Forecast albedo (dimensionless)
    "surface_latent_heat_flux", # Surface latent heat flux (W m-2)
    "surface_net_solar_radiation", # Surface net solar radiation (W m-2)
    "surface_net_thermal_radiation", # Surface net thermal radiation (W m-2)
    "surface_sensible_heat_flux",# Surface sensible heat flux (W m-2)
    "surface_solar_radiation_downwards", # Surface solar radiation downwards (W m-2)
    "surface_thermal_radiation_downwards", # Surface thermal radiation downwards (W m-2)
    "evaporation_from_bare_soil", # Evaporation from bare soil (m)
    "evaporation_from_open_water_surfaces_excluding_oceans", # Evaporation from open water surfaces (excluding oceans) (m)
    "evaporation_from_the_top_of_canopy", # Evaporation from the top of canopy (m)
    "evaporation_from_vegetation_transpiration", # Evaporation from vegetation transpiration (m)
    "potential_evaporation",    # Potential evaporation (m)
    "runoff",                   # Runoff (m)
    "snow_evaporation",         # Snow evaporation (m)
    "sub_surface_runoff",       # Sub-surface runoff (m)
    "surface_runoff",           # Surface runoff (m)
    "total_evaporation",        # Total evaporation (m)
    "10m_u_component_of_wind",  # Eastward component of wind at 10 meters (m s-1)
    "10m_v_component_of_wind",  # Northward component of wind at 10 meters (m s-1)
    "surface_pressure",         # Surface pressure (Pa)
    "total_precipitation",      # Total precipitation (m)
    "leaf_area_index_high_vegetation", # Leaf area index for high vegetation (dimensionless)
    "leaf_area_index_low_vegetation", # Leaf area index for low vegetation (dimensionless)
    "high_vegetation_cover",    # High vegetation cover (fraction)
    "glacier_mask",             # Glacier mask (dimensionless)
    "lake_cover",               # Lake cover (fraction)
    "low_vegetation_cover",     # Low vegetation cover (fraction)
    "lake_total_depth",         # Lake total depth (m)
    "land_sea_mask",            # Land-sea mask (dimensionless)
    "soil_type",                # Soil type (categorical)
    "type_of_high_vegetation",  # Type of high vegetation (categorical)
    "type_of_low_vegetation"   # Type of low vegetation (categorical)
  )

  # check if all specified variables are in the valid list of variables
  if (!all(vars %in% c_vars))
    stop("Invalid value(s) in 'vars'. Allowed values are: ",
         paste(c_vars, collapse = ", "))

  # validate the year
  if (any(year < 1940))
    stop("Data is only available form January 1940")

  # validate the month
  valid_months <- sprintf("%02d", 1:12)
  if (is.null(month))
    month <- valid_months
  else
    month <- sprintf("%02d", month)
  if (!all(month %in% valid_months))
    stop("Invalid value(s) in 'month'. Allowed values are: ",
         paste(valid_months, collapse = ", "))

  # validate the day
  valid_days <- sprintf("%02d", 1:31)
  if (is.null(day))
    day <- valid_days
  else
    day <- sprintf("%02d", day)
  if (!all(day %in% valid_days))
    stop("Invalid value(s) in 'day'. Allowed values are: ",
         paste(valid_days, collapse = ", "))

  # validate the time
  valid_times <- sprintf("%02d:00", 0:23)
  if (is.null(time))
    time <- valid_times
  if (!all(time %in% valid_times))
    stop("Invalid value(s) in 'time'. Allowed values are: ",
         paste(valid_times, collapse = ", "))

  # set ECMWF authentication
  user <- "ecmwfr"
  wf_set_key(key = key, user = user)

  # create a temporary directory to downloaded and extract files
  if (is.null(temp_dir))
    temp_dir <- tempdir()
  if (!dir.exists(temp_dir))
  {
    dir.create(temp_dir)
  }

  # Define output filename
  dfile <- paste0("cams_hourly_", year, "_", month, ".zip")

  # request for getting data
  request <- list(
    # dataset name
    dataset_short_name = "reanalysis-era5-land",
    # air quality variables
    variable = vars,
    # temporal framework: year, month, day, hour
    year = as.character(year),
    month = month,
    day = day,
    time = as.character(time),
    # geographical region: North, West, South, East
    area = area,
    # output file format
    data_format = "grib",
    download_format = "unarchived",
    # output file name
    target = dfile
  )

  # Validate request and credentials
  wf_check_request(request = request)

  # Download data
  out_file <- wf_request(
    user = user,
    request = request,
    transfer = TRUE,
    path = getwd(),
    time_out = 3 * 60 * 60,
    verbose = TRUE
  )

  # create a SpatRaster object from the downloaded data file
  rdata <- terra::rast(out_file)

  # perform temporal aggregation if specified
  if (!is.null(agglevel))
  {
    rdata <- terra::tapp(rdata, index=agglevel, fun="mean")
  }

  return(rdata)
}
