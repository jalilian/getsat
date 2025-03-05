#' Download CAMS European Air Quality Data
#'
#' This function retrieves air quality data from the Copernicus Atmosphere 
#' Monitoring Service (CAMS) using the ECMWF API. It fetches validated 
#' reanalysis data and forecasts for the European domain, covering longitudes 
#' from 45.0° W to 25.0° E and latitudes from 30.0° N to 72.0° N. The data has 
#' a high spatial resolution of 0.1 degrees (~10 km) and is generated through 
#' an ensemble of eleven air quality forecasting systems combined with 
#' observations from the European Environment Agency (EEA) using data 
#' assimilation techniques. The function returns the processed data as a 
#' `SpatRaster` object for spatial analysis.
#'
#' @param key A character string. ECMWF API key associated with your ECMWF account.
#' @param vars A character vector specifying air quality variables to retrieve. 
#'   Supported values include:
#'   - "ammonia" – Ammonia (NH₃)  
#'   - "carbon_monoxide" – Carbon monoxide (CO)  
#'   - "formaldehyde" – Formaldehyde (HCHO)  
#'   - "glyoxal" – Glyoxal (CHOCHO)  
#'   - "nitrogen_dioxide" – Nitrogen dioxide (NO₂)  
#'   - "nitrogen_monoxide" – Nitrogen monoxide (NO)  
#'   - "non_methane_vocs" – Non-methane volatile organic compounds (NMVOCs)  
#'   - "ozone" – Ozone (O₃)  
#'   - "particulate_matter_10um" – Particulate matter with diameter < 10 µm (PM10)  
#'   - "dust" – PM10 dust fraction  
#'   - "pm10_sea_salt_dry" – PM10 sea salt (dry)  
#'   - "pm10_wildfires" – PM10 from wildfires only  
#'   - "particulate_matter_2.5um" – Particulate matter with diameter < 2.5 µm (PM2.5)  
#'   - "total_elementary_carbon" – PM2.5 total anthropogenic carbon  
#'   - "residential_elementary_carbon" – PM2.5 residential anthropogenic carbon  
#'   - "secondary_inorganic_aerosol" – PM2.5 secondary inorganic aerosol (SIA)  
#'   - "pm2.5_total_organic_matter" – PM2.5 total organic matter  
#'   - "peroxyacyl_nitrates" – Peroxyacyl nitrates (PANs)  
#'   - "sulphur_dioxide" – Sulphur dioxide (SO₂)  
#'
#' @param where A numeric vector of length 4 specifying the geographical bounding box 
#'   (`c(North, West, South, East)`) or a matrix/data frame with two columns 
#'   (longitude and latitude of points). All coordinates must be in WGS84 format.
#'   The valid data coverage region is:
#'   - North: 72.0° N
#'   - South: 30.0° N
#'   - West: -45.0° W
#'   - East: 25.0° W
#'
#' @param year A numeric value indicating the year for which data is requested. 
#'   Available years range from 2013 to 2023. If the year is 2022 or earlier, 
#'   validated reanalysis data is used; for 2023 onward, interim reanalysis data is used.
#'
#' @param month A numeric or character vector specifying the months (default: `1:12`).
#'   The values are formatted as two-digit strings (e.g., "01", "02").
#'
#' @param level A character or numeric value indicating the vertical level above 
#'   the surface (default: "0" for surface level). Possible values include:
#'   - "0" (surface)
#'   - "50m", "100m", "250m", "500m", "750m"
#'   - "1000m", "2000m", "3000m", "5000m"
#'
#' @param agglevel A character string specifying the temporal aggregation level.
#'   Available options:
#'   - `NULL` (no aggregation)
#'   - "years", "months", "yearmonths"
#'   - "dekads" (10-day periods), "yeardekads"
#'   - "weeks" (ISO 8601 week number), "yearweeks"
#'   - "days" (default), "doy" (day of the year)
#'   - "7days", "10days", "15days"
#'
#' @param temp_dir A character string specifying a temporary directory for downloaded files. 
#'   If `NULL` (default), the system's temporary directory (`tempdir()`) is used.
#'
#' @return If `where` is a bounding box (vector of length four), a `SpatRaster` object 
#'   (or a list of them) representing the selected air quality variables. If `where` is 
#'   a matrix or data frame, a `data.frame` containing coordinates and corresponding values.
#'
#' @details
#' The function facilitates the automated retrieval of air quality data from the CAMS 
#' reanalysis database. It performs the following steps:
#' 1. Authenticates with ECMWF using the provided API key.
#' 2. Creates a temporary directory for data storage.
#' 3. Constructs and validates the data request.
#' 4. Downloads data in ZIP format.
#' 5. Extracts the ZIP archive and loads the data as a `SpatRaster` object.
#'
#' @examples
#' \dontrun{
#' # Define geographical area (North, West, South, East)
#' area <- c(47.1, 6.6, 35.4, 18.6)
#'
#' # Set ECMWF API key (replace with your actual key)
#' key <- "********************************"
#'
#' # Download ozone concentration data for March 2021
#' oz <- get_cams(key, vars = "ozone", year = 2021, month = 3, where = area)
#'
#' # Plot the retrieved data
#' plot(oz)
#' }
#'
#' @importFrom terra
#' @author Abdollah Jalilian
#' @references
#' - Copernicus Atmosphere Monitoring Service (CAMS): https://atmosphere.copernicus.eu/
#' - ECMWF API Documentation: https://confluence.ecmwf.int/display/WEBAPI/Access+ECMWF+Public+Datasets
#' @export
get_cams <- function(key, 
                     vars,
                     where,
                     year, 
                     month = 1:12,
                     level = "0",
                     agglevel="days",
                     temp_dir = NULL)
{
  # air quality variables
  aq_vars <- c(
    "ammonia", # Ammonia
    "carbon_monoxide", # Carbon monoxide
    "formaldehyde", # Formaldehyde
    "glyoxal", # Glyoxal
    "nitrogen_dioxide", # Nitrogen dioxide
    "nitrogen_monoxide", # Nitrogen monoxide
    "non_methane_vocs", # Non-methane volatile compounds (VOCs)
    "ozone", # Ozone
    "particulate_matter_10um", # Particulate matter d < 10 µm (PM10)
    "dust", # PM 10 Dust fraction
    "pm10_sea_salt_dry", # PM10 sea salt (dry)
    "pm10_wildfires", # Particulate matter d < 10 µm, wildfires only (PM10_WF)
    "particulate_matter_2.5um", # Particulate matter d < 2.5 µm (PM2.5)
    "total_elementary_carbon", # Particulate matter d < 2.5 µm, anthropogenic total carbon (PM2.5_tot)
    "residential_elementary_carbon", # Particulate matter d < 2.5 µm, anthropogenic residential carbon only (PM2.5_res)
    "secondary_inorganic_aerosol", # PM 2.5 Secondary inorganic aerosol (SIA)
    "pm2.5_total_organic_matter", # PM2.5 total organic matter
    "peroxyacyl_nitrates", # Peroxyacyl nitrates (PANs)
    "sulphur_dioxide" # Sulphur dioxide
  )
  
  # check if all specified variables are in the valid list of variables
  if (!all(vars %in% aq_vars))
    stop("Invalid value(s) in 'vars'. Allowed values are: ", 
         paste(aq_vars, collapse = ", "))
  
  # Validate 'where' input
  if (is.numeric(where) && length(where) == 4)
    area <- where
  else if (inherits(where, c("matrix", "data.frame")) && ncol(where) == 2)
    area <- c(max(where[, 2]) + 0.15, 
              min(where[, 1]) - 0.15, 
              min(where[, 2]) - 0.15, 
              max(where[, 1]) + 0.15)
  else
    stop("'where' must be a numeric vector of length 4 or a matrix/data.frame with two columns.")
  
  # valid geographical boundaries
  valid_area <- list(north = 72.0, south = 30.0,
                     west = -45.0,   east = 25.0)
  if (any(area[1] > valid_area$north | 
          area[3] < valid_area$south | 
          area[2] < valid_area$west | 
          area[4] > valid_area$east))
    stop(paste("The specified area is outside the valid data coverage region.", 
               "Please specify an area within North: 72.0, South: 30.0, West: -45.0, East: 25.0."))
  
  # validate the year
  if (any(year < 2013 | year > 2023))
    stop("Data is currently only available for years between 2013 and 2025.")
    
  # set the data type based on the year
  type <- ifelse(year <= 2022, 
                 # Analysis data
                 "validated_reanalysis", 
                 # Forecast data
                 "interim_reanalysis")
  
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
    dataset_short_name = "cams-europe-air-quality-reanalyses",
    # air quality variables 
    variable = vars,
    # numerical air quality model
    model = "ensemble",
    # vertical level above the surface
    #     surface, 50m,100m, 250m, 500m, 750m, 1000m, 2000m, 3000m, 5000m
    level = level,
    type = type,
    # temporal framework: year, month, day, hour
    year = year,
    month = sprintf("%02d", month),
    # geographical region: North, West, South, East
    area = area,
    # output file format
    format = "zip",
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
  
  # unzip downloaded file 
  zfiles <- unzip(out_file, list=TRUE)
  unzip(out_file, exdir=temp_dir)
  
  # create a SpatRaster object from the downloaded data file
  rdata <- lapply(
    zfiles$Name, 
    function(o) terra::rast(file.path(temp_dir, o))
  )
  
  # perform temporal aggregation if specified
  if (!is.null(agglevel)) 
    rdata <- lapply(
      rdata, 
      function(o) terra::tapp(o, index=agglevel, fun="mean")
    )
  
  # If only one variable, simplify the output
  if (length(vars) == 1)
    rdata <- rdata[[1]]
  else
    names(rdata) <- vars
  
  # extract data if 'where' is a matrix
  if (inherits(where, c("matrix", "data.frame"))) 
  {
    where <- data.frame(where)
    rdata <- lapply(
      rdata, 
      function(o) terra::extract(o, where, ID=FALSE)
    )
    rdata <- data.frame(where, do.call(cbind, rdata)) %>%
      pivot_longer(cols = -(1:2), 
                   names_to = c(".value", "time"),
                   names_pattern = "(.*)_(.*)") %>%
      mutate(time = gsub("\\.", "-", time)) %>%
      rename_with(~ sub("\\.d", "", .x))
  }
  
  return(rdata)
}
