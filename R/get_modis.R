#' Retrieve MODIS Data
#'
#' Access and download the Moderate Resolution Imaging Spectroradiometer (MODIS)
#' satellite data, available through the Microsoft's Planetary Computer STAC API.
#' It uses the STAC API, processes the retrieved raster data, and optionally
#' extracts values for given spatial points. Please follow the license and terms
#' of use from NASA and Microsoft. Failure to comply may result in data usage
#' policy violations.
#'
#' @param where A numeric vector of length 4 representing the bounding box in the
#'        form c(xmin, ymin, xmax, ymax) or a matrix/data.frame with two columns
#'      (longitude, latitude) representing points of interest.
#'
#' @param var A character string specifying the MODIS variable to be retrieved.
#'        see the details for more explanation.
#'
#' @param datetime A character string specifying the date range in the format
#'        "YYYY-MM-DD/YYYY-MM-DD".
#'
#' @param collection A character string specifying the name of the MODIS collection
#'        from which the variable should be retrieved. If `NULL` (default), the
#'        function will automatically determine the appropriate collection based
#'        on the selected variable.
#'
#' @param crop Logical, whether to crop the retrieved raster data to the bounding box. Default is TRUE.
#'
#' @param w Window size for the moving window method to fill missing (NA) values using the `focal` function from the `terra` package.
#'        If `w = NA` (default), missing values are not filled. See the documentation of `terra::focal` for more details.
#'
#' @param agglevel A character string specifying the temporal aggregation level for the retrieved data, using the `aggregate` function
#'        in the `terra` package. Available options include:
#'        - `NULL` (no aggregation)
#'        - `"years"`, `"months"`, `"yearmonths"`
#'        - `"dekads"` (10-day periods), `"yeardekads"`
#'        - `"weeks"` (ISO 8601 week number), `"yearweeks"`
#'        - `"days"` (default), `"doy"` (day of the year)
#'        - `"7days"`, `"10days"`, `"15days"`
#'
#' @param download Logical. If `TRUE`, MODIS files will be downloaded before processing.
#'        If `FALSE` (default), data is read directly from the server without downloading.
#'
#' @param output_dir A character string specifying the directory where downloaded files should be saved.
#'        Default is a temporary directory.
#'
#' @param clean_dir Logical. If `TRUE`, downloaded files are deleted after processing
#' (applies only when `download = TRUE`). Default is `FALSE`.
#'
#' @details
#'
#'#' **Important Note:**
#' MODIS data are provided by NASA LP DAAC at the USGS EROS Center (producer, licensor, processor)
#' and Microsoft (host, processor). Users are required to follow the license and
#' terms of use specified by NASA and Microsoft. Failure to do so may violate
#' the data usage policies.
#'
#' This function allows users to retrieve MODIS satellite data for specified
#' geographic regions and time periods. MODIS provides various environmental
#' datasets, such as land surface temperature, vegetation indices, burned areas,
#' and primary productivity. The following table provides information on available
#' MODIS collections and their corresponding variables:
#'
#'
#' \tabular{lll}{
#'   \strong{id} \tab \strong{title} \tab \strong{vars} \cr
#'   modis-64A1-061 \tab MODIS Burned Area Monthly \tab QA, hdf, Last_Day, metadata, Burn_Date, First_Day, Burn_Date_Uncertainty \cr
#'   modis-17A2H-061 \tab MODIS Gross Primary Productivity 8-Day \tab hdf, Gpp_500m, metadata, PsnNet_500m, Psn_QC_500m \cr
#'   modis-11A2-061 \tab MODIS Land Surface Temperature/Emissivity 8-Day \tab hdf, QC_Day, Emis_31, Emis_32, QC_Night, metadata, LST_Day_1km, Day_view_angl, Day_view_time, LST_Night_1km, Clear_sky_days, Night_view_angl, Night_view_time, Clear_sky_nights \cr
#'   modis-17A2HGF-061 \tab MODIS Gross Primary Productivity 8-Day Gap-Filled \tab hdf, Gpp_500m, metadata, PsnNet_500m, Psn_QC_500m \cr
#'   modis-17A3HGF-061 \tab MODIS Net Primary Production Yearly Gap-Filled \tab hdf, Gpp_500m, Npp_500m, metadata, Npp_QC_500m \cr
#'   modis-09A1-061 \tab MODIS Surface Reflectance 8-Day (500m) \tab hdf, metadata, sur_refl_b01, sur_refl_b02, sur_refl_b03, sur_refl_b04, sur_refl_b05, sur_refl_b06, sur_refl_b07, sur_refl_raz, sur_refl_szen, sur_refl_vzen, sur_refl_qc_500m, sur_refl_state_500m, sur_refl_day_of_year \cr
#'   modis-16A3GF-061 \tab MODIS Net Evapotranspiration Yearly Gap-Filled \tab hdf, ET_500m, LE_500m, PET_500m, PLE_500m, metadata, ET_QC_500m \cr
#'   modis-21A2-061 \tab MODIS Land Surface Temperature/3-Band Emissivity 8-Day \tab hdf, QC_Day, Emis_29, Emis_31, Emis_32, QC_Night, metadata, LST_Day_1KM, LST_Night_1KM, View_Time_Day, View_Angle_Day, View_Time_Night, View_Angle_Night \cr
#'   modis-43A4-061 \tab MODIS Nadir BRDF-Adjusted Reflectance (NBAR) Daily \tab hdf, metadata, Nadir_Reflectance_Band1, Nadir_Reflectance_Band2, Nadir_Reflectance_Band3, Nadir_Reflectance_Band4, Nadir_Reflectance_Band5, Nadir_Reflectance_Band6, Nadir_Reflectance_Band7, BRDF_Albedo_Band_Mandatory_Quality_Band1, BRDF_Albedo_Band_Mandatory_Quality_Band2, BRDF_Albedo_Band_Mandatory_Quality_Band3, BRDF_Albedo_Band_Mandatory_Quality_Band4, BRDF_Albedo_Band_Mandatory_Quality_Band5, BRDF_Albedo_Band_Mandatory_Quality_Band6, BRDF_Albedo_Band_Mandatory_Quality_Band7 \cr
#'   modis-09Q1-061 \tab MODIS Surface Reflectance 8-Day (250m) \tab hdf, metadata, sur_refl_b01, sur_refl_b02, sur_refl_qc_250m, sur_refl_state_250m \cr
#'   modis-14A1-061 \tab MODIS Thermal Anomalies/Fire Daily \tab QA, hdf, MaxFRP, sample, FireMask, metadata \cr
#'   modis-13Q1-061 \tab MODIS Vegetation Indices 16-Day (250m) \tab hdf, metadata, 250m_16_days_EVI, 250m_16_days_NDVI, 250m_16_days_VI_Quality, 250m_16_days_MIR_reflectance, 250m_16_days_NIR_reflectance, 250m_16_days_red_reflectance, 250m_16_days_blue_reflectance, 250m_16_days_sun_zenith_angle, 250m_16_days_pixel_reliability, 250m_16_days_view_zenith_angle, 250m_16_days_relative_azimuth_angle, 250m_16_days_composite_day_of_the_year \cr
#'   modis-14A2-061 \tab MODIS Thermal Anomalies/Fire 8-Day \tab QA, hdf, FireMask, metadata \cr
#'   modis-15A2H-061 \tab MODIS Leaf Area Index/FPAR 8-Day \tab hdf, Lai_500m, metadata, Fpar_500m, FparLai_QC, FparExtra_QC, LaiStdDev_500m, FparStdDev_500m \cr
#'   modis-11A1-061 \tab MODIS Land Surface Temperature/Emissivity Daily \tab hdf, QC_Day, Emis_31, Emis_32, QC_Night, metadata, LST_Day_1km, Clear_day_cov, Day_view_angl, Day_view_time, LST_Night_1km, Clear_night_cov, Night_view_angl, Night_view_time \cr
#'   modis-15A3H-061 \tab MODIS Leaf Area Index/FPAR 4-Day \tab hdf, Lai_500m, metadata, Fpar_500m, FparLai_QC, FparExtra_QC, LaiStdDev_500m, FparStdDev_500m \cr
#'   modis-13A1-061 \tab MODIS Vegetation Indices 16-Day (500m) \tab hdf, metadata, 500m_16_days_EVI, 500m_16_days_NDVI, 500m_16_days_VI_Quality, 500m_16_days_MIR_reflectance, 500m_16_days_NIR_reflectance, 500m_16_days_red_reflectance, 500m_16_days_blue_reflectance, 500m_16_days_sun_zenith_angle, 500m_16_days_pixel_reliability, 500m_16_days_view_zenith_angle, 500m_16_days_relative_azimuth_angle, 500m_16_days_composite_day_of_the_year \cr
#'   modis-10A2-061 \tab MODIS Snow Cover 8-day \tab hdf, metadata, Maximum_Snow_Extent, Eight_Day_Snow_Cover \cr
#'   modis-10A1-061 \tab MODIS Snow Cover Daily \tab hdf, NDSI, metadata, orbit_pnt, granule_pnt, NDSI_Snow_Cover, Snow_Albedo_Daily_Tile, NDSI_Snow_Cover_Basic_QA, NDSI_Snow_Cover_Algorithm_Flags_QA
#' }
#'
#' MODIS data is available for different time intervals (daily, 8-day, 16-day,
#' or yearly) and is provided in 10° x 10° tiles at the equator. The tile grid
#' consists of 36 horizontal (h00 to h35) and 18 vertical (v00 to v17) tiles.
#'
#' @examples
#' \dontrun{
#'   # Retrieve 8-day daytime 1km grid land surface temperature for a bounding box
#'   temp <- get_modis(c(-3, 5, -2, 6), var = "LST_Day_1KM",
#'                     datetime = "2023-11-01/2024-02-28")
#'   plot(temp)
#'
#'   # Retrieve yearly total evapotranspiration for specific coordinates
#'   coords <- cbind(runif(n = 100, -3, -2), runif(n = 100, 5, 6))
#'   et_points <- get_modis(coords, var="ET_500m",
#'                         datetime = "2023-11-01/2024-02-28")
#'   print(et_points)
#' }
#'
#' @seealso \link[terra]{rast}, \link[terra]{aggregate}, \link[terra]{focal}, \link[getsat]{check_modis}
#'
#' @references
#' - Data Use Guidelines for NASA Terra and Aqua MODIS, Suomi NPP, and other Collections.
#' - Avilable at https://modaps.modaps.eosdis.nasa.gov/services/faq/LAADS_Data-Use_Citation_Policies.pdf
#' - Microsoft Planetary Computer. Available at https://planetarycomputer.microsoft.com/
#'
#' @author Abdollah Jalilian
#'
#' @export
get_modis <- function(where,
                      var,
                      datetime,
                      collection=NULL,
                      crop=TRUE,
                      w=NA,
                      agglevel=NULL,
                      download=FALSE,
                      output_dir=tempdir(),
                      clean_dir=FALSE)
{
  message("See 'Data Use Guidelines for NASA Terra and Aqua MODIS, Suomi NPP, and other Collections.'\n",
          "- Available at <https://modaps.modaps.eosdis.nasa.gov/services/faq/LAADS_Data-Use_Citation_Policies.pdf>\n")

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

  # valid geographical boundaries
  if (any(bbox[1] <= -180 | bbox[3] >= 180 |
          bbox[2] <= -90 | bbox[4] >= 90))
    stop("The specified area is outside the valid data coverage region.")

  # validate the variable name: var
  if (!is.character(var) || length(var) != 1)
    stop("'var' must be a character string of length one.")

  if (is.null(collection))
  {
    # get all MODIS collections and variable on Microsoft's Planetary Computer
    collecs_modis <- check_modis()

    # search for the target variable
    idx <- vapply(collecs_modis$vars,
                  function(o) var %in% o, logical(length(var)))
    if (sum(idx) > 0)
    {
      collection <- collecs_modis[idx, c("id", "title", "description")]
      message(paste0("\033[", 32, "m", var, "\033[0m"),
              " has been found in collection(s):\n",
              paste(utils::capture.output(print(unname(collection[, 1:2]))),
                    collapse = "\n"),
              "\n")
      if (nrow(collection) > 1)
      {
        message(paste0("\033[", 34, "m", collection[1, 1], "\033[0m"),
                " (", collection[1, 2],
                ") is selected.\nUse argument 'collection' if you need: ",
                paste(collection[-1, 1], collapse = ", "),
                "\n")
      }
      message(paste("-- Collection description ", strrep("-", 24), "\n",
                    collection[1, 3], "\n", strrep("-", 50), "\n"))
      collection <- collection[1, 1]
    } else{
      stop(var, " was not found in any collection.")
    }

    rm(collecs_modis)
  }

  # set a new timeout value (e.g., 60 seconds)
  oldtimeout <- getOption("timeout")
  options(timeout=3600)

  # attempt to connect to the API
  message("Connecting to the Microsoft Planetary Computer STAC API...\n")
  for (attempt in 1:5)
  {
    items <- tryCatch(
      {
        # Connect to the Microsoft Planetary Computer STAC API
        rstac::stac(
          "https://planetarycomputer.microsoft.com/api/stac/v1"
        ) |>
          # STAC search API
          rstac::stac_search(
            # collection IDs to include in the search for items
            collections = collection,
            # bounding box (xmin, ymin, xmax, ymax) in  WGS84 longitude/latitude
            bbox = bbox,
            # date-time range
            datetime = datetime,
            # maximum number of results
            limit = NULL
          ) |>
          # HTTP GET requests to STAC web services
          rstac::get_request() |>
          # allow access assets from Microsoft's Planetary Computer
          rstac::items_sign(sign_fn=rstac::sign_planetary_computer()) |>
          # fetch all STAC Items
          rstac::items_fetch()
      },
      error = function(e) {
        message(sprintf("Attempt %d failed: %s", attempt, e$message))
        return(NULL)
      }
    )

    if (!is.null(items))
      break # successful

    # exponential delay
    Sys.sleep(1 * 2^(attempt - 1))
    attempt <- attempt + 1
  }

  if (is.null(items))
    stop("Failed to connect to the API after ", 5, " attempts")

  # validate results
  if (length(items$features) == 0)
    stop("No data retrieved. Data may be unavailable for the specified period.")

  ids <- do.call(rbind, lapply(items$features,
                function(o){
                  # Split by "." and extract date and tile
                  parts <- strsplit(o$id, "\\.")[[1]]
                  # date: year and day of the year
                  date <- as.Date(parts[2], format = "A%Y%j")
                  # horizontal tile number, vertical tile number
                  tile <- parts[3]
                  return(data.frame(collection=parts[1], date=date, tile=tile))
                }))
  message("Getting MODIS data \n  tiles: ",
          paste(unique(ids$tile), collapse = ", "), "\n  dates: ",
          paste(unique(ids$date), collapse = ", "), "\n")

  # check HTTP errors
  httperror <- vapply(items$features, function(o) {
    httr::http_error(o$assets[[var]]$href)
  }, logical(1))
  if (any(httperror))
    stop("HTTP request failed for tiles and dates below (status code 4xx):\n",
         paste(utils::capture.output(print(ids[httperror, ])), collapse="\n"))

  if (!download)
  {
    # set temporary directory for terra
    terra::terraOptions(tempdir = output_dir)
    # create the progress bar
    pb <- utils::txtProgressBar(min=0, max=length(items$features), style=3)
    icount <- 0
    # load and process rasters
    rdata <- lapply(items$features, function(o){
      r <- terra::rast(o$assets[[var]]$href)
      if (crop)
      {
        # project the extent to match the raster's CRS
        pbx <- terra::project(terra::ext(bbox, xy=TRUE), "EPSG:4326", terra::crs(r))
        w <- terra::intersect(terra::ext(r), pbx)
        r <- terra::crop(r, pbx)
      }
      # ppdate the progress bar
      icount <<- icount + 1
      utils::setTxtProgressBar(pb, icount)
      return(r)
    })
    # clean up terra temporary files
    terra::tmpFiles(remove = TRUE)
    cat("\n")
  } else{
    if (clean_dir)
      initial_files <- list.files(output_dir, full.names=TRUE)

    # download items for the slected var(s)
    items <- items |>
      rstac::assets_select(asset_names=var) |>
      rstac::assets_download(asset_names = var,
                             items_max=Inf,
                             overwrite=TRUE, output_dir=output_dir)

    # load and process rasters
    rdata <- lapply(items$features, function(o){
      r <- terra::rast(o$assets[[var]]$href)
      if (crop)
      {
        # project the extent to match the raster's CRS
        pbx <- terra::project(terra::ext(bbox, xy=TRUE), "EPSG:4326", terra::crs(r))
        w <- terra::intersect(terra::ext(r), pbx)
        r <- terra::crop(r, pbx)
      }
      return(r)
    })
  }

  # group rasters by date and mosaic them if needed
  rdata <- lapply(split(rdata, ids$date), function(x){
    if (length(x) == 1)
    {
      x[[1]]
    } else{
      # merge raster tiles into a single raster,  if more than one tile
      do.call(terra::mosaic, c(x, fun="mean"))
    }
  })

  # assign dates to reasters
  rdata <- mapply(function(r, d) {
    terra::time(r) <- as.Date(d);
    return(r)
  }, rdata, names(rdata), SIMPLIFY=FALSE)

  # convert list of rasters to one multi-layer raster
  rdata <- terra::rast(rdata)

  # moving window (focal) aggregation by window size w
  if (!is.na(w))
  {
    rdata <- terra::focal(rdata, w=w, fun="mean", na.policy="only", na.rm=TRUE)
  }

  # perform temporal aggregation if specified
  if (!is.null(agglevel))
    rdata <- terra::tapp(rdata, index=agglevel, fun="mean", na.rm=TRUE)

  # projection to EPSG:4326 (World Geodetic System 1984, WGS84)
  rdata <- terra::project(rdata, "EPSG:4326")

  # if 'where' is a matrix/data frame, extract var values for points
  if (inherits(where, c("matrix", "data.frame")))
  {
    where <- data.frame(where)
    rdata <- terra::extract(rdata, where, ID=FALSE)
    names(rdata) <- paste(var, names(rdata), sep="_")
    rdata <- cbind(where, rdata)
  }

  if (download && clean_dir)
  {
    new_files <- setdiff(list.files(output_dir, full.names=TRUE), initial_files)
    # remove only the new files
    file.remove(new_files)
  }

  # restore the original timeout
  options(timeout=oldtimeout)

  return(rdata)
}
