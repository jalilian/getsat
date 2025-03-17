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
#' @param clean_dir Logical, whether to clean the output directory before downloading new files. Default is FALSE.
#'
#' @details
#'
#' This function allows users to retrieve MODIS satellite data for specified
#' geographic regions and time periods. MODIS provides various environmental
#' datasets, such as land surface temperature, vegetation indices, burned areas,
#' and primary productivity. The following table provides information on available
#' MODIS collections and their corresponding variables:
#'
#' **Important Note:**
#' MODIS data are provided by NASA LP DAAC at the USGS EROS Center (producer, licensor, processor)
#' and Microsoft (host, processor). Users are required to follow the license and
#' terms of use specified by NASA and Microsoft. Failure to do so may violate
#' the data usage policies.
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
#' or yearly) and is provided in 10° x 10° tiles at the equator.
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
      min(where[, 1]) - 0.15, min(where[, 2]) - 0.15,
      max(where[, 1]) + 0.15, max(where[, 2]) + 0.15
    )
  } else {
    stop("'where' must be a numeric vector of length 4 (bounding box) or a matrix/data.frame with two columns (longitude, latitude).")
  }

  # validate bounding box format
  if (bbox[1] >= bbox[3] || bbox[2] >= bbox[4])
    stop("Bounding box must be in the format c(xmin, ymin, xmax, ymax) with valid coordinates.")

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
      collection <- collecs_modis[idx, c("id", "title")]
      message(var, " has been found in collection(s):\n",
              paste(capture.output(print(collection)), collapse = "\n"),
              "\n")
      if (length(collection) > 1)
      {
        message(collection[1, 1], " (", collection[1, 2],
                ") is selected.\nUse argument 'collection' if you need: ",
                paste(collection[-1, 1], collapse = ", "),
                "\n")
        collection <- collection[1, 1]
      }
    } else{
      stop(var, " was not found in any collection.")
    }
  }


  # Connect to the Microsoft Planetary Computer STAC API
  items <- rstac::stac(
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
  message("Getting MODIS tiles: ",
          paste(names(table(ids$tile)), collapse = ", "), "\n")

  if (!download)
  {
    # create the progress bar
    pb <- txtProgressBar(min=0, max=length(items$features), style=3)
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
      setTxtProgressBar(pb, icount)
      return(r)
    })
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
    rdata <- data.frame(where, rdata)
  }

  if (download && clean_dir)
  {
    new_files <- setdiff(list.files(output_dir, full.names=TRUE), initial_files)
    # remove only the new files
    file.remove(new_files)
  }

  return(rdata)
}
