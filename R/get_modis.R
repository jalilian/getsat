#' Retrive MODIS Data
#'
#' @details
#'
#' The following table provides information on available MODIS collections and
#' their corresponding variables:
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
#'
get_modis <- function(where,
                      var,
                      datetime,
                      output_dir=tempdir(),
                      clean_dir=FALSE)
{library(rstac)

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

  # extract id and title of collections
  collecs_modis <- do.call(rbind, lapply(collecs$collections, function(o) {
    data.frame(id = o$id, title = o$title, stringsAsFactors=FALSE)
  }))
  # filter MODIS collections
  idx <- grepl("^modis", collecs_modis$id)
  if (!any(idx))
    stop("No MODIS collections found.")
  collecs_modis <- collecs_modis[idx, ]

  # extract available variables
  collecs_modis$vars <- lapply(collecs$collections[idx], function(o) names(o$item_assets))


  var <- "LST_Day_1KM"

  # search for the target variable
  idx <- vapply(collecs_modis$vars, function(o) var %in% o, logical(1))
  if (sum(idx) > 0)
  {
    found_in <- collecs_modis$id[idx]
    message(var, " has been found in collection(s): ", paste(found_in, collapse = ", "))
  } else{
    stop(var, " was not found in any collection.")
  }



  items <- rstac::stac(
    "https://planetarycomputer.microsoft.com/api/stac/v1"
  ) |>
    # STAC search API
    rstac::stac_search(
      # collection IDs to include in the search for items
      collections = cc,
      # bounding box (xmin, ymin, xmax, ymax) in  WGS84 longitude/latitude
      bbox = bbox,
      # maximum number of results
      #limit = 999
    ) |>
    # HTTP GET requests to STAC web services
    rstac::get_request() |>
    # allow access assets from Microsoft's Planetary Computer
    rstac::items_sign(sign_fn=rstac::sign_planetary_computer()) |>
    # fetch all STAC Items
    rstac::items_fetch()

  modis_collections <- c(
    "modis-64A1-061", "modis-17A2H-061", "modis-11A2-061", "modis-17A2HGF-061",
    "modis-17A3HGF-061", "modis-09A1-061", "modis-16A3GF-061", "modis-21A2-061",
    "modis-43A4-061", "modis-09Q1-061", "modis-14A1-061", "modis-13Q1-061",
    "modis-14A2-061", "modis-15A2H-061", "modis-11A1-061", "modis-15A3H-061",
    "modis-13A1-061", "modis-10A2-061", "modis-10A1-061"
  )

  # retrieve DEM tiles from Microsoft Planetary Computer
  items <- rstac::stac(
    "https://planetarycomputer.microsoft.com/api/stac/v1"
  ) |>
    # STAC search API
    rstac::stac_search(
      # collection IDs to include in the search for items
      collections = paste0("cop-dem-glo-", res),
      # bounding box (xmin, ymin, xmax, ymax) in  WGS84 longitude/latitude
      #bbox = bbox,
      # maximum number of results
      #limit = 999
    ) |>
    # HTTP GET requests to STAC web services
    rstac::get_request() |>
    # allow access assets from Microsoft's Planetary Computer
    rstac::items_sign(sign_fn=rstac::sign_planetary_computer()) |>
    # fetch all STAC Items
    rstac::items_fetch() |>
    # download items
    rstac::assets_download(asset_names="data",
                           overwrite=TRUE,
                           output_dir=output_dir)


  items <- stac(
    "https://planetarycomputer.microsoft.com/api/stac/v1") %>%
    # STAC search API
    stac_search(
      # collection IDs to include in the search for items
      collections = collections,
      # bounding box (xmin, ymin, xmax, ymax) in  WGS84 longitude/latitude
      bbox = bbox,
      # date-time range
      datetime = datetime,
      # maximum number of results
      limit = 999
    ) %>%
    # HTTP GET requests to STAC web services
    get_request() %>%
    # allow access assets from Microsoft's Planetary Computer
    items_sign(sign_fn=sign_planetary_computer()) %>%
    # fetch all STAC Items
    items_fetch()

}
