#' Rasterize and mask the ncfile downloaded by the ERA5 function
#'
#' This function takes the downloaded ncfile by the ERA5 function and converts it to a SpatRaster.\cr
#' In addition, the function will crop the raster to the extent of the spatial object specified in the ERA5 function.
#'
#' @param ncfile The downloaded ncfile ("netcdf" format) from the Copernicus Climate Data Store by the ERA5 function.\cr
#' @param spatial_obj The spatial object specified in the ERA5 function to crop the raster to
#' @return A SpatRaster object that has been cropped to the extent of the provided spatial object
#' @export
#'
#' @examples
#' ncfile <- "2m_temperature_2020-01-18__2020-01-30.nc"
#' spatial_obj <- st_read("Germany.shp")
#' masked_and_rasterized_data <- rasterize_and_mask(ncfile, spatial_obj)
#'
#'
rasterize_and_mask <- function(ncfile, spatial_obj) {
  # Rasterize the input raster
  rasterized <- rast(ncfile)

  # Check if spatial_obj is of class sf, sfc, or sfg
  if (inherits(spatial_obj, "sf") || inherits(spatial_obj, "sfc") || inherits(spatial_obj, "sfg")) {
    # Transform the spatial object to the raster CRS
    spatial_obj <- st_transform(spatial_obj, st_crs(rasterized))
    # same for when spatial_obj is raster data
  } else if (inherits(spatial_obj, "SpatRaster") || inherits(spatial_obj, "SpatVector")) {
    # Transform the spatial object to the raster CRS
    spatial_obj <- project(spatial_obj, crs(rasterized))
  }
  # Convert sfc or sfg to sf (so that mask function works)
  if (inherits(spatial_obj, "sfc") || inherits(spatial_obj, "sfg")){
    spatial_obj <- st_sf(spatial_obj)
  }

  # Crop the rasterized raster using the provided bounding box
  masked_raster <- mask(rasterized, spatial_obj)
  return(masked_raster)
}
