#' Rasterize and mask the ncfile downloaded by the ERA5 function
#'
#' This function takes the downloaded ncfile by the ERA5 function and converts it to a SpatRaster.\cr
#' In addition, the function will crop the raster to the extent of the spatial object specified in the ERA5 function.
#'
#' @param aoi The Area of interest provided by the user in the get_ERA5 function.\cr
#' This can be an sf, sfc, sfg, SpatRaster or SpatVector object or a country name selected from the rgeoboundaries package.
#' @param ncfile The downloaded ncfile ("netcdf" format) from the Copernicus Climate Data Store by the ERA5 function.\cr
#' @import terra
#' @import sf
#' @return A SpatRaster object that has been cropped to the extent of the provided aoi.
#' @export
#'
rasterize_and_mask <- function(ncfile, aoi) {
  # Rasterize the input raster
  rasterized <- rast(ncfile)

  # Check if spatial_obj is of class sf, sfc, or sfg
  if (inherits(aoi, "sf") || inherits(aoi, "sfc") || inherits(aoi, "sfg")) {
    # Transform the spatial object to the raster CRS
    aoi <- st_transform(aoi, st_crs(rasterized))
    # same for when spatial_obj is raster data
  } else if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
    # Transform the spatial object to the raster CRS
    aoi <- project(aoi, crs(rasterized))
  }
  # Convert sfc or sfg to sf (so that mask function works)
  if (inherits(aoi, "sfc") || inherits(aoi, "sfg")){
    aoi <- st_sf(aoi)
  }

  # Crop the rasterized raster using the provided bounding box
  masked_raster <- mask(rasterized, aoi)
  return(masked_raster)
}
