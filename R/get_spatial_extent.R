#' Get the extent of any sf or terra object as input for the ERA5 function
#'
#' The function extracts the extent for any sf or terra object with rounded coordinates to the grid size specified by the user.\cr
#' The ERA5 data will be downloaded based on this extent.\cr
#' The function will return the extent in the desired format for the API: "North/West/South/East" (e.g., "51.75/7.25/51.5/7.5"). \cr
#' The function will also transform the spatial_object to the CRS of the ERA5 data (World Geodetic System 1984, EPSG: 4326) if it is not already in this CRS.
#' @import terra
#' @import sf
#'
#' @param aoi The spatial object provided by the user in the ERA5 function
#' @param grid The grid size to which the extent should be rounded (e.g. "0.25/0.25")
#'
#'
#' @return A character string with the extent of the aoi in the format "North/West/South/East" as required by the API-
#' @export
get_extent_rounded_2_grid <- function(aoi, grid) {
  grid_size <- as.numeric(unlist(strsplit(grid, "/", fixed = TRUE))[1]) # grid_size will be 0.25 e.g.(need a numeric value)

  # Check if spatial_obj has a defined CRS and if it matches the desired CRS
  if (inherits(aoi, "sf") || inherits(aoi, "sfc") || inherits(aoi, "sfg")) {
    if (is.null(st_crs(aoi)) || !identical(st_crs(aoi)$proj4string, st_crs("+proj=longlat +datum=WGS84 +no_defs"))) {
      aoi <- st_transform(aoi, st_crs("+proj=longlat +datum=WGS84 +no_defs"))  # Convert to desired CRS
    }
  } else if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
    if (is.null(crs(aoi)) || !identical(crs(aoi), crs("+proj=longlat +datum=WGS84 +no_defs"))) {
      aoi <- project(aoi, crs("+proj=longlat +datum=WGS84 +no_defs"))  # Convert to desired CRS
    }
  } else {
    stop("Unsupported spatial object type. Please provide an sf, sfc, sfg, SpatRaster, or SpatVector object.")
  }

  # Calculate the extent rounded to grid size
  if (inherits(aoi, "sf") || inherits(aoi, "sfc") || inherits(aoi, "sfg")) {
    extent_vals <- st_bbox(aoi)
    extent_vals <- ceiling(extent_vals / grid_size) * grid_size # will be rounded up to the nearest multiple of the grid size
    extent_string <- paste(extent_vals[c(4, 1, 2, 3)], collapse = "/")  # Reordered values for "North, West, South, East"
    return(extent_string)
  } else if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
    extent_vals <- ext(aoi)
    extent_vals <- ceiling(extent_vals / grid_size) * grid_size # will be rounded up to the nearest multiple of the grid size
    extent_string <- paste(extent_vals[c(4, 1, 2, 3)], collapse = "/")  # Reordered values for "North, West, South, East"
    return(extent_string)
  } else {
    stop("Unsupported spatial object type. Please provide an sf, sfc, sfg, SpatRaster, or SpatVector object.")
  }
}
