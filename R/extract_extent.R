#' Get the extent of any sf or terra object
#'
#' `myfunction` 'The function extracts the extent for any sf or terra object with rounded coordinates depending on the grid size specified by the user.
#'     The function will return the extent in the format "North/West/South/East" (e.g., "51.75/7.25/51.5/7.5").
#'     The function will also transform the spatial_object to the desired CRS of the Era5 data if it is not already in the desired CRS.
#'

#'
#' @param spatial_obj Any spatial object of class sf, sfc, sfg (sf package) or SpatRaster or SpatVector (terra package)
#' @param grid The grid size to which the extent should be rounded (e.g. "0.25/0.25")
#'
#' @import sf
#' @import terra
#'
#' @return A character string with the extent in the format "North/West/South/East" (e.g., "51.75/7.25/51.5/7.5")
#' @examples
#'
#'
#' myfunction(1) # returns 1
#'
#' @export
get_extent_rounded_2_grid <- function(spatial_obj, grid) {
  grid_size <- as.numeric(unlist(strsplit(grid, "/", fixed = TRUE))[1]) # grid_size will be 0.25 e.g.(need a numeric value)

  # Check if spatial_obj has a defined CRS and if it matches the desired CRS
  if (inherits(spatial_obj, "sf") || inherits(spatial_obj, "sfc") || inherits(spatial_obj, "sfg")) {
    if (is.null(st_crs(spatial_obj)) || !identical(st_crs(spatial_obj)$proj4string, st_crs("+proj=longlat +datum=WGS84 +no_defs"))) {
      spatial_obj <- st_transform(spatial_obj, st_crs("+proj=longlat +datum=WGS84 +no_defs"))  # Convert to desired CRS
    }
  } else if (inherits(spatial_obj, "SpatRaster") || inherits(spatial_obj, "SpatVector")) {
    if (is.null(crs(spatial_obj)) || !identical(crs(spatial_obj), crs("+proj=longlat +datum=WGS84 +no_defs"))) {
      spatial_obj <- project(spatial_obj, crs("+proj=longlat +datum=WGS84 +no_defs"))  # Convert to desired CRS
    }
  } else {
    stop("Unsupported spatial object type. Please provide an sf, sfc, sfg, SpatRaster, or SpatVector object.")
  }

  # Calculate the extent rounded to grid size
  if (inherits(spatial_obj, "sf") || inherits(spatial_obj, "sfc") || inherits(spatial_obj, "sfg")) {
    extent_vals <- st_bbox(spatial_obj)
    extent_vals <- ceiling(extent_vals / grid_size) * grid_size # will be rounded up to the nearest multiple of the grid size
    extent_string <- paste(extent_vals[c(4, 1, 2, 3)], collapse = "/")  # Reordered values for "North, West, South, East"
    return(extent_string)
  } else if (inherits(spatial_obj, "SpatRaster") || inherits(spatial_obj, "SpatVector")) {
    extent_vals <- ext(spatial_obj)
    extent_vals <- ceiling(extent_vals / grid_size) * grid_size # will be rounded up to the nearest multiple of the grid size
    extent_string <- paste(extent_vals[c(4, 1, 2, 3)], collapse = "/")  # Reordered values for "North, West, South, East"
    return(extent_string)
  } else {
    stop("Unsupported spatial object type. Please provide an sf, sfc, sfg, SpatRaster, or SpatVector object.")
  }
}
usethis::use_roxygen_md()
devtools::document()
