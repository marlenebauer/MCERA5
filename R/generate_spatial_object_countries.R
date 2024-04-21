#' Get the geometry of a selected country
#'
#'This function returns the geometry of a country chosen from the rgeoboundaries package  .\cr
#'The user can specify the country name and optionally the administrative level. \cr
#'For details view the documentation of the rgeoboundaries package (https://github.com/wmgeolab/rgeoboundaries).
#'
#' @param country_name A character. The name of the country for which the geometry is to be extracted.
#'
#' @param admin_level Integer. The administrative level of the country. If not specified, the function will return the geometry of the country.
#' @import rgeoboundaries
#' @return  A sfc Multipolygon containing the geometry of the selected country.
#' @export
#'
#'
generate_spatial_obj <- function(country_name, admin_level = NULL) {
  if (is.null(admin_level)) {
    country <- geoboundaries(country_name)
  } else {
    country <- geoboundaries(country_name, admin_level)
  }
  if (nrow(country) == 0) {
    stop(paste("Country", country_name, "not found. Please provide a valid country name."))
  }
  return(country$geometry)
}

