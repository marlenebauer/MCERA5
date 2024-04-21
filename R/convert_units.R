#' Function to convert units to more common units
#'
#' This function converts values and units from data sets which are in Kelvin (to °C), meters (to mm), meters of water equivalent (to mm of water equivalent), and Pascals (to hPa).
#'
#' @param data Object of class SpatRaster.
#' @import terra
#'
#' @return An object of class SpatRaster with converted units if units are in the list of units to be converted
#' @export
#'
convert_units <- function(data) {
  # Convert units from K to °C
  if (unique(units(data)) == "K") {
    data <- data - 273.15
    units(data) <- "°C"
    # Convert units from m to mm
  } else if (unique(units(data)) == "m") {
    data <- data * 1000
    units(data) <- "mm"
    # Convert units from m of water equivalent to mm of water equivalent
  } else if (unique(units(data)) == "m of water equivalent") {
    data <- data * 1000
    units(data) <- "mm of water equivalent"
    # Convert units from Pa to hPa
  } else if (unique(units(data)) == "Pa") {
    data <- data / 100
    units(data) <- "hPa"
  }
  return(data)
}
