#' Extract months for climatological seasons4
#'
#' This function extracts the months for the climatological seasons that will be passed to the get_era5 function as target_months.\cr
#' The climatological seasons are defined as follows:\cr
#' - DJF: December, January, February\cr
#' - MAM: March, April, May\cr
#' - JJA: June, July, August\cr
#' - SON: September, October, November\cr
#'
#'
#' @param season A character string or vector of climatological seasons. It can be one or more of the following: 'DJF', 'MAM', 'JJA', 'SON'
#'
#' @return A character vector of months corresponding to the climatological seasons
#' @export
#'
get_seasonal_data <- function(season) {
  # If invalid seasons are provided, stop the function and return an error message
  valid_seasons <- c("DJF", "MAM", "JJA", "SON")
  if (any(!season %in% valid_seasons)) {
    stop("Invalid season(s). Please enter one or more of: 'DJF', 'MAM', 'JJA', 'SON'")
  }
  # List of months for each season
  month_mapping <- list(
    DJF = c("12", "01", "02"),
    MAM = c("03", "04", "05"),
    JJA = c("06", "07", "08"),
    SON = c("09", "10", "11")
  )
  # extract the target months based on the seasons
  # It is possible to select multiple seasons
  target_months <- unlist(lapply(season, function(season) month_mapping[[season]]))

  return(unique(target_months))
}

