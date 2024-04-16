#' Get the months of a season as input for the ERA5 function
#'
#'This function returns the months of a season specified by the user. \cr
#'These months are used as input for the ERA5 function to download the data for the specified season.\cr
#'Available Seasons are: \cr
#'- DJF ( December, January, February) \cr
#'- MAM (March, April, May) \cr
#'- JJA (June, July, August) \cr
#'- SON (September, October, November)
#' @param season Character string. The season for which the months are required. Available inputs are "DJF", "MAM", "JJA", and "SON".
#' @return A list of months for the specified season
#' @export
#'
#' @examples
#' season <- "DJF"
#' months_season <- seasonal_data(season)
#' print(months_season)
#' [1] "12" "01" "02"
#'
# function to get the months of a season
get_seasonal_data <- function(season){
  if(season == "DJF"){
    target_months <- c("12", "01", "02")
  } else if(season == "MAM"){
    target_months <- c("03", "04", "05")
  } else if(season == "JJA"){
    target_months <- c("06", "07", "08")
  } else if(season == "SON"){
    target_months <- c("09", "10", "11")
  } else {
    stop("Invalid season. Please enter one of: 'DJF', 'MAM', 'JJA', 'SON'")
  }
  return(target_months)
}

