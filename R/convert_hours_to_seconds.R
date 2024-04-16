#' Convert hours to seconds for the timeout parameter in the ERA5 function
#'
#' This function allows you to use hours or seconds as input for the timeout parameter in the \code{\link{era5}} function.
#' By default, you can only use seconds as input for the timeout parameter. This function allows you to use hours as input as well.
#' The timeout parameter is used to set the maximum time to wait for the data to be downloaded from the Copernicus Climate Data Store.
#
#' @param time_out Character string indicating the time to wait for the data to be downloaded. \cr
#' It can be in hours (ending with 'h') or seconds (ending with 's'). \cr
#'
#' @return The time in seconds as input for the timeout parameter.
#' @export
#'
#' @examples
#' time_out <- "3h"
#' time_out <- convert_hours_to_seconds(time_out)
#' print(time_out)
#' '10800'
#'
convert_hours_to_seconds <- function(time_out) {
  if (grepl("h$", time_out)) {  # Check if the input ends with 'h' (indicating hours)
    hours <- as.numeric(sub("h$", "", time_out))  # Extract the numeric part
    seconds <- hours * 3600  # Convert hours to seconds
    return(seconds)  # Return the calculated time in seconds
  } else if (grepl("s$", time_out)) {  # Check if the input ends with 's' (indicating seconds)
    seconds <- as.numeric(sub("s$", "", time_out))  # Extract the numeric part
    return(seconds)  # Return the provided time in seconds
  } else {
    stop("Input must be in hours (ending with 'h') or seconds (ending with 's').")
  }
}
