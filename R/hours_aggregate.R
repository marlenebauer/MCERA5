#' Aggregate hourly raster data to daily data
#' This function aggregates hourly raster data to daily data by calculating the mean or sum for each day.
#'
#' @param data Object of class SpatRaster.
#' Data must be hourly. If not all 24 hours of a day are provided, only the provided hours will be considered in the aggregation
#' @param aggr Method to aggregate the data. Can be mean() or sum().
#' @param areaAvg Logical. If TRUE, the function returns the daily sum() or mean() of the area average of the raster data. Default FALSE.
#' @import terra
#' @import dplyr
#' @import tidyr
#'
#' @return Object of class SpatRaster with the daily mean or sum at grid-cell level or for the area average.
#' @export
#'
hours_aggregate <- function(data, aggr, areaAvg=FALSE){
  # Call convert_units()
  data <- MCERA5::convert_units(data)
  # Extract dates and hours from the time component
  dates <- unique(format(as.Date(time(data)), format = "%Y-%m-%d"))
  hours <- unique(format(as.POSIXct(time(data)), format = "%H"))
  # Check, if days are complete
  num_layers <- nlyr(data)
  if (num_layers %% 24 != 0) {
    warning("Incomplete days detected. Aggregating to daily mean or sum is done with provided hours. Please provide all hours for each day to compute daily mean or sum including all hours of each day.")
  }

  # Calculate number of days
  num_days <- num_layers / length(hours)
  # Subset raster into the daily layers
  daily_layers <- lapply(1:num_days, function(day) {
    start_idx <- (day - 1) * length(hours) + 1
    end_idx <- day * length(hours)
    return(data[[start_idx:end_idx]])
  })

  # Calculate the mean and sum for each raster cell for each day
  if (aggr=="mean"){
    daily_mean <- rast(lapply(daily_layers, function(x) mean(x)))
    time(daily_mean) <- as.Date(dates)
    # Return area average if TRUE
    if (areaAvg==TRUE){
      daily_mean <- global(as.numeric(daily_mean), "mean", na.rm=TRUE)
      daily_mean$time <- as.Date(dates)
    }
    return(daily_mean)
  }
  else if (aggr=="sum"){
    daily_sum <- rast(lapply(daily_layers, function(x) sum(x)))
    time(daily_sum) <- as.Date(dates)
    # Return area average if TRUE
    if (areaAvg==TRUE){
      daily_sum <- global(as.numeric(daily_sum), "mean", na.rm=TRUE)
      daily_sum$time <- as.Date(dates)
    }
    return(daily_sum)
  }
}
