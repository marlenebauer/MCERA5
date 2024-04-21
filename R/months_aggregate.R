#' Aggregate monthly raster data to annual data
#' This function aggregates monthly raster data to annual data by calculating the mean or sum for each year.
#'
#' @param data Object of class SpatRaster.
#' Data can be monthly or monthly averaged by hour of day with only one hour selected.
#' If target_months or seasons were used to download the data, only these months will be considered in the aggregation
#' @param aggr Method to aggregate the data. Can be mean() or sum().
#' @param areaAvg Logical. If TRUE, the function returns the annual sum() or mean() of the area average of the raster data. Default FALSE.
#' @import terra
#' @import dplyr
#' @import tidyr
#'
#' @return Object of class SpatRaster with the annual mean or sum at grid-cell level or for the area average.
#' @export
#'
months_aggregate <- function(data, aggr, areaAvg =FALSE) {
  # Call convert_units()
  data <- MCERA5::convert_units(data)
  # Check if time attribute of raster is present in the input data
  if (is.null(time(data))) {
    stop("Time attribute not found.")
  }
  # Extract hours and check if only one hour is present in the dataset
  hours <- format(as.POSIXct(time(data)), format = "%H")
  if (length(unique(hours)) != 1){
    stop("More than one hour per month selected. Please choose a dataset which only contains one hour per day/month.")
  }
  # Calculate number of layers (= number of timesteps) in raster data
  num_layers <- nlyr(data)
  # Check, if years are complete
  if (num_layers %% 12 != 0) {
    warning("Incomplete years detected. Aggregating to annual mean or sum is done with provided months
            Please provide all months for each year to compute annual mean or sum including all months of each year.")
  }

  years <- unique(format(as.Date(time(data)), format = "%Y"))
  # Create a list to store rasters for each year
  annual_layers <- list()
  # Loop through each unique year
  for (year in years) {
    # Subset the raster for the current year
    subset_data <- subset(data, format(as.Date(time(data)), "%Y") == year)

    # Store the subset raster in the list
    annual_layers[[year]] <- subset_data
  }

  # Calculate the mean or sum for each raster cell for each year
  if (aggr=="mean"){
    annual_mean <- rast(lapply(annual_layers, function(x) mean(x)))
    time(annual_mean) <- as.Date(paste0(years, "-01-01"))
    # Return area average if TRUE
    if (areaAvg==TRUE){
      annual_mean <- global(as.numeric(annual_mean), "mean", na.rm=TRUE)
    }
    return(annual_mean)
  }
  else if (aggr=="sum"){
    annual_sum <- rast(lapply(annual_layers, function(x) sum(x)))
    time(annual_sum) <- as.Date(paste0(years, "-01-01"))
    # Return area average if TRUE
    if (areaAvg==TRUE){
      annual_sum <- global(as.numeric(annual_sum), "sum", na.rm=TRUE)
    }
    return(annual_sum)
  }
}
