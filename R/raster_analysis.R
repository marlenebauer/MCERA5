#' Monthly analysis at grid-cell level
#'
#' This function groups input data by months and calculates statistics over time period of data set.
#' Calculated parameters are mean, median, min, max, Sen Slope, Mann-Kendall tau and Mann-Kendall p-value.
#'
#'
#' @param data Object of class SpatRaster.
#' Data can be monthly or monthly averaged by hour of day with only one hour selected.
#' If target_months or seasons were used to download the data, only these months will be considered in the analysis.
#' @import terra
#' @import sf
#' @import dplyr
#' @import tidyr
#' @import trend
#' @import Kendall
#'
#' @return A data frame with the following columns: x-coordinate, y-coordinate, months, mean, median, min, max, slope, mk_tau and mk_pval of each month over time at grid-cell level.
#' Data can be plotted with monthly_plots() from MCERA5.
#' @export
#'
raster_analysis <- function(data){
  # Call convert_units()
  data <- MCERA5::convert_units(data)
  # Check if time attribute of raster is present in the input data
  if (is.null(time(data))) {
    stop("Time attribute not found.")
  }
  # Extract hours and check if only one hour is present in the dataset
  hours <- format(as.POSIXct(time(data)), format = "%H")
  if (length(unique(hours)) != 1){
    stop("More than one hour per day/month selected. Please choose a dataset which only contains one hour per day/month.")
  }
  # Check if the input dataset is monthly
  months <-format(as.Date(time(data)), format = "%m")
  years <-format(as.Date(time(data)), format = "%Y")
  if (length(months) > length(unique(years))*length(unique(months))){
    stop("Not a monthly dataset. Please choose a monthly dataset for this analysis.")
  }
  # Convert spatial raster to data frame, keep coordinates and time
  df <- as.data.frame(data, xy=TRUE, time=TRUE)
  # Convert to long table format
  df <- pivot_longer(df, cols = -c(x, y), names_to = "month")
  # Combine months and data frame
  df <- cbind(months, df)
  # Group by x, y and months and calculate statistics over time
  raster_results <- df %>%
    group_by(x, y, months)%>%
    summarize(mean = mean(value),
              median = median(value),
              min = min(value),
              max = max(value),
              slope = {
                result <- sens.slope(value)
                as.numeric(result$estimates)
              },
              mk_tau = {
                result_MK <- MannKendall(value)
                as.numeric(result_MK$tau)
              },
              mk_pval = {
              result_MK <- MannKendall(value)
              as.numeric(result_MK$sl)
              }
              , .groups = 'drop'
    )
  return(raster_results)
}
