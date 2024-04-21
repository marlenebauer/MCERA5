#' Analysis of monthly area averages
#' This function calculates area average for each time step, groups data by months and calculates statistics over time period of data set.
#' Calculated parameters are mean, median, min, max, Sen Slope, Mann-Kendall tau and Mann-Kendall p-value.

#' @param data Object of class SpatRaster.
#' Data can be monthly or monthly averaged by hour of day with one or more hours selected.
#' @import terra
#' @import dplyr
#' @import tidyr
#' @import trend
#' @import Kendall
#' 
#' @return A data frame with the following columns: months, mean, median, min, max, slope, mk_tau and mk_pval of each month over time for the area average of the data.
#' @export
#'
area_analysis <- function(data){
  # Call convert_units() 
  data <- MCERA5::convert_units(data)
  # Convert data to data frame
  df <- as.data.frame(data, xy = TRUE, time=TRUE)
  # Create an empty data frame to store the area averages
  areaAvg_df <- data.frame()
  # Remove x and y coordinates from data frame
  df <- df[!(colnames(df) %in% c("x", "y"))]
  # Calculate the area average for each time step
  area_average <- as.numeric(round(apply(df, 2, mean), 4))
  # Extract time from the column names of the input dataframe
  time <- colnames(df)
  time <- format(as.POSIXct(time), format = "%Y-%m-%d %H:%M:%S")
  # Extract years, months, days and hours from the time component
  years <-format(as.POSIXct(time), format = "%Y")
  months <-format(as.POSIXct(time), format = "%m")
  days <-format(as.POSIXct(time), format = "%d")
  hours <-format(as.POSIXct(time), format = "%H")
  
  # Check if the input data set is monthly
  if (length(months) > (length(unique(years))*length(unique(months))*length(unique(hours)))){
    stop("Not a monthly dataset. Please choose a monthly dataset for this analysis.")
  }
  
  # Check if the input data set is monthly with only one hour
  else if (length(months) <= length(unique(years))*length(unique(months))){
    areaAvg_df <- as.data.frame(cbind(time, years, months, days, hours, area_average)) 
    trend_results <- areaAvg_df %>%
      # Group by months
      group_by(months) %>%
      summarize(
        mean = mean(as.numeric(area_average)),
        median = median(as.numeric(area_average)),
        min = min(as.numeric(area_average)),
        max = max(as.numeric(area_average)),
        slope = {
          result <- sens.slope(as.numeric(area_average))
          as.numeric(result$estimates)
        },
        mk_tau = {
          result_MK <- MannKendall(as.numeric(area_average))
          as.numeric(result_MK$tau)
        },
        mk_pval = {
          result_MK <- MannKendall(as.numeric(area_average))
          as.numeric(result_MK$sl)
        }
        , .groups = 'drop'
      )
  }
  # If data set is monthly by hour of day (with more than one hour selected)
  else{
    areaAvg_df <- as.data.frame(cbind(time, years, months, days, hours, area_average)) 
    trend_results <- areaAvg_df %>%
      # Group by months and hours
      group_by(months, hours) %>%
      summarise(
        mean=mean(as.numeric(area_average)),
        median = median(as.numeric(area_average)),
        min = min(as.numeric(area_average)),
        max = max(as.numeric(area_average)),
        slope = {
          result <- sens.slope(as.numeric(area_average))
          as.numeric(result$estimates)
        },
        mk_tau = {
          result_MK <- MannKendall(as.numeric(area_average))
          as.numeric(result_MK$tau)
        },
        mk_pval = {
          result_MK <- MannKendall(as.numeric(area_average))
          as.numeric(result_MK$sl)
        }
        , .groups = 'drop'
      )
  }
  return(trend_results)
}

