#' Calculate Statistics for annual data at grid-cell level or for area averages
#'
#' @param annual_data Object of class SpatRaster which is already aggregated to annual level. You can use months_aggregate().
#' @param areaAvg Logical. If TRUE, the input data must be area averaged. Default FALSE (data must be a raster).
#' @import terra
#' @import dplyr
#' @import tidyr
#' @import trend
#' @import Kendall
#'
#' @return Mean, median, min, max, sens_slope, mk_tau and mk_pval. List for areaAvg = TRUE or dataframe with x, y, all values and statistics for areaAvg = FALSE.
#' @export
annual_analysis <- function(annual_data, areaAvg=FALSE) { #annual_mean or annual_sum
  if (areaAvg==TRUE){
    if (ncol(annual_data)>1){
      stop("Please provide an area-averaged annual dataset for analysis.")
    } else if (colnames(annual_data)=="mean"){
      slope = {
        result_slope <- sens.slope(as.numeric(annual_data$mean))
        as.numeric(result_slope$estimates)
      }
      mk_tau = {
        result_MK <- MannKendall(as.numeric(annual_data$mean))
        as.numeric(result_MK$tau)
      }
      mk_pval = {
        result_MK <- MannKendall(as.numeric(annual_data$mean))
        as.numeric(result_MK$sl)
      }
      # Create result list and assign names
      result <- list(slope = slope, tau = mk_tau, p_value = mk_pval)
      names(result) <- c("sen_slope", "kendall_tau", "kendall_pval")
    } else if (colnames(annual_data)=="sum"){
      slope = {
        result <- sens.slope(as.numeric(annual_data$sum))
        as.numeric(result$estimates)
      }
      mk_tau = {
        result_MK <- MannKendall(as.numeric(annual_data$sum))
        as.numeric(result_MK$tau)
      }
      mk_pval = {
        result_MK <- MannKendall(as.numeric(annual_data$mean))
        as.numeric(result_MK$sl)
      }
      # Create result list and assign names
      result <- list(slope = slope, tau = mk_tau, p_value = mk_pval)
      names(result) <- c("sen_slope", "kendall_tau", "kendall_pval")
    }
    return(result)
  }
  else {
    years <- unique(format(as.Date(time(annual_data)), format = "%Y"))
    if (nlyr(annual_data)>length(years)){
      stop("Please provide an annual raster dataset for annual raster analysis.")
    }
    # Convert to data frame
    df <- as.data.frame(annual_data, xy=TRUE)
    # Extract coordinates and remove them from the data frame for the analysis
    coords <- df[, (1:2)]
    df <- df[, -(1:2)]
    # Calculate Sen's slope and Mann-Kendall test for each grid cell over the years
    slope <- sapply(apply(df, 1, sens.slope), function(x) as.numeric(gsub("[^0-9.-]", "", x))[1])
    mk_tau <- sapply(apply(df, 1, MannKendall), function(x) as.numeric(gsub("[^0-9.-]", "", x))[1])
    mk_pval <- sapply(apply(df, 1, MannKendall), function(x) as.numeric(gsub("[^0-9.-]", "", x))[2])
    df <- cbind(coords, df, slope, mk_tau, mk_pval)
    return(df)
  }
}

