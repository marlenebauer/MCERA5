% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annual_analysis.R
\name{annual_analysis}
\alias{annual_analysis}
\title{Calculate Statistics for annual data at grid-cell level or for area averages}
\usage{
annual_analysis(annual_data, areaAvg = FALSE)
}
\arguments{
\item{annual_data}{Object of class SpatRaster which is already aggregated to annual level. You can use months_aggregate().}

\item{areaAvg}{Logical. If TRUE, the input data must be area averaged. Default FALSE (data must be a raster).}
}
\value{
Mean, median, min, max, sens_slope, mk_tau and mk_pval. List for areaAvg = TRUE or dataframe with x, y, all values and statistics for areaAvg = FALSE.
}
\description{
Calculate Statistics for annual data at grid-cell level or for area averages
}
