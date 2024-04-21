#' Plot analysis results for monthly raster data
#'
#' @param data Dataframe which contains statistics grouped by month. Monthly data can be analysed with raster_analysis.
#' @param data_col Column name of the data to plot.Can be mean, median, min, max, slope, mk_tau or mk_pval.
#' @param output_file Output file name. Should include the file extension.
#' @import ggplot2
#' @import gridExtra
#' @import terra
#'
#' @return Plot showing selected data_col for all months at grid-cell level.
#' @export
#'
monthly_plots <- function(data, data_col, output_file) {
  min_val <- min(data[[data_col]])
  max_val <- max(data[[data_col]])
  plots <- lapply(unique(data$months), function(month) {
    month_df <- subset(data, month == data$months)
    ggplot(month_df, aes_string(x = month_df$x, y = month_df$y, fill = data_col)) +
      geom_tile() +
      scale_fill_viridis_c(limits = c(min_val - 2, max_val + 2)) +
      labs(title = month.name[as.numeric(month)], x = "Longitude", y = "Latitude") +
      theme(aspect.ratio = 1, panel.background = element_blank(), panel.grid = element_blank())
  })

  grid <- grid.arrange(grobs = plots, ncol = 3)
  ggsave(output_file, plot = grid, width = 8, height = 12, units = "in", dpi = 300)
}
