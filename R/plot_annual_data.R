#' Plot annual raster data
#'
#' @param data Object of class SpatRaster which contains annual layers. Monthly data can be aggregated with months_aggregate().
#' @param output_file Output file name. Should include the file extension.
#' @import ggplot2
#' @import gridExtra
#' @import terra
#'
#' @return Plot showing all years as raster in a grid.
#' @export
#'
plot_annual_data <- function(data, output_file) {
  # Calculate min and max values for each layer
  all_values <- values(data)
  min_val <- min(all_values, na.rm=TRUE)
  max_val <- max(all_values, na.rm=TRUE)
  # Get the number of layers
  layers <- nlyr(data)

  # Create an empty list to store plots
  plots <- list()

  # Loop through each layer and create a plot
  for (i in 1:layers) {
    # Extract the ith layer
    layer <- data[[i]]
    year <- format(as.Date(time(layer)), format = "%Y")
    layer_df <- as.data.frame(layer, xy=TRUE, time=TRUE)
    colnames(layer_df) <- c("longitude", "latitude", "value")

    # Create the plot for the current layer
    plot <- ggplot(layer_df, aes(x=longitude, y=latitude, fill=value)) +
      geom_tile() +
      scale_fill_viridis_c(limits = c(min_val - 2, max_val + 2)) +
      labs(title = paste("Year", year), x = "Longitude", y = "Latitude") +
      theme(aspect.ratio = 1, panel.background = element_blank(), panel.grid = element_blank())

    # Add the plot to the list
    plots[[i]] <- plot
  }

  # Arrange the plots in a grid
  grid <- grid.arrange(grobs = plots, ncol = 3)

  # Save the grid of plots to a file
  ggsave(output_file, plot = grid, width = 8, height = 12, units = "in", dpi = 300)
}
