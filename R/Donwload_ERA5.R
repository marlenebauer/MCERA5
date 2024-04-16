#' Title
#'
#' @param user
#' @param key
#' @param dataset_short_name
#' @param variable
#' @param StartDate
#' @param EndDate
#' @param year_interval
#' @param month_interval
#' @param day_interval
#' @param target_months
#' @param target_days
#' @param seasonal_data
#' @param spatial_obj
#' @param country_name
#' @param admin_level
#' @param product_type
#' @param pressure_level
#' @param target_file
#' @param time_out
#' @param path
#' @param time
#' @param grid
#'
#' @return
#' @export
#'
#' @examples
era5 <- function(user, key, dataset_short_name, variable, StartDate, EndDate, year_interval,
                 month_interval, day_interval, target_months = NULL, target_days = NULL,seasonal_data = NULL,
                 spatial_obj = NULL, country_name = NULL, admin_level = NULL, product_type = NULL, pressure_level = NULL, target_file = NULL,
                 time_out, path = NULL, time, grid = NULL) {
  # Set default grid value based on dataset_short_name if grid is not provided by the user
  if (is.null(grid)) {
    if (grepl("land", dataset_short_name, ignore.case = TRUE)) {
      grid <- ".1/.1"
    } else {
      grid <- ".5/.5"
    }
  }

  # Set the key for the user
  wf_set_key(
    user = user,
    key = key,
    service = "cds"
  )
  # Check if pressure_level is selected and dataset_short_name contains 'pressure'
  if (!is.null(pressure_level) && !grepl("pressure", dataset_short_name, ignore.case = TRUE)) {
    stop("Pressure level can only be selected when a pressure dataset is selected.")
  }

  if (dataset_short_name %in% c("reanalysis-era5-single-levels-monthly-means", "reanalysis-era5-pressure-levels-monthly-means","reanalysis-era5-land-monthly-means") &
      product_type %in% c("monthly_averaged_reanalysis", "monthly_averaged_ensemble_members")) {
    time <- "00:00"
  }
  # Define a function to generate spatial object from country name and admin level
  # if no spatial object is provided
  # format is sfc object
  generate_spatial_obj <- function(country_name, admin_level = NULL) {
    if (is.null(admin_level)) {
      country <- geoboundaries(country_name)
    } else {
      country <- geoboundaries(country_name, admin_level)
    }
    if (nrow(country) == 0) {
      stop(paste("Country", country_name, "not found. Please provide a valid country name."))
    }
    return(country$geometry)
  }

  # Generate spatial object geometry if not provided
  if (is.null(spatial_obj) && !is.null(country_name)) {
    spatial_obj <- generate_spatial_obj(country_name, admin_level)
  }
  # Determine target months based on seasonal data if specified
  # Determine target months based on seasonal data if specified
  if (!is.null(seasonal_data)) {
    target_months <- get_seasonal_data(seasonal_data)
  }

  # extract date components to make them in the right format for the API
  date_components <- extract_date_components(
    StartDate = StartDate,
    EndDate = EndDate,
    dataset_short_name = dataset_short_name,
    year_interval = year_interval,
    month_interval = month_interval,
    day_interval = day_interval,
    target_months = target_months,
    target_days = target_days
  )

  #generate extent for the spatial object (take rounded coordinates, otherwise it doesn't work)
  extent_string <- get_extent_rounded_2_grid(spatial_obj, grid)

  # Generate default target file name if not provided
  if (is.null(target_file)) {
    target_file <- generate_default_target_file_name(variable, StartDate, EndDate)
  }

  # Define the request parameters
  request <- list(
    "dataset_short_name" = dataset_short_name,
    "product_type"       = product_type,
    "variable"           = variable,
    'pressure_level'     = pressure_level,
    "year"               = date_components$years,
    "month"              = date_components$months,
    "day"                = date_components$days,
    "time"               = time,
    "grid"               = grid,
    "area"               = extent_string, #N/W/S/E format
    "format"             = "netcdf",
    "target"             = target_file
  )
  # Determine the target directory (by default, the current working directory)
  if (is.null(path)) {
    target_path <- getwd()  # Use the current working directory if no path is provided
  } else {
    target_path <- path  # Use the provided path
  }
  # add timeout in hours or seconds
  time_out <- convert_hours_to_seconds(time_out)

  # Execute the request
  ncfile <- wf_request(
    user = user,
    request = request,
    transfer = TRUE,
    verbose = TRUE,
    time_out = time_out, # Timeout value provided by the user
    path = target_path
  )
  # Check if the download was successful
  if (!is.null(ncfile)) {
    message("You successfully downloaded the variable ", variable, " from the dataset ", dataset_short_name, " for the time period ", StartDate, " to ", EndDate," as SpatRaster. Use print to inspect the SpatRaster object.")
  } else {
    warning("There was an issue with the download. Please check your parameters and try again.")
  }

  # Rasterize the downloaded netCDF file and mask it to the spatial object
  masked_raster <- rasterize_and_mask(ncfile, spatial_obj)
  return(masked_raster)
}
