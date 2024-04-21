#' Extracting date components for the desired format of the API
#'
#' The Copernicus Climate Data Store (CDS) API requires the date components for downloading the data to be in a specific format. \cr
#' It requires one list for the years, one for the months and one for the days. \cr
#' This function allows the user to set a StartDate and EndDate, and the function will automatically extract the years, months, and days, if no other specifications are set. \cr
#' In addition, the user can filter the dates based on the following parameters: \cr
#' - target_years: select specific years \cr
#' - target_months: select specific months \cr
#' - target_days: select specific days \cr
#' - season: select a season \cr
#' - year_interval: select every nth year \cr
#' - month_interval: select every nth month \cr
#' - day_interval: select every nth day \cr
#'
#' @param StartDate A character string or Date object specifying the start date of the data extraction in the format "YYYY-MM-DD" (e.g. "2010-01-01").
#' @param EndDate A character string or Date object specifying the end date of the data extraction in the format "YYYY-MM-DD" (e.g. "2018-11-05").
#' @param dataset_short_name A character string specifying the dataset short name (e.g. "ERA5-Land monthly averaged data") as in the API
#' It is used to check if the dataset is monthly or daily. If the dataset is monthly, days will be an empty list.
#' @param year_interval An integer specifying the interval for the years (e.g. 2,3,...). The default is 1 (optional parameter).
#' @param month_interval An integer specifying the interval for the months (e.g. 2,3,...). The default is 1 (optional parameter).
#' @param day_interval An integer specifying the interval for the days (e.g. 2,3,...). The default is 1 (optional parameter).
#' @param target_years A character vector specifying the years to be selected (e.g. c("2015", "2016", "2017")). If provided, only these years will be selected and the months and days adjusted (optional parameter).
#' @param target_days A character vector specifying the days to be selected (e.g. c("15", "16", "17")). If provided, only these days will be selected and the months and years adjusted (optional parameter).\cr
#' If the dataset is monthly, it is not possible to select days, the parameter will be ignored and days will be empty.
#' @param season A character string specifying the season to be selected (e.g. "DJF", "MAM", "JJA", "SON") (optional parameter) \cr
#' @param target_months A character vector specifying the months to be selected (e.g. c("01", "02", "03")). If provided, only these months will be selected and the days and years adjusted (optional parameter).
#' @return A list containing the extracted date components: \cr
#' - year: A character vector containing the years \cr
#' - month: A character vector containing the months \cr
#' - day: A character vector containing the days
#' @export
#'
extract_date_components <- function(StartDate, EndDate, dataset_short_name, year_interval = 1, month_interval = 1, day_interval = 1,
                                    target_years=NULL, target_days=NULL, season=NULL, target_months=NULL) {

  # Check if StartDate and EndDate are in Date format, if not, convert them
  if (!inherits(StartDate, "Date")) {
    StartDate <- as.Date(StartDate)
  }
  if (!inherits(EndDate, "Date")) {
    EndDate <- as.Date(EndDate)
  }
  # Create sequence of dates between StartDate and EndDate for each day
  seq_dates <- seq(from = StartDate, to = EndDate, by = "day")
  # extract months and years from the date sequence
  month_year <- format(seq_dates, '%Y-%m')
  # extract all unique combinations of year and month for filtering later
  unique_year_month <- unique(month_year)
  # extract the months and unlist them
  month <- lapply(unique_year_month, function(x) gsub(".*-", "", x))
  #unlist the months
  month <- unlist(month)
  # extract the years
  year <- lapply(unique_year_month, function(x) gsub("-.*", "", x))
  # unlist them and only keep the unique values for the years
  unique_year <- unlist(unique(year))

  # Check if seasons are provided
  if (!is.null(season)) {
    # The months selected based on the seasons will then be the input for the target_months
    target_months <- MCERA5::get_seasonal_data(season)
  }

  ### Filter the Dates based on the input parameters ###

  # First Option: Check if only target_years are provided

  # If target_years are provided, only those years will be selected
  # months and days will be adjusted based on target_years
  if (!is.null(target_years) && is.null(target_months) && is.null(target_days)) {
    # Filter the dates based on target_years
    filtered_dates <- seq_dates[format(seq_dates, "%Y") %in% target_years]
    # Extract the years from the filtered_dates, we only need unique year values
    year <- unique(format(filtered_dates, "%Y"))
    # Extract the months
    month <- unlist(lapply(year, function(y) unique(format(filtered_dates[format(filtered_dates, "%Y") == y], "%m"))))
    # Extract the days
    days <- format(filtered_dates, "%d")
    # Convert the days to integer
    # otherwise the first day would be 01 for example, but the API expects 1
    days <- sprintf("%d", as.integer(days))
    # If the dataset is monthly, it is not possible to select days
    # so days will be an empty list
    if (grepl("monthly", tolower(dataset_short_name))) {
      days <- "[]"

      # Warn the user that days will not be considered
      if (!is.null(target_days)){
        warning("It is not possible to select days when selecting a monthly dataset. Only year and month will be considered.")
      }
    }
    # Second Option: Check if target_years and target_months are provided
    # days will be adjusted based on target_months
  } else if (!is.null(target_years) && !is.null(target_months) && is.null(target_days)) {
    # Filter dates based on target_years and target_months
    filtered_dates <- seq_dates[format(seq_dates, "%Y") %in% target_years & format(seq_dates, "%m") %in% target_months]
    # Extract the years from the filtered_dates, we only need unique year values
    year <- unique(format(filtered_dates, "%Y"))
    # # Extract the unique months for each year from the filtered dates
    # by iterating over the unique years and filtering dates accordingly
    # it checks, where the year matches the year in the unique years
    # and then extracts the unique months for each year
    month <- unlist(lapply(year, function(y) unique(format(filtered_dates[format(filtered_dates, "%Y") == y], "%m"))))
    # Extract the days
    days <- format(filtered_dates, "%d")
    # Convert the days to integer
    days <- sprintf("%d", as.integer(days))
    # If the dataset is monthly, it is not possible to select days
    if (grepl("monthly", tolower(dataset_short_name))) {
      days <- "[]"
      # Warn the user that days will not be considered
      if (!is.null(target_days)){
        warning("It is not possible to select days when selecting a monthly dataset. Only year and month will be considered.")
      }
    }
    # Third Option: Check if target_years and target_days are provided
    # months will be adjusted based on target_years and target_days
  } else if (!is.null(target_years) && is.null(target_months) && !is.null(target_days)) {
    # Filter dates based on target_years and target_days
    filtered_dates <- seq_dates[format(seq_dates, "%Y") %in% target_years & format(seq_dates, "%d") %in% target_days]
    year <- unique(format(filtered_dates, "%Y"))
    month <- unlist(lapply(year, function(y) unique(format(filtered_dates[format(filtered_dates, "%Y") == y], "%m"))))
    days <- format(filtered_dates, "%d")
    days <- sprintf("%d", as.integer(days))
    if (grepl("monthly", tolower(dataset_short_name))) {
      days <- "[]"
      # Warn the user that days will not be considered
      if (!is.null(target_days)){
        warning("It is not possible to select days when selecting a monthly dataset. Only year and month will be considered.")
      }
    }
    # Fourth Option: Check if target_months are provided
    # If target_months are provided, only those months will be selected
    # days and years will be adjusted based on target_months
  } else if (!is.null(target_months) && is.null(target_days) && is.null(target_years)) {
    # filter dates based on target_months
    filtered_dates <- seq_dates[format(seq_dates, "%m") %in% target_months]
    # extract the years from the filtered_dates
    year <- unique(format(filtered_dates, "%Y"))
    # extract the months from the filtered_dates (the target_month for each year)
    month <- unlist(lapply(year, function(y) unique(format(filtered_dates[format(filtered_dates, "%Y") == y], "%m"))))
    # extract the days from the filtered_dates
    days <- format(filtered_dates, "%d")
    # convert the days to integer
    days <- sprintf("%d", as.integer(days))
    # If the dataset is monthly, it is not possible to select days
    if (grepl("monthly", tolower(dataset_short_name))) {
      days <- "[]"
      # Warn the user that days will not be considered
      if (!is.null(target_days)){
        warning("It is not possible to select days when selecting a monthly dataset. Only year and month will be considered.")
      }}
    # Additionally, make days an empty list if season is provided
    # and a monthly dataset is selected
    if (!is.null(season)) {
      #check if StartDate month matches with the first month of the seasonal data
      #and if the day of StartDate is the first day of the month
      if (as.numeric(format(StartDate, "%m")) != as.integer(target_months[1]) ||
          (as.numeric(format(StartDate, "%d")) != 01)){
        stop(paste("StartDate does not match with the first month of your selected season:", season,
                   ".\nIt should start with:", target_months[1],
                   ".\nIn addition: Day of StartDate must be the first day of the month."
        ))
      }
      # also check if the EndDate month matches with the last month of the seasonal data
      # and if the day of EndDate is the last day of the month
      if (as.numeric(format(EndDate, "%m")) != as.integer(target_months[length(target_months)])) {
        stop(paste("EndDate month does not match with the seasonal data:", season,
                   ".\nIt should end with:", target_months[length(target_months)],
                   ".\nIn addition: Day of EndDate must be the last day of the month."
        ))
      } # Check if the day of the EndDate is the last day of the month
      # If the last month of the season is February, check if the day is 28 or 29
      if (target_months[length(target_months)] == "02" && !as.numeric(format(EndDate, "%d")) %in% c(28, 29)) {
        stop("EndDate must be the last day of the month of the season selected.")
      }
      # Check if the day of the EndDate is Mai or August
      # day of EndDate must be 31
      if (target_months[length(target_months)] == "05" && as.numeric(format(EndDate, "%d")) != 31) {
        stop("EndDate must be the last day of the month of the season selected.")
      }
      if (target_months[length(target_months)] == "08" && as.numeric(format(EndDate, "%d")) != 31) {
        stop("EndDate must be the last day of the month of the season selected.")
      }
      # Check if the day of the EndDate is November
      # day of EndDate must be 30
      if (target_months[length(target_months)] == "11" && as.numeric(format(EndDate, "%d")) != 30) {
        stop("EndDate must be the last day of the month of the season selected.")
      }
    }

    # Fifth Option: Check if target_months and  target_days are provided
  } else if (!is.null(target_months) && !is.null(target_days) && is.null(target_years)) {
    # Filter months based on target_months and target_days
    filtered_dates <- seq_dates[format(seq_dates, "%m") %in% target_months & format(seq_dates, "%d") %in% target_days]
    year <- unique(format(filtered_dates, "%Y"))
    month <- unlist(lapply(year, function(y) unique(format(filtered_dates[format(filtered_dates, "%Y") == y], "%m"))))
    days <- format(filtered_dates, "%d")
    days <- sprintf("%d", as.integer(days))
    # If the dataset is monthly, it is not possible to select days
    if (grepl("monthly", tolower(dataset_short_name))) {
      days <- "[]"

      # Warn the user that days will not be considered
      if (!is.null(target_days)){
        warning("It is not possible to select days when selecting a monthly dataset. Only year and month will be considered.")
      }
    }
    # Sixth Option: Check if only target_days are provided
  } else if (!is.null(target_days) && is.null(target_months) && is.null(target_years)) {
    # Filter days based on day_interval and target_days
    filtered_dates <- seq_dates[format(seq_dates, "%d") %in% target_days]
    year <- unique(format(filtered_dates, "%Y"))
    month <- unlist(lapply(year, function(y) unique(format(filtered_dates[format(filtered_dates, "%Y") == y], "%m"))))
    days <- format(filtered_dates, "%d")
    days <- sprintf("%d", as.integer(days))
    if (grepl("monthly", tolower(dataset_short_name))) {
      days <- "[]"
      # Warn the user that days will not be considered
      if (!is.null(target_days)){
        warning("It is not possible to select days when selecting a monthly dataset. Only year and month will be considered.")
      }
    }
    # Seventh Option: Check if target_days taget_months and  target_years are provided
  } else if (!is.null(target_days) && !is.null(target_months) && !is.null(target_years)) {
    # Filter days based on day_interval and target_days
    filtered_dates <- seq_dates[format(seq_dates, "%d") %in% target_days & format(seq_dates, "%m") %in% target_months & format(seq_dates, "%Y") %in% target_years]
    year <- unique(format(filtered_dates, "%Y"))
    month <- unlist(lapply(year, function(y) unique(format(filtered_dates[format(filtered_dates, "%Y") == y], "%m"))))
    days <- format(filtered_dates, "%d")
    days <- sprintf("%d", as.integer(days))
    if (grepl("monthly", tolower(dataset_short_name))) {
      days <- "[]"

      # Warn the user that days will not be considered
      if (!is.null(target_days)){
        warning("It is not possible to select days when selecting a monthly dataset. Only year and month will be considered.")
      }
    }
    ## Optionally, filter the dates based on interval parameters ##
    # The default for the the interval parameters is 1
    # If the interval parameters are not 1, the dates will be filtered based on the interval parameters
  } else if (day_interval > 1) {
    # Filter the dates based on the day_interval
    filtered_dates <- seq_dates[seq(1, length(seq_dates), by = day_interval)]
    year <- unique(format(filtered_dates, "%Y"))
    month <- unlist(lapply(year, function(y) unique(format(filtered_dates[format(filtered_dates, "%Y") == y], "%m"))))
    days <- format(filtered_dates, "%d")
    days <- sprintf("%d", as.integer(days))
  } else if (year_interval > 1) {
    # dates can't be as easily filtered by year_interval as by day_interval
    # Therefore, Iterate over the unique years and select every nth year based on year_interval
    year <- unique_year[seq(1, length(unique_year), by = year_interval)]
    # Then, filter the dates based on the extracted years
    filtered_dates <- seq_dates[format(seq_dates, '%Y') %in% year]
    # extract the months
    month <- unlist(lapply(year, function(y) unique(format(filtered_dates[format(filtered_dates, "%Y") == y], "%m"))))
    # extract the days
    days <- format(filtered_dates, "%d")
    days <- sprintf("%d", as.integer(days))
    # Filter dates by month_interval
  } else if (month_interval > 1) {
    dates <- seq_dates
    # filter the unique_year_month based on month_interval
    # and return every nth month based on month_interval
    unique_year_month_int <- unique_year_month[seq(1, length(unique_year_month), by = month_interval)]
    # extract the years
    year <- unlist(unique(lapply(unique_year_month_int, function(x) gsub("-.*", "", x))))
    # extract the months
    month <- unlist(lapply(unique_year_month_int, function(x) gsub(".*-", "", x)))
    # get days
    # filter seq_dates based on the unique_year_month
    filtered_dates_int <- dates[format(dates, '%Y-%m') %in% unique_year_month_int]
    days <- format(filtered_dates_int, "%d")
    days <- sprintf("%d", as.integer(days))
  } else {
    # Default case when no specific target_years, target_months, or target_days are provided
    days <- format(seq_dates, "%d")
    days <- sprintf("%d", as.integer(days))
    month <- month
    year <- unique_year
    # if the dataset is monthly, it is not possible to select days
    # so days will be an empty list
    if (grepl("monthly", tolower(dataset_short_name))) {
      days <- "[]"
      # Warn the user that days will not be considered
      if (!is.null(target_days)){
        warning("It is not possible to select days when selecting a monthly dataset. Only year and month will be considered.")
      }
    }
  }
  # Return a list of extracted components
  return(list(year = year, month = month, day = days))
}

