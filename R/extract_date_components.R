#' Extract date components as input for the ERA5 function
#'
#'This function extracts the year, month and day between StartDate and EndDate specified in the ERA5 function. \cr
#' The API for the Copernicus Climate Data Store can only handle year, month and day as input for the download request. \cr
#' This function simplifies the process of extracting the year, month and day by allowing the user to specify a Start date and End date. \cr
#' Moreover, the user can specify an interval for the year, month and day, so that every n-th year, month and day is extracted. \cr
#' The user can also specify target years, months and days to extract only the specified years, months and days. \cr
#' Besides, the user can specify a season so that the function will extract the months of the specified season. \cr
#' If a monthly dataset is specified, only the year and month will be extracted.
#' @param StartDate Character string. The start date specified in the ERA5 function (e.g. "2000-01-01")
#' @param EndDate Character string. The end date specified in the ERA5 function (e.g. "2000-01-31")
#' @param dataset_short_name Character string. The short name of the dataset (e.g. "reanalysis-era5-land")
#' @param year_interval Integer. The interval for the years to be extracted. The default is 1 (optional parameter)
#' @param month_interval Integer. The interval for the months to be extracted. The default is 1 (optional parameter)
#' @param day_interval Integer. The interval for the days to be extracted. The default is 1 (optional parameter)
#' @param target_years Character vector. Can be used to only extract specific years (optional parameter).
#' @param target_days Character vector. Can be used to only extract specific days (optional parameter).
#' @param seasonal_data Character string. Can be used to extract the months of a specific season (optional parameter).\cr
#' For details, see the \code{\link{get_seasonal_data}} function.
#' @param target_months Character vector. Can be used to only extract specific months (optional parameter).
#'
#' @return A list with the extracted years, months and days
#' @export
#'
#' @examples
#' StartDate <- "2000-01-01"
#' EndDate <- "2000-01-31"
#' dataset_short_name <- "reanalysis-era5-land"
#' date_components <- extract_date_components(StartDate, EndDate, dataset_short_name)
#' print(date_components$days)
#'$years
#'[1] "2000"
#'$months
#'[1] "01"
#'$days
#'[1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20"
#'[21] "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31"
#'
#'Example using target_days
#'StartDate <- "2000-01-01"
#'EndDate <- "2000-12-31"
#'dataset_short_name <- "reanalysis-era5-land"
#'date_components <- extract_date_components(StartDate, EndDate, dataset_short_name, target_days = c("01", "15", "31"))
#'print(date_components)
#'$years
#'[1] "2000"
#'$months
#'[1] "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"
#'$days
#'[1] "01" "15" "31" "01" "15" "31" "01" "15" "31" "01" "15" "31" "01" "15" "31" "01" "15" "31" "01" "15"
#'[21] "31" "01" "15" "31" "01" "15" "31" "01" "15" "31" "01" "15" "31" "01" "15" "31"
#'
#'Example using seasonal data
#'StartDate <- "2000-12-01"
#'EndDate <- "2002-02-01"
#'dataset_short_name <- "reanalysis-era5-land-monthly-means"
#'date_components <- extract_date_components(StartDate, EndDate, dataset_short_name, seasonal_data = "DJF")
#'print(date_components)
#'$years
#'[1] "2000" "2001" "2002"
#'$months
#'[1] "12" "01" "02" "12" "01" "02"
#'$days
#'NULL

extract_date_components <- function(StartDate, EndDate, dataset_short_name, year_interval = 1, month_interval = 1, day_interval = 1,
                                    target_years=NULL, target_days=NULL, seasonal_data=NULL, target_months=NULL) {

  # Check if StartDate and EndDate are in Date format, if not, convert them
  if (!inherits(StartDate, "Date")) {
    StartDate <- as.Date(StartDate)
  }
  if (!inherits(EndDate, "Date")) {
    EndDate <- as.Date(EndDate)
  }

  # Create sequence of dates between StartDate and EndDate
  seq_dates <- seq(from = StartDate, to = EndDate, by = "day")

  # Extract year and month
  year_month <- format(seq_dates, '%Y-%m')

  # Set days as a single [] if dataset is monthly
  if (grepl("monthly", tolower(dataset_short_name))) {
    days <- "[]"

    # Warn the user about selecting days for a monthly dataset
    warning("It is not possible to select days when selecting a monthly dataset. Only year and month will be considered.")
  } else {
    days <- format(seq_dates, "%d")
  }

  # Define target months based on seasonal data if specified
  if (!is.null(seasonal_data)) {
    target_months <- MCERA5::get_seasonal_data(seasonal_data)
    days <- "[]" # Set days as empty for seasonal data

    # Check if StartDate month matches the seasonal data
    if (as.numeric(format(StartDate, "%m")) != as.integer(target_months[1])) {
      warning(paste("StartDate month does not start with the same month of seasonal data:", seasonal_data, ". It should start with:", target_months[1]))
    }

    # Check if EndDate month matches the seasonal data
    if (as.numeric(format(StartDate, "%m")) != as.integer(target_months[3])) {
      warning(paste("EndDate month does not match with the seasonal data:", seasonal_data, ". It should end with:", target_months[3]))
    }
  }

  # Let's store year, month, and day in a list
  if (!"[]" %in% days || !is.null(target_days)) {
    unique_year_months <- length(unique(year_month)) # how many months we have in total
    dates_list <- as.list(rep(NA, unique_year_months)) # an empty list to be filled with date specifications

    for(i in 1:unique_year_months) { # calls loop: identify the dates to be called for each month in the sequence
      if (!is.null(target_days)) {
        days_for_month <- target_days
      } else {
        days_for_month <- min(days[which(year_month == unique(year_month)[i])]):max(days[which(year_month == unique(year_month)[i])])
      }

      dates_list[[i]] <- c(gsub("-.*.", "", unique(year_month)[i]), # year
                           gsub(".*.-", "", unique(year_month)[i]), # month
                           days_for_month) # days of that month
    }
  } else {
    unique_year_months <- length(unique(year_month)) # how many months we have in total
    dates_list <- as.list(rep(NA, unique_year_months)) # an empty list to be filled with date specifications

    for(i in 1:unique_year_months) { # calls loop: identify the dates to be called for each month in the sequence
      dates_list[[i]] <- c(gsub("-.*.", "", unique(year_month)[i]), # year
                           gsub(".*.-", "", unique(year_month)[i])) # month
    }
  }

  # Define a function to filter based on target_years if specified
  filter_years <- function(years_unique, target_years) {
    if (!is.null(target_years)) {
      return(years_unique[years_unique == target_years])
    } else {
      return(years_unique)
    }
  }

  # Define a function to filter based on target_months if specified
  filter_months <- function(months_unique, target_months) {
    if (!is.null(target_months)) {
      return(months_unique[months_unique %in% target_months])
    } else {
      return(months_unique)
    }
  }

  # Define a function to filter based on target_months if specified
  filter_days <- function(days_unique, target_days) {
    if (!is.null(target_days)) {
      return(days_unique[days_unique == target_days])
    } else {
      return(days_unique)
    }
  }

  # Extract years
  years_extr <- lapply(dates_list, function(x) x[1])
  years_unique <- filter_years(unlist(unique(years_extr))[seq(1, length(unlist(unique(years_extr))), year_interval)], target_years)

  # Extract months
  months_extr <- lapply(dates_list, function(x) x[2])
  months_unique <- filter_months(unlist(months_extr)[seq(1, length(unlist(months_extr)), month_interval)], target_months)

  # Extract days
  if (!"[]" %in% days) {
    days_extr <- lapply(dates_list, function(x) x[3:length(x)])
    days_unique <- filter_days(unlist(days_extr)[seq(1, length(unlist(days_extr)), day_interval)], target_days)
  } else {
    days_unique <- NULL
  }

  # Return years, months, and days as lists
  return(list(years = years_unique, months = months_unique, days = days_unique))
}
