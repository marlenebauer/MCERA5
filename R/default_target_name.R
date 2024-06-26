#' Generate default target name for the ncfile downloaded by the ERA5 function
#'
#'This function generates a default target name for the ncfile downloaded by the ERA5 function. \cr
#' The default target name is generated by concatenating the variable name, StartDate and EndDate.
#'
#' @param variable Character string. The variable name specified in the ERA5 function.
#' datatype:
#' @param StartDate Character string. The start date specified in the ERA5 function.
#' @param EndDate Character string. The end date specified in the ERA5 function.
#' @param target_file Character string. If the user has specified a name fo the ncfile, this will be used instead of the file name generated by this function (optional parameter)
#'
#' @return A character string for the name of the ncfile
#' @export
#'

generate_default_target_file_name <- function(variable, StartDate, EndDate, target_file = NULL) {
  if (is.null(target_file)) {
    default_target_file <- paste0(variable, "_", StartDate, "__", EndDate, ".nc")
  } else {
    default_target_file <- target_file
  }
  return(default_target_file)
}
