
#' @title make_filename
#' @description The function \code{make_file} transform a variable as a integer and print a character vector
#' containing a formatted combination of text and variable value.
#' @param year as a variable to tranform it in an integer value.
#' @return a character vector containing a formatted combination of text and variable value.
#' @details you need enter a number or it return a NA.
#' @examples make_filename(2013)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
