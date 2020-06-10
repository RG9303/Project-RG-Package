
#' @title fars_read_years
#' @description The function \code{fars_read_years} save the name of a specific data base according to a year,
#' read the ddatabase and transmute it drops existing variables as year.
#' @param years as a variable to tranform it in an integer value accross \code{make_filename}. It will be used
#' when it generate the name of a file with the function \code{fars_read}.
#' @return if the year is in the set: 2013, 2014 or 2015, it will transmute a database in a new data frame depending
#' on a specific month. Otherwise it will print a warning.
#' @details you need enter a number as a year contained in the set: 2013, 2014 or 2015 or it will return a warning
#' as a message.
#' @examples fars_read_years(2013)
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
