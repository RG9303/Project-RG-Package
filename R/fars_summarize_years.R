#' @title fars_summarize_years
#' @description The function \code{fars_summarize_years} transmute the data frame by group, summarize and
#' spread a key-value pair across the variables year and n.
#' @param years as a variable to read the other functions and transmute the data frame.
#' @return a data frame by group of year and month, and summarize by count. It will print the head of the database.
#' @details you need install the library tidyr and conserve the format of the variables.
#' @import tidyr
#' @examples fars_summarize_years(2014)
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
