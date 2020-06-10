#' @title fars_read
#' @description The function \code{fars_read} read a csv file if it exists and forwards the argument a data frame.
#' @param filename to enter a database with format csv.
#' @return if file exists, this function read the file and return a database as a data frame. If the extension
#' is diferent to csv, it can not read the file.
#' @details you need install packages like dplyr and readr before this or it may result in an error
#' @import readr
#' @import dplyr
#' @examples fars_read(filename = accident_2013.csv.bz2)
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
