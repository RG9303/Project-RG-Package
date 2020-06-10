#' @title fars_map_state
#' @description The function \code{fars_map_state} transform the principal database and add
#' conditionals for some variables. Plot a map with a specific lat and long.
#' @param state.num as a variable that represent a state.
#' @param year as a variable to tranform it in an integer value.
#' @return if number of a state is unique and it is contained in the variable STATE of the data
#' it will make a data frame with this, with conditionals to transform NAs in the
#' variables LONGITUD AND LATITUDE and print a map with this location. Otherwise print a
#' message "no accidents to plot" and return an invisible object.
#' @details you need to install the package "map" and specify a number of a state.
#' @import graphics
#' @examples fars_map_state(19, 2013)
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
