\name{fars_map_state}
\alias{fars_map_state}

\title{A name string of a database}
\description{
Transform the principal database and add conditionals for some variables. Plot a map with a specific lat and long. The variable state is a variable with format numeric.
}
\usage{
 fars_map_state(state.num, year)
}
\arguments{
  \item{state}{a number that represents a code state.}
  \item{year}{a connection or a character string giving the
    year of the file to load.}
}
\seealso{
  \code{\link{summarize}}.
}
\examples{
## transmute a database transmuted
fars_map_state(19, 2013)

}
\keyword{file}


