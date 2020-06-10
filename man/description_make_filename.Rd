\name{make_filename}
\alias{make_filename}

\title{A name string of a database}
\description{
  Transform a variable as a integer and print a character vector containing a formatted combination of text and variable value and return a character vector containing a formatted combination of text and variable value. You need to enter a number.
}
\usage{
 make_filename(year)
}
\arguments{
  \item{year}{a connection or a character string giving the
    year of the file to load.}
}
\seealso{
  \code{\link{sprintf}}.
}
\examples{
## ptint a name of a database
make_filename(2013)

}
\keyword{file}


