\name{fars_read_years}
\alias{fars_read_years}

\title{A name string of a database}
\description{
  Save the name of a specific data base according to a year, read the database and transmute it drops existing variables as year (2013, 2014 and 2015).
}
\usage{
 fars_read_years(years)
}
\arguments{
  \item{year}{a connection or a character string giving the
    year of the file to load.}
}
\seealso{
  \code{\link{mutate}}.
}
\examples{
## save a database transmuted
fars_read_years(2013)

}
\keyword{file}


