\name{fars_read}
\alias{fars_read}

\title{How to make a map}
\description{
  A dataframe containing county-specific data by state in USA with contour using latitude and longitude of geographic center, and region code. It     includes each U.S. county as of the 2013, 2014 and 2015 census. Read a csv file when it exists and forwards the argument as a data frame with       format csv.
}
\usage{
fars_read(file = filename)
}
\arguments{
  \item{file}{a connection or a character string giving the
    name of the file to load.}
}
\seealso{
  \code{\link{read_csv}}.
}
\examples{
## read a database
fars_read(file = "x")

}
\keyword{file}
