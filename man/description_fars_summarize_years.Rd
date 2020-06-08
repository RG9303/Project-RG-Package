\name{fars_summarize_years}
\alias{fars_summarize_years}

\title{A name string of a database}
\description{
 transmute the data frame by group, summarize and spread a key-value pair across the variables year (2013, 2014 and 2015) and a month.
}
\usage{
 fars_summarize_years(year)
}
\arguments{
  \item{year}{a connection or a character string giving the
    year of the file to load.}
}
\seealso{
  \code{\link{summarize}}.
}
\examples{
## transmute a database transmuted
fars_summarize_years(2014)

}
\keyword{file}


