\name{julian}
\alias{julian}
\title{Julian}
\description{
  Convert between Calendar Dates and Julian.
}
\usage{
julian(m, d, y, origin.=c(month=1, day=1, year=1960))
}
\arguments{
  \item{m}{vector of month numbers.}
  \item{d}{vector of day numbers.}
  \item{y}{vector of years}
  \item{origin}{vector specifying the origin as month, day, and year. If
missing, it defaults to options(chron.origin) if this is
non-null, otherwise c(month=1, day=1, year=1960).}
}
\value{
  For each date, the function return the number of days between it and January 1, 1960.
  The date will be missing if the string is not interpretable.
}

\details{ This function was taken from Becker, Chambers, and Wilks
          (1988), and was slightly modified to take dates objects; some
          also take the extra argument origin. 
}

}
\seealso{
  \code{\link{event.chart}},
  \code{\link{event.convert}},
  \code{\link{month.day.year}
}
\keyword{chron}

