\name{month.day.year}
\alias{month.day.year}
\title{month.day.year}
\description{
  Convert between Julian and Calendar Dates.
}
\usage{
month.day.year(jul, origin.=c(month=1, day=1, year=1960))
}
\arguments{
  \item{jul}{vector of Julian Dates, i.e., number of days since origin.}
  \item{origin.}{vector specifying the origin as month, day, and year. If
missing, it defaults to options(chron.origin) if this is
non-null, otherwise c(month=1, day=1, year=1960).}
}
\value{
  For each julian date we obtain a list of month day year.
}


\details{ Thi function was taken from Becker, Chambers, and Wilks
          (1988), and was slightly modified to take dates objects; some
          also take the extra argument origin. 
}
\seealso{
  \code{\link{event.chart}},
  \code{\link{event.convert}},
  \code{\link{julian}}
}
\keyword{chron}
