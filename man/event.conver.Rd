\name{event.convert}
\alias{event.convert}
\title{Convert coded event matrix or dataframe to a matrix of multiple event
times.}
\description{
  Convert coded event matrix or dataframe to a matrix of multiple event
times to be used in the event.chart function.
}
\usage{
event.convert(data2, event.time = 1, event.code = 2)
}
\arguments{
  \item{data2}{a matrix or dataframe with at least 2 columns; by default, the first
column contains the event time and the second column contains the k event codes (e.g. 1=dead, 0=censord).}
  \item{event.time}{the column number in data contains the event time.}
  \item{event.code}{the column number in data contains the event code.}
}
\value{
  In the survival analysis, the data typically come in two columns: one column
containing survival time and the other containing censoring indicator or event code. The event.convert function converts this type of data into 
multiple columns of event times, one column of each event type, suitable for the event.chart
function.} 
\examples{
# To convert coded time-to-event data, then, draw an event chart: 
  
surv.time <- c(5,6,3,1,2)
cens.ind   <- c(1,0,1,1,0)
surv.data  <- cbind(surv.time,cens.ind)
event.data <- event.convert(surv.data)
event.chart(cbind(rep(0,5),event.data),x.julian=TRUE,x.reference=1,
omi=c(1.5,0,.7,.2),mai=c(.6,.9,.6,.1), mgp=c(1,.4,0),cex=.65)
par(omi=c(1,0,.6,0))
mtext(side=3,"To convert coded time-to-event data, then, draw an event chart:",
line=3)
}
\seealso{
  \code{\link{julian}},
  \code{\link{month.day.year}}, 
  \code{\link{event.chart}}

\keyword{event.chart,chron}
