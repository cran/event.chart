\name{event.chart}
\alias{event.chart}
\title{Very flexible event chart for Time-to-Event Data}
\description{
  Creates an event chart on the current graphics device. Also, allows user  
  to plot legend on plot area or on separate page. Contains features useful
  for plotting data with time-to-event outcomes which arise in a variety of
  studies including randomized clinical trials and non-randomized cohort
  studies. This function can use as input a matrix or a data frame.
  Data were extracted from Shin et al. 1996. 
}
\usage{
event.chart(data, subset.r = 1:dim(data)[1], subset.c = 1:dim(data)[2],

        sort.by = NA, sort.ascending = TRUE,
        sort.na.last = TRUE, sort.after.subset = TRUE,
        y.var = NA, y.var.type = 'n',
        y.jitter = FALSE, y.jitter.factor = 1,
        y.renum = FALSE, NA.rm = FALSE, x.reference = NA,
        now = max(data[,subset.c], na.rm = TRUE),
        now.line = FALSE, now.line.lty = 2,
        now.line.lwd = 1, now.line.col = 1, pty='m',
        date.orig = c(1,1,1960), titl = 'Event Chart',

        y.idlabels = NA, y.axis = 'auto',
        y.axis.custom.at = NA, y.axis.custom.labels = NA,
        y.julian = FALSE, y.lim.extend = c(0,0),
        y.lab = ifelse(is.na(y.idlabels), '' , as.character(y.idlabels)),

        x.axis.all = TRUE, x.axis = 'auto',
        x.axis.custom.at = NA, x.axis.custom.labels = NA,
        x.julian = FALSE, x.lim.extend = c(0,0), x.scale = 1,
        x.lab = ifelse(x.julian, 'Follow-up Time', 'Study Date'),

        line.by = NA, line.lty = 1, line.lwd = 1, line.col = 1,
        line.add = NA, line.add.lty = NA,
        line.add.lwd = NA, line.add.col = NA,
        point.pch = 1:length(subset.c),
        point.cex = rep(0.6,length(subset.c)),
        point.col = rep(1,length(subset.c)),

        legend.plot = FALSE, legend.location = 'o', legend.titl = titl,
        legend.titl.cex = 3.0, legend.titl.line = 1.0,
        legend.point.at = list(x = c(5,95), y = c(95,30)),
        legend.point.pch = point.pch,
        legend.point.text = ifelse(rep(is.data.frame(data),
              length(subset.c)), names(data[,subset.c]), subset.c),
        legend.cex = 2.5, legend.bty = 'n',
        legend.line.at = list(x = c(5,95), y = c(20,5)),
        legend.line.text = names(table(as.character(data[,line.by]),
              exclude = c('','NA'))),
        legend.line.lwd = line.lwd, legend.loc.num = 1,

        ...)
}
\arguments{
  \item{data}{a matrix or data frame with rows corresponding to subjects and
columns corresponding to variables.  Note that for a data frame or
matrix containing multiple time-to-event
data (e.g., time to recurrence, time to death, and time to
last follow-up), one column is required for each specific event.}
  \item{subset.r}{subset of rows of original matrix or data frame to place in 
event chart.
Logical arguments may be used here.}
  \item{subset.c}{subset of columns of original matrix or data frame to place 
in event chart;
if working with a data frame, a vector of data frame variable names may be
used for subsetting purposes (e.g., c('randdate', 'event1')).}
  \item{sort.by}{column(s) or data frame variable name(s) with which to sort 
the chart's output.
The default is NA, thereby resulting in a chart sorted by original row number.}
  \item{sort.ascending}{logical flag (which takes effect only if the argument 
sort.by is utilized).
If TRUE (default), sorting is done in ascending order; if FALSE, descending 
order.}
  \item{sort.na.last}{logical flag (which takes effect only if the argument 
sort.by is utilized).
If TRUE (default), NA values are considered as last values in ordering.}
  \item{sort.after.subset}{logical flag (which takes effect only if the 
argument sort.by is utilized).
If FALSE, sorting data (via sort.by specified variables
or columns) will be performed prior to row subsetting (via subset.r);
if TRUE (default), row subsetting of original data will be done before 
sorting.}
  \item{y.var}{variable name or column number of original matrix or data frame 
with
which to scale y-axis.
Default is NA, which will result in equally spaced lines on y-axis
(based on original data or sorted data if requested by sort.by).
Otherwise, location of lines on y-axis will be dictated by specified variable
or column.  Examples of specified variables may be date of an event
or a physiological covariate.  Any observation which has
a missing value for the y.var variable will not appear on the graph.}
  \item{y.var.type}{type of variable specified in y.var (which will only take 
effect if
argument y.var is utilized). If 'd', specifed variable is a date (either
numeric julian date or a dates object);  if 'n', specifed variable
is numeric (e.g., systolic blood pressure level) although not a julian date.}
  \item{y.jitter}{logical flag (which takes effect only if the argument y.var 
is utilized).
Due to potential ties in y.var variable, y.jitter (when TRUE) will jitter
the data to allow discrimination between observations at the possible cost
of producing slightly inaccurate dates or covariate values;  if FALSE (the
default), no jittering will be performed.  The y.jitter algorithm
assumes a uniform distribution of observations across the range of y.var.
The algorithm is as follows:

size.jitter <-(diff(range(y.var))/(2*(length(y.var)-1)) )*y.jitter.factor 

The default of y.jitter.factor is 1.  The entire product is then used as an
argument into runif:  y.var <-
y.var + runif(length(y.var), -size.jitter, size.jitter) .}
  \item{y.jitter.factor}{an argument used with the y.jitter function to scale 
the range of added noise.
Default is 1.}
  \item{y.renum}{logical flag.  If TRUE, subset observations are listed on 
y-axis from
1 to length(subset.r); if FALSE (default), subset observations are listed
on y-axis in original form.  As an example, if subset.r = 301:340 and
y.renum == TRUE, y-axis will be shown as 1 through 40.  However, if
y.renum == FALSE, y-axis will be shown as 301 through 340.  The above examples
assume the following argument, NA.rm, is set to FALSE.}
  \item{NA.rm}{logical flag.  If TRUE, subset observations which have NA for 
each variable
specified in subset.c will not have an entry on the y-axis.  Also, if
the following argument, x.reference, is specified, observations with
missing x.reference values will also not have an entry on the y-axis.  If FALSE
(default), user can identify those observations which do have NA for
every variable specified in subset.c (or, if x.reference is specified, also
those observations which are missing only the x.reference value); this can
easily be done by examining the resulting y-axis and
recognizing the observations without any plotting symbols.}
  \item{x.reference}{column of original matrix or data frame with which to 
reference the x-axis.
That is, if specified, all columns specified in subset.c will be substracted
by x.reference.  An example may be to see the timing of events before and
after treatment or to see time-to-event after entry into study.
The event times will be aligned using the x.reference argument
as the reference point.}
  \item{now}{he 'now' date which will be used for top of y-axis
when creating the Goldman eventchart (see reference below).
Default is max(data[, subset.c], na.rm = TRUE).}
  \item{now.line}{logical flag. A feature utilized by the Goldman Eventchart. 
When 

x.reference is specified as the start of follow-up and
y.var = x.reference, then the Goldman chart can be created.
This argument, if TRUE, will cause the plot region to be square, and will
draw a line with a slope of -1 from the top of the y-axis to the right
end of the x-axis.  Essentially, it denotes end of current follow-up period
for looking at the time-to-event data.  Default is FALSE.}
  \item{now.line.lty}{line type of now.line.}
  \item{now.line.lwd}{line width of now.line.}
  \item{now.line.col}{color of now.line.}
  \item{pty}{graph option, pty='m' is the default; use pty='s' for the square 
looking
Goldman's event chart.}
  \item{date.orig}{date of origin to consider if dates are in julian, SAS, or 
S-Plus dates
object format;  default is January 1, 1960 (which is the default origin
used by both  S-Plus and SAS).  Utilized when either y.julian = FALSE or
x.julian = FALSE.}
  \item{titl}{title for event chart.  Default is 'Event Chart'.}
  \item{y.idlabels.}{column or data frame variable name used for y-axis 
labels.  For example,
if c('pt.no') is specified, patient ID (stored in 'pt.no')
will be seen on y-axis labels
instead of sequence specified by subset.r.  This argument takes precedence
over both y.axis='auto' and y.axis='custom' (see below).
NOTE:  Program will issue warning if this argument is
specified and if is.na(y.var) == FALSE;  y.idlabels will not be
used in this situation.  Also, attempting to plot too many patients
on a single event chart will cause undesirable plotting of y.idlabels.}
  \item{y.axis}{character string specifying whether program will control 
labelling
of y-axis (with argument 'auto'), or if user will control labelling
(with argument 'custom').  If 'custom' is chosen, user must specify
location and text of labels using y.axis.custom.at and
y.axis.custom.labels arguments, respectively, listed below.
This argument will not be utilized if y.idlabels is specified.}
  \item{y.axis.custom.at}{user-specified vector of y-axis label locations.
Must be used when y.axis = 'custom'; will not be used otherwise.}
  \item{y.axis.custom.labels}{user-specified vector of y-axis labels.
Must be used when y.axis = 'custom'; will not be used otherwise.}
  \item{y.julian}{logical flag (which will only be considered if 

y.axis == 'auto' and (!is.na(y.var) & y.var.type== 'd')).  

If FALSE (default), will convert julian
numeric dates or S-Plus dates objects into 'mm/dd/yy' format
for the y-axis labels.  If TRUE, dates will be printed in
julian (numeric) format.}
  \item{y.lim.extend}{two-dimensional vector representing the number of units 
that the user
wants to increase ylim on bottom and top of y-axis, respectively.
Default = c(0,0).  This argument will not take effect if the Goldman chart
is utilized.}
  \item{y.lab}{single label to be used for entire y-axis.  Default will be the 
variable name
or column number of y.idlabels (if non-missing) and blank otherwise.}
  \item{x.axis.all}{logical flag. If TRUE (default), lower and upper limits of 
x-axis will be
based on all observations (rows) in matrix or data frame.  If FALSE, lower and
upper limits will be based only on those observations specified by subset.r
(either before or after sorting depending on specification of sort.by and
value of sort.after.subset).}
  \item{x.axis}{character string specifying whether program will control 
labelling
of x-axis (with argument 'auto'), or if user will control labelling
(with argument 'custom').  If 'custom' is chosen, user must specify
location and text of labels using x.axis.custom.at and
x.axis.custom.labels arguments, respectively, listed below.}
  \item{x.axis.custom.at}{user-specified vector of x-axis label locations.
Must be used when x.axis == 'custom'; will not be used otherwise.}
  \item{x.axis.custom.labels}{user-specified vector of x-axis labels.
Must be used when x.axis == 'custom'; will not be used otherwise.}
  \item{x.julian}{logical flag (which will only be considered if x.axis == 
'auto').
If FALSE (default), will convert julian dates objects
into 'mm/dd/yy' format for the x-axis labels.  If TRUE, dates will be
printed in julian (numeric) format.  NOTE:  This argument should remain TRUE if
x.reference is specified.}
  \item{x.lim.extend}{two-dimensional vector representing the number of time 
units (usually in days)
that the user wants to increase xlim on left-hand side and right-hand
side of x-axis, respectively.  Default = c(0,0).  This argument will not
take effect if the Goldman chart is utilized.}
  \item{x.scale}{a factor whose reciprocal is multiplied to original units of 
the
x-axis.  For example, if the original data frame is in units of days,
x.scale = 365 will result in units of years (notwithstanding leap years).
Default is 1.}
  \item{x.lab}{single label to be used for entire x-axis.  Default will be 'On 
Study Date'
if x.julian == FALSE and 'Time on Study' if x.julian = TRUE.}
  \item{line.by}{column or data frame variable name for plotting unique lines 
by unique
values of vector (e.g., specify c('arm') to plot unique lines by
treatment arm).  Can take at most one column or variable name.
Default is NA which produces identical lines for each patient.}
  \item{line.lty}{vector of line types corresponding to ascending order of 
line.by values.
If line.by is specified, the vector should be the length of
the number of unique values of line.by.
If line.by is NA, only line.lty[1] will be used.
The default is 1.}
  \item{line.lwd}{vector of line widths corresponding to ascending order of 
line.by values.
If line.by is specified, the vector should be the length of
the number of unique values of line.by.
If line.by is NA, only line.lwd[1] will be used.
The default is 1.}
  \item{line.col}{vector of line colors corresponding to ascending order of 
line.by values.
If line.by is specified, the vector should be the length of
the number of unique values of line.by.
If line.by is NA, only line.col[1] will be used.
The default is 1.}
  \item{line.add}{a 2xk matrix with k=number of pairs of additional line 
segments to add.
For example, if it is of interest to draw additional line segments
connecting events one and two, two and three, and four and five,
(possibly with different colors), an appropriate line.add argument would be 
matrix(c
('first.event','second.event','second.event','third.event', 'fourth.event',

'fifth.event'),2, 3).
One line segment
would be drawn between first.event and second.event,
a second line segment would be drawn between second.event and third.event,
and a third line segment would be drawn between
fourth.event and fifth.event. Different line types, widths and colors can be 
specified (in arguments
listed just below).

The convention use of subset.c and line.add must match (i.e., column name
must be used for both or column number must be used for both).

If line.add != NA, length of line.add.lty, line.add.lwd, and line.add.col
must be the same as number of pairs of additional line segments to add.

NOTE:  The drawing of the original default line
may be suppressed (with line.col = 0),
and line.add can be used to do all the line plotting for the event chart.}
  \item{line.add.lty}{a kx1 vector corresponding to the columns of line.add; 
specifies the line
types for the k line segments.}
  \item{line.add.lwd}{a kx1 vector corresponding to the columns of line.add; 
specifies the line
widths for the k line segments.}
  \item{line.add.col}{a kx1 vector corresponding to the columns of line.add; 
specifies the line
colors for the k line segments.}
  \item{point.pch}{vector of pch values for points representing each event.  
If similar
events are listed in multiple columns (e.g., regular visits or
a recurrent event), repeated pch values may be listed in the
vector (e.g., c(2,4,rep(183,3))).
If length(point.pch) < length(subset.c), point.pch will be repeated until
lengths are equal; a warning message will verify this condition.}
  \item{point.cex}{vector of size of points representing each event.
If length(point.cex) < length(subset.c), point.cex will be repeated until
lengths are equal; a warning message will verify this condition.}
  \item{point.col}{vector of colors of points representing each event.
If length(point.col) < length(subset.c), point.col will be repeated until
lengths are equal; a warning message will verify this condition.}
  \item{legend.plot}{logical flag;if TRUE, a legend will be plotted. Location 
of legend will
be based on specification of legend.location along with values of other
arguments listed below. Default is FALSE (i.e., no legend plotting).}
\item{legend.location}{will be used only if legend.plot=TRUE.
If 'o' (default), a one-page legend will precede the output of the chart.
The user will need to hit <enter> in order for the event chart to be 
displayed.If 'i', an internal legend will be placed in the plot region
based on legend.point.at.  If 'l', a legend will be placed in the plot region
using the locator option.  Legend will map points to events (via column
names, by default) and, if line.by is specified, lines to groups (based on
levels of line.by).}
  \item{legend.titl}{title for the legend; default is title to be used for 
main plot.
Only used when legend.location = 'o'.}
  \item{legend.titl.cex}{size of text for legend title.  Only used when
legend.location = 'o'.}
  \item{legend.titl.line}{line location of legend title dictated by mtext 
function with outer=FALSE option;
default is 1.0.  Only used when legend.location = 'o'.}
  \item{legend.point.at}{location of upper left and lower right corners of 
legend area to
be utilized for describing events via points and text.}
  \item{legend.point.pch}{vector of pch values for points representing each 
event in the legend.
Default is point.pch.}
  \item{legend.point.text}{text to be used for describing events;  the default 
is setup for a data frame,
as it will print the names of the columns specified by subset.c .}
  \item{legend.cex}{size of text for points and event descriptions.  Default 
is 2.5 which is setup
for legend.location = 'o'.  A much smaller cex is recommended (possibly 0.75)
for use with legend.location = 'i' or legend.location = 'l'.}
  \item{legend.bty}{option to put a box around the legend(s); default is to 
have no box
(legend.bty = 'n').  Option legend.bty = 'o' will produce a legend box.}
  \item{legend.line.at}{if line.by was specified (with legend.location = 'o' 
or legend.location = 'i'),
this argument will dictate the location of the upper left and lower right
corners of legend area to be utilized for describing the different
line.by values (e.g., treatment.arm).  The default is setup for
legend.location == 'o'.}
  \item{legend.line.text}{text to be used for describing line.by values;  the 
default are the names
of the unique non-missing line.by values as produced from the table function.}
  \item{legend.line.lwd}{vector of line widths corresponding to line.by
values.}
  \item{legend.loc.num}{number used for locator argument when legend.locator = 
'l'.  If 1 (default),
user is to locate only the top left corner of the legend box.  If 2, user
is to locate both the top left corner and the lower right corner.  This will
be done twice when line.by is specified (once for points and once for lines).}
  \item{...}{additional par arguments for use in main plot.}
}
\value{an event chart is created on the current graphics device.
If legend.plot = TRUE and legend.location = 'o',
a one-page legend will precede the event chart.  Please note that par
parameters on completion of function will be reset to par parameters
existing prior to start of function.
}
\seealso{
  \code{\link{event.chart}},
  \code{\link{julian}},
  \code{\link{month.day.year}},
  \code{\link{event.convert}},
  \code{\link{chron}}
}
\examples{
data(shin)
shin<-as.matrix(shin)

##Data were extracted from Shin D.M., Lee, J.S., Lippman, S.M., Lee, J.J.,
##Tu, Z.N.,Choi, G., Heyne, K., Shin, H.J.,C., Ro, J.Y., Goepfert, H., Hong, 
##W. K.,and Hittelman, W. N. (1996), "p53 Expression: Predicting Recurrence
##and Second  Primary Tumors in Head and Neck Squamous Cell Carcinoma,
##"Journal of National  Cancer Institute, 88, 519-529.

##id denotes subject ID
##age denotes the age of the subject
##p53 denotes the status of the covariate p53: 0=negative; 1=positive
##stage denotes the stage
##datened denotes the registration at NED = no evidence of disease
##daterec denotes the recurrence data
##datespt denote the time of the second primary tumor
##datedead denotes the time of the death
##datelfu denotes the time od the last follow-up

##we got the others columns in the following way:
##ind.dead <- !is.na(datedead)
##date.last.seen <- rep(NA,20)
##date.last.seen[ind.dead] <- datedead[ind.dead]
##date.last.seen[!ind.dead] <- datelfu[!ind.dead]
##date.last.seen = date.last.seen
##fu = date.last.seen - datened
##rec = daterec - datened
##spt = datespt - datened
##p53.time =100000 * (1-p53) + (date.last.seen - datened)
##p53.rec =100000 * (1-p53) + rec
##p53.spt = 100000 * (1-p53) + spt

##to produce simple Calendar Event Chart.

event.chart(shin,subset.c=c(5,6,7,8,9),
 y.idlabels=1,
 x.julian=FALSE,date.orig=c(month=1, day=1, year=1960),
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),point.col=rep(1,5),
 line.lwd=1,x.lab='Calendar Date',y.lab='Patient ID',
 titl="Calendar Event Chart",cex=.95,las=1,
 omi=c(2,0,1,.2),mai=c(.7,.7,.4,.07), mgp=c(2,.5,0), cex=.8)

legend(7900,.5,pch=c(1,17,16,4,0),c("date of registration","date of recurrence",
"date of SPT","date of death","date last follow-up"),ncol=5,cex=.8)

##to produce simple interval Event Chart,
event.chart(shin,subset.c=c(5,6,7,8,9), x.reference=5,
 y.idlabels=1, x.julian=TRUE, x.scale=365,
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),point.col=rep(1,5),
 line.lwd=1,x.lab='Years From No Evidence of Disease',
 y.lab='Patient ID',titl="Interval Event Chart",
 y.axis='custom',y.axis.custom.at=1:20,y.axis.custom.label=1:20,
 omi=c(2,0,1,.2),mai=c(.7,.7,.4,.07), mgp=c(2,.5,0), cex=.75)

legend(1.2,.2,pch=c(1,17,16,4,0),c("date of registration","date of recurrence",
"date of SPT","date of death","date last follow-up"),ncol=5, cex=.8)

##to produce Goldman Event Chart

event.chart(shin,subset.c=c(5,6,7,8,9), x.reference=5,
 x.julian=TRUE, x.scale=365, y.var=5, now.line=TRUE, y.var.type='d',
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),point.col=rep(1,5),
 line.lwd=1,x.lab='Years From No Evidence of Disease',
 y.lab='Calendar Date of NED',titl="Goldman Event Chart", adj=0.5,
 omi=c(2,0,1,.2), mai=c(.7,.7,.4,.07), mgp=c(2,.5,0), cex=.75)

legend(1.2,7340,pch=c(1,17,16,4,0),c("date of registration","date of recurrence",
"date of SPT","date of death","date last follow-up"),ncol=5, cex=.75)

##(a) to produce Calendar Event Chart by last F/U

event.chart(shin,subset.c=c(5,6,7,8,9), sort.ascending=FALSE,
 y.idlabels=1,legend.plot=FALSE,
 x.julian=FALSE,date.orig=c(month=1, day=1, year=1960),
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),point.col=rep(1,5),
 line.lwd=1,x.lab='Calendar Date',y.lab='Patient ID',
 titl=" (a) Calendar EC by Last F/U",las=1,
 omi=c(1,0,1,.2),mai=c(.6,.7,.4,.1), mgp=c(2,.5,0), cex=.8,
 sort.by=10, x.axis='custom',
 x.axis.custom.at=c(julian(1,1,1980), julian(1,1,1985), julian(1,1,1990),
 julian(1,1,1995)),x.axis.custom.labels=c("1/1/80","1/1/85","1/1/90","1/1/95"),
 x.lim.extend=c(0,450))

par(omi=c(1,0,1,.2),mai=c(.6,.7,.4,.1), mgp=c(2,.5,0))
abline(v=c(julian(1,1,1985),julian(1,1,1990)),lty=2,col=2,lwd=.1)

par(omi=c(0,0,0,0))
mtext(side=3,"Sorting and Alignment Extensions of Event Charts (EC)",line=-2)

## (b) to produce Interval Event Chart by F/U time

event.chart(shin,subset.c=c(5,6,7,8,9), x.reference=5, sort.ascending=FALSE,
 x.julian=TRUE, x.scale=365, y.idlabels=1,
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),point.col=rep(1,5),
 line.lwd=1,x.lab='Years From No Evidence of Disease',
 y.lab='Patient ID',titl=" (b) Interval EC by F/U Time",
 y.axis='custom',y.axis.custom.at=1:20,y.axis.custom.label=1:20,
 omi=c(.5,0,1,.2),mai=c(.6,.7,.4,.1), mgp=c(2,.5,0), cex=.7,
 sort.by=11)

par(omi=c(.5,0,1,.2),mai=c(.6,.7,.4,.1), mgp=c(2,.5,0))
abline(v=c(julian(1,27,1986)-julian(5,29,1981),
 julian(2,8,1991)-julian(8,21,1981))/365,lty=2,col=2,lwd=.1)

##(c) to produce Event Chart by time of recurrence

event.chart(shin,subset.c=c(5,6,7,8,9),x.reference=6,NA.rm=TRUE,
 sort.ascending=FALSE, x.julian=TRUE, x.scale=365, y.idlabels=1,
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),
 point.col=rep(1,5),
 line.lwd=1,x.lab='Years From Recurrence',
 y.lab='Patient ID',titl="(c) EC by Time to Recurrence",
 y.axis='custom',y.axis.custom.at=1:20,y.axis.custom.label=1:20,
 omi=c(1.8,0,.8,.2), mai=c(.4,.9,.3,.1), mgp=c(2,.4,0), cex=.7,
 sort.by=12)

## (d) to produce Event Chart by time to SPT

event.chart(shin,subset.c=c(5,6,7,8,9),x.reference=7,NA.rm=TRUE,
 sort.ascending=FALSE,x.julian=TRUE, x.scale=365, y.idlabels=1,
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),
 point.col=rep(1,5),line.lwd=1,x.lab='Years From SPT',
 y.lab='Patient ID',titl="(d) EC by Time to SPT",
 y.axis='custom',y.axis.custom.at=1:20,y.axis.custom.label=1:20,
 omi=c(1.5,0,.7,.2), mai=c(.6,.9,.6,.1), mgp=c(2,.4,0), cex=.7,
 sort.by=13)

##(a) to produce Event Chart p53 and On-Study time

event.chart(shin,subset.c=c(5,6,7,8,9), x.reference=5, sort.ascending=FALSE,
 x.julian=TRUE, x.scale=365, y.idlabels=1,
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),point.col=rep(1,5),
 x.lab='Years From No Evidence of Disease',
 y.lab='Patient ID',titl="(a) EC by p53 & On-Study Time",
 omi=c(1.5,0,.7,.2),mai=c(.6,.9,.6,.1), mgp=c(2,.4,0), cex=.65,
 sort.by=14, line.by=3, line.lty=c(2,1), line.lwd=c(1,1), line.col=c(1,1))

par(omi=c(0,0,0,0))
mtext(side=3,"Extensions of Event Charts for Showing the Covariate Effect:",Line=4)

## (b) to produce Event Chart by p53 and time to recur

event.chart(shin,subset.c=c(5,6,7,8,9), x.reference=6,
 NA.rm=TRUE,
 sort.ascending=FALSE,x.julian=TRUE, x.scale=365, y.idlabels=1,
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),point.col=rep(1,5),
 x.lab='Years From Recurrence',
 y.lab='Patient ID',titl="(b) EC by p53 & Time to Recur",
 omi=c(1.8,0,.8,.2), mai=c(.4,.9,.3,.1), mgp=c(2,.4,0), cex=.65,
 sort.by=15, line.by=3, line.lty=c(2,1), line.lwd=c(1,1), line.col=c(1,1))

## (c) to produce Event Chart by p53 and time to SPT

event.chart(shin,subset.c=c(5,6,7,8,9), x.reference=7, NA.rm=TRUE,
 sort.ascending=FALSE,x.julian=TRUE, x.scale=365, y.idlabels=1,
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),point.col=rep(1,5),
 x.lab='Years From SPT',
 y.lab='Patient ID',titl="(c) EC by p53 & Time to SPT",
 omi=c(2,0,1,.2), mai=c(.6,.7,.4,.1), mgp=c(2,.4,0), cex=.65,
 sort.by=16, line.by=3, line.lty=c(2,1), line.lwd=c(1,1), line.col=c(1,1))


##(d) to produce Event Chart by age coded by p53. Solid line
##for p53(+) cases and dashed line for p53(-)cases

set.seed(1993)
event.chart(shin,subset.c=c(5,6,7,8,9), x.reference=5, sort.ascending=FALSE,
 x.julian=TRUE, x.scale=365, y.var=2, y.jitter=TRUE,
 point.cex=c(.6,.8,.6,.8,.8),point.pch=c(1,17,16,4,0),point.col=rep(1,5),
 x.lab='Years From No Evidence of Disease',
 y.lab='Age',titl="(d) EC by Age coded by p53",y.lim.extend=c(2,3),
 y.axis='custom', y.axis.custom.at=c(40,50,60,70,80),
 y.axis.custom.label=c(40,50,60,70,80),omi=c(1,0,1,.2),mai=c(.2,.7,.4,.1),
 mgp=c(2,.5,0), cex=.65,line.by=3, line.lty=c(2,1), line.lwd=c(1,1),
 line.col=c(1,1))

par(omi=c(0.2,0,1,.2))
mtext(side=1,"Solid Line for p53(+) Cases and Dashed Line for p53(-) Cases",
line=3)

}

\references{
\item{} Dubin, J. A., Lee, J. J., Hess, K. R. (1997). "The Utility of Event
Charts,"
in Proceedings of the Biometrics Section, the American Statistical
Association, 196-201.
\item{} Lee, J. J., Hess, K. R. and Dubin, J. A. (2000) "Extensions and
Applications of Event Charts,"
The American Statistician 54, 63-70.
\item{} Shin, D.M., Lee, J.S., Lippman, S.M., Lee, J.J., Tu, Z.N.,
Choi, G., Heyne, K., Shin, H.J.C., Ro, J.Y. Goepfert, H., Hong, W.K., and
Hittelman, W.N. (1996) "p53 Expression: Predicting Recurrence and Second 
Primary
Tumors in Head and Neck Squamous Cell Carcinoma", Journal of National Cancer
Institute, 88, 519-529.
}
\keyword{chron}
\keyword{datasets}

