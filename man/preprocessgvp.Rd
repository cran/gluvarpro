\name{preprocessgvp}
\alias{preprocessgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{preprocessing raw data from continuous glucose monitoring
%%  ~~function to do ... ~~
}
\description{
Generic function for preprocessing raw data from continuous glucose monitoring with glucose values collected with sampling time of 5 or 15 minutes each. The function is specially designed for preprocessing data from \emph{Medtronic} and \emph{Abbott} continuous glucose monitoring.
}
%
\usage{
preprocessgvp(x, dp = 2, tp = 3, gp = 31, ts = 5, df = "yyyy/mm/dd",
        tf = "hh:mm:ss", all = FALSE, type = "normal")
}
%
\arguments{
\item{x}{
Data-set with data frame format containing at least two or three columns:

  date: date of the observation with only the date or the date plus the time.

  time: time of the observation with 24 hour format.

  glucose: glucose value of the observation.
}
\item{dp}{
Numeric value with column position where the variable date is. Default value of 2.
}
\item{tp}{
Numeric value with column position where the variable time is. Default value of 3.
}
\item{gp}{
Numeric value with column position where the variable glucose is. Default value of 31.
}
\item{ts}{
Numeric value with sampling time of glucose values. Permitted values are 5 and 15 minutes. Default value of 5 minutes.
}
\item{df}{
Character value with the format of variable date. Permitted values are \emph{yyyy/mm/dd} and \emph{dd/mm/yyyy}. Default value is \emph{yyyy/mm/dd}.
}
\item{tf}{
Character value with the format of variable time. Permitted values are \emph{hh:mm:ss} and \emph{hh:mm}. Default value is \emph{hh:mm:ss}.
}
\item{all}{
Logical value for showing all columns of data frame (TRUE) or only columns for variables date, time and glucose (FALSE). Default value is FALSE.
}
\item{type}{
Character value to control the different types of prerpocessing. To preserve time slots use \emph{normal}. For round time slots to 5 or 15 minutes between registers use \emph{round}. For round time to slots 5 or 15 minutes between registers and complete missing time slots use \emph{complete}. Default value is \emph{normal}.
}
}
%
\value{
A data frame containing date, time, glucose values and all other variables from the original data-set.
}
%
\author{
Sergio Contador.
}
%
\seealso{
datagvp4
}
\examples{
data("datagvp4")
preprocessgvp(datagvp4)
}
