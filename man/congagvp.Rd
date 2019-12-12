\name{congagvp}
\alias{congagvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{continuous overall net glycemic action
%%  ~~function to do ... ~~
}
\description{
Continuous overall net glycemic action is a measure of glycemic variability specifically developed for use on continuous glucose monitoring data. It is calculated as the standard deviation of the sum of the differences between a current observation and an observation n hours previously. Because conga does not require arbitrary glucose cutoffs or arbitrary defined rises and falls, it seems to be a more objective manner to define glucose variability than \emph{mvgvp} or \emph{magegvp}.
}
%
\usage{
congagvp(x, t = 24, ts = 15, h = 1)
}
%
\arguments{
\item{x}{
Data-set with data frame format containing three columns:

date: date of the observation with format \emph{yyyy/mm/dd}.

time: time of the observation with 24 hour format \emph{hh:mm:ss}.

glucose: glucose value of the observation in [mg/dl].
}
\item{t}{
Interval for calculating the measurement. Permitted values are 4, 6, 8, 12 and 24 hours. Default value of 24 hours.
}
\item{ts}{
Sampling time of glucose values. Permitted values are 5 and 15 minutes. Default value of 15 minutes.
}
\item{h}{
Type of measure calculated. Permitted values are from 1 to 24 hours, with differences of 1 hour. Default value of 1 hour.
}
}
%
\value{
A data frame containing the conga values.
}
%
\author{
Sergio Contador.
}
%
\references{
C.M. McDonnell, S.M. Donath, S.I. Vidmar, G.A. Werther, and F.J. Cameron. A novel approach to
continuous glucose analysis utilizing glycemic variation. Diabetes Technology \& Therapeutics, 7(2):253–263, 2005.
}
%
\examples{
data("datagvp1")
congagvp(datagvp1)
}
