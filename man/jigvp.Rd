\name{jigvp}
\alias{jigvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{j index
%%  ~~function to do ... ~~
}
\description{
J index is a measure of glycemic variability that combines information of the standard deviation and the mean, and excludes severe and persistent hypoglycemia.
}
%
\usage{
jigvp(x, t = 24)
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
}
%
\value{
A data frame containing the ji values.
}
%
\author{
Sergio Contador.
}
%
\references{
F. John Service. Glucose variability. Diabetes, 62(5):1398–1404, 2013.
}
%
\examples{
data("datagvp1")
jigvp(datagvp1)
}
