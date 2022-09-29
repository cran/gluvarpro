\name{magegvp}
\alias{magegvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{mean amplitude of glycemic excursions
%%  ~~function to do ... ~~
}
\description{
Mean amplitude of glycemic excursions is a measure of glycemic variability that calculates changes in blood glucose that exceed multiples of the standard deviation, and
that are in hypoglycemic and hyperglycemic values. It is based on the number of glycemic excursions, using glucose values that are above or below the limits of hypoglycemia and hyperglycemia.
}
%
\usage{
magegvp(x, t = 24, n = 1, type = "auto")
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
Numeric value with interval for calculating the measurement. Permitted values are 4, 6, 8, 12 and 24 hours. Default value of 24 hours.
}
\item{n}{
Numeric value with number of multiple values of standard deviation. Default value of 1.
}
\item{type}{
Character value with type of mage calculation. Permitted values are \emph{auto}, \emph{nardin2peak} and \emph{peak2nardin}. Default value is \emph{auto}.
}
}
%
\value{
A data frame containing the number of glycemic excursions ge and mage values.
}
%
\author{
Sergio Contador.
}
%
\references{
F. John Service, George D. Molnar, John W. Rosevear, Eugene Ackerman, Leal C. Gatewood, William F. Taylor. Mean Amplitude of Glycemic Excursions, a Measure of Diabetic Instability. Diabetes. Vol 9, N 19, 1970.
}
%
\examples{
data("datagvp1")
magegvp(datagvp1)
}
