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
magegvp(x, t = 24, n = 1)
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
\item{n}{
Number of multiple values of standard deviation with default value of 1.
}
}
%
\value{
A data frame containing the number of glycemic excursions ge, lmage, hmage and mage values.
}
%
\author{
Sergio Contador.
}
%
\references{
Seniz Sevimer Tuncan, Mehmet Uzunlulu, Ozge telci caklili, Hasan Huseyin Mutlu, and Aytekin Oguz.
Evaluation of the glycemic fluctuation as defined as the mean amplitude of glycemic excursion in hospitalized
patients with type 2 diabetes. 1, 11 2016.
}
%
\examples{
data("datagvp1")
magegvp(datagvp1)
}
