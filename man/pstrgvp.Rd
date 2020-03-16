\name{pstrgvp}
\alias{pstrgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{percentage spent below/above the target range
%%  ~~function to do ... ~~
}
\description{
Percentage spent below/above the target range is an average measure of glycemia that calculates the percentage of average time that the patient is in hypoglycemic and hyperglycemic ranges. This measure calculates the time-in-range measure (\emph{npstr}) but does not give more weight to extremely low values (\emph{lpstr}) nor to high values (\emph{hpstr}). Arbitrary target range may not be optimal, so the ranges must to be chosen careful.
}
%
\usage{
pstrgvp(x, t = 24, tdown = 70, tup = 180)
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
\item{tdown}{
Numeric value with target range low. Default value of 70 [mg/dl].
}
\item{tup}{
Numeric value with target range high. Default value of 180 [mg/dl].
}
}
%
\value{
A data frame containing the lpstr, hpstr, npstr and pstr values.
}
%
\author{
Sergio Contador.
}
%
\references{
Gabor Marics, Zsofia Lendvai, Csaba Lodi, Levente Koncz, David Zakarias, Gyorgy Schuster, Borbala
Mikos, Csaba Hermann, Attila J. Szabo, and Peter Toth-Heyn. Evaluation of an open access software for
calculating glucose variability parameters of a continuous glucose monitoring system applied at pediatric
intensive care unit. BioMedical Engineering OnLine, 14(1):37, Apr 2015.
}
%
\examples{
data("datagvp1")
pstrgvp(datagvp1)
}
