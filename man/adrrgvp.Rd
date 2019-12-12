\name{adrrgvp}
\alias{adrrgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{average daily risk range
%%  ~~function to do ... ~~
}
\description{
Average daily risk range is a measure of glycemic risk that is based on risk values obtained from glucose levels that are mathematically transformed to give equal weight to hyperglycemic and hypoglycemic excursions. The \emph{adrrgvp} is scored based on risk categories: Low risk, [0,20); moderate risk, [20,40); and high risk, 40 and above.
}
%
\usage{
adrrgvp(x, t = 24)
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
A data frame containing the adrr values.
}
%
\author{
Sergio Contador.
}
%
\references{
Boris P. Kovatchev, Erik Otto, Daniel Cox, Linda Gonder-Frederick, and William Clarke. Evaluation of a
new measure of blood glucose variability in diabetes. Diabetes Care, 29(11):2433–2438, 2006.
}
%
\seealso{
bgigvp(x, t = 24)
}
%
\examples{
data("datagvp1")
adrrgvp(datagvp1)
}
