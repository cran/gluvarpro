\name{aucgvp}
\alias{aucgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{area under curve
%%  ~~function to do ... ~~
}
\description{
Area under curve is an average measure of glycemia that quantifies the average exposure to hypoglycemia and hyperglycemia events. The integral trapezoidal cumulative function is used to calculate the area. The area under a curve between two points can be found by doing a definite integral between the two points. To find the area under the curve y = f(x) between x = a and x = b, integrate y = f(x) between the limits of a and b.
}
%
\usage{
aucgvp(x, t = 24, tdown = 70, tup = 180)
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
Interval for calculate measure. Permitted values are 4, 6, 8, 12 and 24 hours. Default value of 24 hours.
}
\item{tdown}{
Target range low with default value of 70 [mg/dl].
}
\item{tup}{
Target range high with default value of 180 [mg/dl].
}
}
%
\value{
A data frame containing the lauc, hauc and auc values.
}
%
\author{
Sergio Contador.
}
%
\references{
Jean-Pierre Le Floch, Philippe Escuyer, Eric Baudin, Dominique Baudon, and Leon Perlemuter. Blood
glucose area under the curve: Methodological aspects. Diabetes Care, 13(2):172–175, 1990.
}
%
\examples{
data("datagvp1")
aucgvp(datagvp1)
}
