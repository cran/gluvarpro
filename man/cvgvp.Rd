\name{cvgvp}
\alias{cvgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{percentage coefficient of variation
%%  ~~function to do ... ~~
}
\description{
Percentage coefficient of variation is a measure of glycemic variability defined as the ratio of the standard deviation to the mean.
}
%
\usage{
cvgvp(x, t = 24)
}
%
\arguments{
\item{x}{
Data-set with data frame format containing three columns:

  date: date of the observation with format \emph{yyyy/mm/dd}.

  time: time of the observation with 24 hour format \emph{hh:mm:ss}.

  glucose: glucose value of the observation.
}
\item{t}{
Numeric value with interval for calculating the measurement. Permitted values are 4, 6, 8, 12 and 24 hours. Default value of 24 hours.
}
}
%
\value{
A data frame containing the cv values.
}
%
\author{
Sergio Contador.
}
%
\references{
Everitt, Brian (1998). The Cambridge Dictionary of Statistics. Cambridge, UK New York: Cambridge University Press.
}
%
% \begin{References}\relax
% Boris P. Kovatchev, Erik Otto, Daniel Cox, Linda Gonder-Frederick, and William Clarke. Evaluation of a
% new measure of blood glucose variability in diabetes. Diabetes Care, 29(11):2433–2438, 2006.
% \end{References}
%
\seealso{
meangvp(x, t = 24)

sdgvp(x, t = 24)
}
%
\examples{
data("datagvp1")
cvgvp(datagvp1)
}
