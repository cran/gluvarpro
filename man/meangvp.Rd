\name{meangvp}
\alias{meangvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{arithmetic mean
%%  ~~function to do ... ~~
}
\description{
Arithmetic mean is an average measure of glycemia that calculates the sum of a set of data values divided by the number of data values in the data-set.
}
%
\usage{
meangvp(x, t = 24)
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
A data frame containing the mean values.
}
%
\author{
Sergio Contador.
}
%
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988). The New S Language. Wadsworth & Brooks/Cole.
}
%
% \begin{References}\relax
% Boris P. Kovatchev, Erik Otto, Daniel Cox, Linda Gonder-Frederick, and William Clarke. Evaluation of a
% new measure of blood glucose variability in diabetes. Diabetes Care, 29(11):2433–2438, 2006.
% \end{References}
%
\seealso{
cvgvp(x, t = 24)
}
%
\examples{
data("datagvp1")
meangvp(datagvp1)
}
