\name{iqrgvp}
\alias{iqrgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{igrgvp (inter-quartile range).
%%  ~~function to do ... ~~
}
\description{
Inter-quartile range is a measure of glycemic variability defined as the difference between 75th and 25th percentiles.
}
%
\usage{
iqrgvp(x, t = 24)
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
A data frame containing the iqr values.
}
%
\author{
Sergio Contador.
}
%
\references{
Tukey, J. W. (1977). Exploratory Data Analysis. Reading: Addison-Wesley.
}
%
% \begin{References}\relax
% Boris P. Kovatchev, Erik Otto, Daniel Cox, Linda Gonder-Frederick, and William Clarke. Evaluation of a
% new measure of blood glucose variability in diabetes. Diabetes Care, 29(11):2433–2438, 2006.
% \end{References}
%
\examples{
data("datagvp1")
iqrgvp(datagvp1)
}
