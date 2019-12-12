\name{fillgvp}
\alias{fillgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fill glucose values from continuous glucose monitoring
%%  ~~function to do ... ~~
}
\description{
Generic functions for replacing NA values with interpolated values, performing linear or cubic spline interpolation of given data points.
}
%
\usage{
fillgvp(x, method = "linear", n = 4)
}
%
\arguments{
\item{x}{
Data-set with data frame format containing one column:

glucose: glucose value of the observation in [mg/dl].
}
\item{method}{
Missing values (NAs) are replaced by linear interpolation via \emph{linear} or cubic spline interpolation via \emph{cubic}, respectively. Default value is \emph{linear}.
}
\item{n}{
Maximum number of consecutive NAs to fill. Any longer gaps will be left unchanged. Default value is 4.
}
}
%
\value{
A data frame containing glucose values.
}
%
\author{
Sergio Contador.
}
%
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988). The New S Language. Wadsworth & Brooks/Cole.

Dougherty, R. L., Edelman, A. and Hyman, J. M. (1989). Positivity-, monotonicity-, or convexity-preserving cubic and quintic Hermite interpolation. Mathematics of Computation, 52, 471–494.
}
%
% \begin{References}\relax
% Boris P. Kovatchev, Erik Otto, Daniel Cox, Linda Gonder-Frederick, and William Clarke. Evaluation of a
% new measure of blood glucose variability in diabetes. Diabetes Care, 29(11):2433–2438, 2006.
% \end{References}
%
\examples{
data("datagvp1")
fillgvp(datagvp1)
}
