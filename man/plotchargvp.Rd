\name{plotchargvp}
\alias{plotchargvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{bar plot of glucose characterization measures
%%  ~~function to do ... ~~
}
\description{
Generic function for create bar plot of glucose characterization measures.
}
%
\usage{
plotchargvp(x, text = FALSE)
}
%
\arguments{
\item{x}{
Data-set with data frame format containing one column:

  glucose: glucose value in [mg/dl].
}
\item{text}{
Logical value for plotting glucose characterization measures inside the bar plot. Default value is FALSE.
}
}
%
\author{
Sergio Contador.
}
%
% \begin{References}\relax
% Boris P. Kovatchev, Erik Otto, Daniel Cox, Linda Gonder-Frederick, and William Clarke. Evaluation of a
% new measure of blood glucose variability in diabetes. Diabetes Care, 29(11):2433–2438, 2006.
% \end{References}
%
\seealso{
chargvp(x)
}
%
\examples{
data("datagvp1")
plotchargvp(datagvp1)
}
