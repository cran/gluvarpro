\name{plotmgvp}
\alias{plotmgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot multiple glucose variability measures
%%  ~~function to do ... ~~
}
\description{
Function for plotting given data points arranging multiple grobs on a draw.
}
%
\usage{
plotmgvp(x, col = FALSE, var = "auc")
}
%
\arguments{
\item{x}{
Data-set with data frame format containing five or six columns:

  date: date of the observation with format \emph{yyyy/mm/dd}.

  time: time of the observation with 24 hour format \emph{hh:mm:ss}.

  variability measure: glucose variability measure.
}
\item{col}{
Logical value for plotting data points with different colours. If data-set contains more than one day, it can be selected different colors (TRUE) for each day of data or one color (FALSE). Default value is FALSE.
}
\item{var}{
Character value with variable for plotting. Permitted values are \emph{auc}, \emph{bgi}, \emph{mage}, and \emph{pstr}. Default value is \emph{auc}.
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
\examples{
data("datagvp1")
x <- aucgvp(datagvp1)
plotmgvp(x)
}
