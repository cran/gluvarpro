\name{plotboxgvp}
\alias{plotboxgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{box plot of glucose values and glucose variability measures
%%  ~~function to do ... ~~
}
\description{
Generic function for create box plot of given data points, plotting the inter-quartile range in a blue box with the median value as a horizontal line  and the mean value as a red point.
}
%
\usage{
plotboxgvp(x, var = "glucose")
}
%
\arguments{
\item{x}{
Data-set with data frame format containing three columns:

  date: date of the observation with format \emph{yyyy/mm/dd}.

  time: time of the observation with 24 hour format \emph{hh:mm:ss}.

  glucose or variability measure: glucose value [mg/dl] or glucose variability measure.
}
\item{var}{
Character value with variable for plotting. Permitted values are \emph{glucose}, \emph{adrr}, \emph{lauc}, \emph{hauc}, \emph{auc}, \emph{lbgi}, \emph{hbgi}, \emph{bgi}, \emph{conga}, \emph{cv}, \emph{iqr}, \emph{ji}, \emph{li}, \emph{ge}, \emph{lmage}, \emph{hmage}, \emph{mage}, \emph{mean}, \emph{mv}, \emph{sd}, \emph{lpstr}, \emph{hpstr}, \emph{npstr} and \emph{pstr}. Default value is \emph{glucose}.
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
plotboxgvp(datagvp1)
}
