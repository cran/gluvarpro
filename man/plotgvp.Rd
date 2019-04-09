\name{plotgvp}
\alias{plotgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotgvp (plot glucose values and glucose variability measures).
%%  ~~function to do ... ~~
}
\description{
Generic function for plotting given data points.
}
%
\usage{
plotgvp(x, col = "one", var = "glucose")
}
%
\arguments{
\item{x}{
Data-set with data frame format containing three columns:

date: date of the observation with format \emph{yyyy/mm/dd}.

time: time of the observation with 24 hour format \emph{hh:mm:ss}.

glucose or variability measure: glucose value [mg/dl] or glucose variability measure.
}
\item{col}{
Color for plotting data points. If data-set contains more than one day, it can be selected different colors (\emph{multiple}) for each day of data or one color (\emph{one}). Default value is \emph{one} color.
}
\item{var}{
Variable for plotting. Permitted values are \emph{glucose}, \emph{adrr}, \emph{lauc}, \emph{hauc}, \emph{auc}, \emph{lbgi}, \emph{hbgi}, \emph{bgi}, \emph{conga}, \emph{cv}, \emph{iqr}, \emph{ji}, \emph{li}, \emph{ge}, \emph{lmage}, \emph{hmage}, \emph{mage}, \emph{mean}, \emph{mv}, \emph{sd}, \emph{lpstr}, \emph{hpstr}, \emph{npstr} and \emph{pstr}. Default value is \emph{glucose}.
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
plotgvp(datagvp1)
}
