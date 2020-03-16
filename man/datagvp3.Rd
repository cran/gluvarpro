\name{datagvp3}
\alias{datagvp3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{clean data-set from Abbott continuous glucose monitoring
%%  ~~function to do ... ~~
}
\description{
Data preprocessed from type 1 diabetic patient acquired through Abbott Freestyle Libre continuous glucose monitoring.
}
%
\usage{
data("datagvp3")
}
%
% \begin{Arguments}
% \begin{ldescription}
%
% \end{ldescription}
% \end{Arguments}
%
\details{
Data-set with 476 complete days of data acquired with sampling time of 15 minutes. There are 45696 observations in total.
}
%
\format{
Data-set with data frame format containing three columns:

  date: date of the observation with format \emph{yyyy/mm/dd}.

  time: time of the observation with 24 hour format \emph{hh:mm:ss}.

  glucose: glucose value of the observation in [mg/dl].
}
%
% \begin{Author}\relax
%
% \end{Author}
%
%
\source{
Hospital Universitario Principe de Asturias de Alcala de Henares, Madrid, Spain.
}
%
% \begin{References}\relax
%
% \end{References}
%
\examples{
data("datagvp3")
datagvp3
}
