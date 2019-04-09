\name{datagvp2}
\alias{datagvp2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{data-set from Medtronic continuous glucose monitoring.
%%  ~~function to do ... ~~
}
\description{
Data from type 1 diabetic patient acquired through Medtronic continuous glucose monitoring.
}
%
\usage{
data("datagvp2")
}
%
% \begin{Arguments}
% \begin{ldescription}
% \item[\code{x}]
% \end{ldescription}
% \end{Arguments}
%
\details{
Data-set with 36 complete days of data acquired with sampling time of 5 minutes. There are a total of 10368 observations, 10 with NA values of glucose.
}
%
\format{
Data-set with data frame format containing three columns:

  date: date of the observation with format \emph{yyyy/mm/dd}.

  time: time of the observation with 24 hour format \emph{hh:mm:ss}.

  glucose: glucose value of the observation in [mg/dl].
}
% %
% \begin{Author}\relax
% Sergio Contador.
%
% \end{Author}
%
%
\source{
School of Electrical Engineering and Computer Science, Ohio University, Ohio, United States.
}
%
\references{
Cindy Marling and Razvan Bunescu. The OhioT1DM Dataset for Blood Glucose Level Prediction - DRAFT.
}
%
\examples{
data("datagvp2")
datagvp2
}
