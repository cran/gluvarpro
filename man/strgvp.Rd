\name{strgvp}
\alias{strgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{information about data-set
%%  ~~function to do ... ~~
}
\description{
Generic function for printing information about data-set. Includes information about number of days of data, number of registers, number of glucose values, number of glucose values with NA (missing values) and range of glucose values. It shows the absolute values and the relative values for each day, containing the year, month, day and time.
}
%
\usage{
strgvp(x)
}
%
\arguments{
\item{x}{
Data-set with data frame format containing three columns:

  date: date of the observation with format \emph{yyyy/mm/dd}.

  time: time of the observation with 24 hour format \emph{hh:mm:ss}.

  glucose: glucose value of the observation.
}
}
%
\value{
A list containing the absolute values and the relative values for each day with the information of the data-set.
}
%
\author{
Sergio Contador.
}
%
\examples{
data("datagvp1")
strgvp(datagvp1)
}
