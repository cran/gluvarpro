\name{ligvp}
\alias{ligvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{lability index
%%  ~~function to do ... ~~
}
\description{
Lability index is a measure of glycemic variability that evaluates the metabolic lability and its possible improvement in patients candidates for islet transplantation.
}
%
\usage{
ligvp(x, t = 24, ts = 5)
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
Numeric value with interval for calculating the measurement. Permitted values are 4, 6, 8, 12 and 24 hours. Default value of 24 hours.
}
\item{ts}{
Numeric value with sampling time of glucose values. Permitted values are 5 and 15 minutes. Default value of 5 minutes.
}
}
%
\value{
A data frame containing the li values.
}
%
\author{
Sergio Contador.
}
%
\references{
Edmond A. Ryan, Tami Shandro, Kristy Green, Breay W. Paty, Peter A. Senior, David Bigam, A.M. James
Shapiro, and Marie-Christine Vantyghem. Assessment of the severity of hypoglycemia and glycemic lability
in type 1 diabetic subjects undergoing islet transplantation. Diabetes, 53(4):955–962, 2004.
}
%
\examples{
data("datagvp1")
ligvp(datagvp1)
}
