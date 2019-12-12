\name{mvgvp}
\alias{mvgvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{m value
%%  ~~function to do ... ~~
}
\description{
M value is a measure of glycemic variability that quantifies the glycemic control of diabetic patients. It is a measure of the stability of the glucose excursions in comparison with an ideal glucose default value of 6.6 [mmol/l]-120 [mg/dl]. The m value is zero in healthy controls, rising with increasing glycemic variability or poorer glycemic control, making it difficult to distinguish between patients with either high mean glucose or high glucose variability. Moreover, because hypoglycemia has a greater impact on the m value than hyperglycemia, it is more a clinical than a mathematical indicator of glycemic control.
}
%
\usage{
mvgvp(x, t = 24, gi = 120)
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
\item{gi}{
Ideal glucose value with default value of 120 [mg/dl].
}
}
%
\value{
A data frame containing the mv values.
}
%
\author{
Sergio Contador.
}
%
\references{
Sarah E. Siegelaar, Frits Holleman, Joost B. L. Hoekstra, and J. Hans DeVries. Glucose variability; does
it matter? Endocrine Reviews, 31(2):171–182, 2010.
}
%
\examples{
data("datagvp1")
mvgvp(datagvp1)
}
