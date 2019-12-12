\name{bgigvp}
\alias{bgigvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{blood glucose index
%%  ~~function to do ... ~~
}
\description{
Blood glucose index is a measure of glycemic risk based on the same normalizing transformation as the \emph{adrrgvp} measure but is specifically designed to be sensitive to hypoglycemia (\emph{lbgi}) and hyperglycemia (\emph{hbgi}), respectively, and to have zero correlation with their opposite ranges on the blood glucose scale. The \emph{lbgi} and \emph{hbgi} are scored based on risk categories: Low risk, [0,2.5); moderate risk, [2.5,5); and high risk, 5 and above.
}
%
\usage{
bgigvp(x, t = 24)
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
}
%
\value{
A data frame containing the lbgi, hbgi and bgi values.
}
%
\author{
Sergio Contador.
}
%
\references{
Boris P. Kovatchev, Erik Otto, Daniel Cox, Linda Gonder-Frederick, and William Clarke. Evaluation of a
new measure of blood glucose variability in diabetes. Diabetes Care, 29(11):2433–2438, 2006.
}
%
%
\seealso{
adrrgvp(x, t = 24)
}
%
\examples{
data("datagvp1")
bgigvp(datagvp1)
}
