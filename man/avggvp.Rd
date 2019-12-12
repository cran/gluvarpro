\name{avggvp}
\alias{avggvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{average mean
%%  ~~function to do ... ~~
}
\description{
Generic function for the arithmetic mean and the standard deviation.
}
%
\usage{
avggvp(x, var = "glucose", sd = FALSE)
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
Variable for calculate the mean value and the standard deviation. Permitted values are \emph{glucose}, \emph{adrr}, \emph{lauc}, \emph{hauc}, \emph{auc}, \emph{lbgi}, \emph{hbgi}, \emph{bgi}, \emph{conga}, \emph{cv}, \emph{iqr}, \emph{ji}, \emph{li}, \emph{ge}, \emph{lmage}, \emph{hmage}, \emph{mage}, \emph{mean}, \emph{mv}, \emph{sd}, \emph{lpstr}, \emph{hpstr}, \emph{npstr} and \emph{pstr}. Default value is \emph{glucose}.
}
\item{sd}{
Logical value for calculate the standard deviation. Default value is FALSE.
}
}
%
\value{
A numeric value containing the mean value or a character value containing the mean value and the standard deviation.
}
%
\author{
Sergio Contador.
}
%
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
}
%
\examples{
data("datagvp1")
avggvp(datagvp1)
}
