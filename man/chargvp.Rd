\name{chargvp}
\alias{chargvp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{glucose characterization measures
%%  ~~function to do ... ~~
}
\description{
Generic function to calculate different glucose characterization measures. It calculates the average glucose, the standard deviation, and the percentages of time where the data has very low glucose levels (<54 [mg/dl]), low ([54,70) [mg/dl]), in range ([70,180] and [70,140] [mg/dl]), high (>180 [mg/dl]), and very high (>250 [mg/dl]), as defined in the recommendations of ADA (American Diabetes Association).
}
%
\usage{
chargvp(x)
}
%
\arguments{
\item{x}{
Data-set with data frame format containing one column:

  glucose: glucose value of the observation in [mg/dl].

}
}
%
\value{
A data frame containing glucose characterization measures.
}
%
\author{
Sergio Contador.
}
%
\references{
Thomas Danne, Revital Nimri, Tadej Battelino, Richard M. Bergenstal, Kelly L. Close, J. Hans DeVries, SatishGarg, Lutz  Heinemann, Irl Hirsch, Stephanie A. Amiel, Roy Beck,  Emanuele Bosi, Bruce Buckingham, ClaudioCobelli, Eyal Dassau, Francis J. Doyle, Simon Heller, Roman Hovorka, Weiping Jia, Tim Jones, Olga Kordonouri,Boris Kovatchev, Aaron Kowalski, Lori Laffel, David Maahs, Helen R. Murphy, Kirsten Nørgaard, Christopher G.Parkin, Eric Renard, Banshi Saboo, Mauro Scharf, William V. Tamborlane, Stuart A. Weinzimer, and Moshe Phillip.International consensus on use of continuous glucose monitoring.Diabetes Care, 40(12):1631–1640, 2017.
}
%
\seealso{
plotchargvp(x, text = FALSE)
}
%
\examples{
data("datagvp1")
chargvp(datagvp1)
}
