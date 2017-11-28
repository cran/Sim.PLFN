\name{cuts.to.PLFN}
\alias{cuts.to.PLFN}

\title{
Convert cuts to Piecewise Linear Fuzzy Number
}
\description{
This function can easily covert a cuts matrix into a Piecewise Linear Fuzzy Number.
}
\usage{
cuts.to.PLFN(cuts)
}
\arguments{
  \item{cuts}{
A matrices, with knot.n raw and 2 column, which contains all information about a Piecewise Linear Fuzzy Number.
}
}
\value{
This function returned a Piecewise Linear Fuzzy Number.
}
\references{
Gagolewski, M., Caha, J. (2015) FuzzyNumbers Package: Tools to deal with fuzzy numbers in R. R package version 0.4-1, https://cran.r-project.org/web/packages=FuzzyNumbers

Gagolewski, M., Caha, J. (2015) A guide to the FuzzyNumbers package for R (FuzzyNumbers version 0.4-1) http://FuzzyNumbers.rexamine.com
}
\examples{

if(!require(FuzzyNumbers)){install.packages("FuzzyNumbers")}
library(FuzzyNumbers)

knot.n = 2
T <- PLFN( knot.n=knot.n, 
	X.dist="norm", X.dist.par=c(0,1),
	slX.dist="exp", slX.dist.par=3,
	srX.dist="beta", srX.dist.par=c(1,3)
	)

T
plot(T, type="b")
CUTS <- PLFN.to.cuts(T, knot.n)
CUTS

T2 = cuts.to.PLFN(CUTS)
plot(T2, type="b", col=2, add=TRUE, lwd=3, lty=3)
}

\keyword{ TriangularFuzzyNumber }
\keyword{ TrapezoidalFuzzyNumber }
\keyword{ FuzzyNumber }
\keyword{ PowerFuzzyNumber }
\keyword{ PLFN }
\keyword{ S.PLFN }
\keyword{ cuts.to.PLFN }
\keyword{ PLFN.to.cuts }
\keyword{ Mean }
\keyword{ Var }
\keyword{ Sd }
\keyword{ CV }

\seealso{
DISTRIB
FuzzyNumbers
FuzzyNumbers.Ext.2
Calculator.LR.FNs
}
