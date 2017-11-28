\name{Sum}
\alias{Sum}

\title{
Summation of n Piecewise Linear Fuzzy Numbers
}
\description{
This function is able to calculate the summation of a sample from Piecewise Linear Fuzzy Numbers (PLFNs).
}
\usage{
Sum(S.PLFN)
}
\arguments{
  \item{S.PLFN}{
A sample from Piecewise Linear Fuzzy Numbers (PLFNs), with n PLFNs. This sample is an array with \code{ dim=c(knot.n+2,2,n) }.
}
}
\value{
This function returned a Piecewise Linear Fuzzy Number.
}
\examples{

if(!require(FuzzyNumbers)){install.packages("FuzzyNumbers")}
library(FuzzyNumbers)

n=3; knot.n=4
Sample <- S.PLFN( n, knot.n,
			X.dist="pois", X.dist.par=5,
			slX.dist="exp", slX.dist.par=3,
			srX.dist="beta", srX.dist.par=c(1,3)
			)
Sample
S = Sum(Sample)

# For plotting random fuzzy sample:
xlim = c(min(Sample[knot.n+2,1,]),S["a4"])
  plot( cuts.to.PLFN(Sample[,,1]), type="o", xlim=xlim ) 
  plot( cuts.to.PLFN(Sample[,,2]), type="o", add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,3]), type="o", add=TRUE ) 
  abline( h=round((knot.n+1):0/(knot.n+1),4), lty=3, col="gray70") 

plot( S, type="b", add=TRUE, col=2, lwd=3 )
}

\keyword{ TriangularFuzzyNumber }
\keyword{ TrapezoidalFuzzyNumber }
\keyword{ FuzzyNumber }
\keyword{ PowerFuzzyNumber }
\keyword{ PLFN }
\keyword{ S.PLFN }
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
