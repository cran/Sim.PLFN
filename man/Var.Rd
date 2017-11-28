\name{Var}
\alias{Var}

\title{
Variance of n Piecewise Linear Fuzzy Numbers
}
\description{
This function is able to calculate the variance of a sample with size \code{n} from Piecewise Linear Fuzzy Numbers (PLFNs).
}
\usage{
Var(S.PLFN)
}
\arguments{
  \item{S.PLFN}{
A sample from Piecewise Linear Fuzzy Numbers (PLFNs), with n PLFNs. This sample is an array with \code{ dim=c(knot.n+2,2,n) }.
}
}
\value{
This function returned a Piecewise Linear Fuzzy Number as the variance of PLFNs.
}
\examples{

if(!require(FuzzyNumbers)){install.packages("FuzzyNumbers")}
library(FuzzyNumbers)

n=3; knot.n=4
Sample <- S.PLFN( n, knot.n,
			X.dist="norm", X.dist.par=c(3,2),
			slX.dist="exp", slX.dist.par=3,
			srX.dist="beta", srX.dist.par=c(1,3)
			)
Sample

# For plotting random fuzzy sample:
xlim = c( min(Sample), max(Sample) )
  plot( cuts.to.PLFN(Sample[,,1]), type="o", xlim=xlim ) 
  plot( cuts.to.PLFN(Sample[,,2]), type="o", add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,3]), type="o", add=TRUE ) 
  abline( h=round((knot.n+1):0/(knot.n+1),4), lty=3, col="gray70") 


plot(Mean(Sample), col=4, lwd=2, add=TRUE, type="b", pch=2)
S2 = Var(Sample)
S2
PLFN.to.cuts(S2, knot.n)
plot(S2, col=2, lwd=2, add=TRUE, type="b", pch=3)
}

\keyword{ TriangularFuzzyNumber }
\keyword{ TrapezoidalFuzzyNumber }
\keyword{ FuzzyNumber }
\keyword{ PowerFuzzyNumber }
\keyword{ PLFN }
\keyword{ S.PLFN }

\keyword{ PLFN.to.cuts }
\keyword{ Mean }
\keyword{ Sd }
\keyword{ CV }
\keyword{ Sum }

\seealso{
DISTRIB
FuzzyNumbers
FuzzyNumbers.Ext.2
Calculator.LR.FNs
}
