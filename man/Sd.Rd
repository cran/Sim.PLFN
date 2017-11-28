\name{Sd}
\alias{Sd}

\title{
Standard of Deviation of n Piecewise Linear Fuzzy Numbers
}
\description{
This function is able to calculate the Standard of Deviation for a sample with size \code{n} from Piecewise Linear Fuzzy Numbers (PLFNs).
}
\usage{
Sd(S.PLFN)
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
			X.dist="norm", X.dist.par=c(3,2),
			slX.dist="exp", slX.dist.par=3,
			srX.dist="beta", srX.dist.par=c(1,3)
			)
Sample

# For plotting random fuzzy sample:
xlim = c(0, max(Sample[knot.n+2,2,]))
  plot( cuts.to.PLFN(Sample[,,1]), type="b", xlim=xlim ) 
  plot( cuts.to.PLFN(Sample[,,2]), type="b", add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,3]), type="b", add=TRUE ) 
  abline( h=round((knot.n+1):0/(knot.n+1),4), lty=3, col="gray70") 


plot(Mean(Sample), col=4, lwd=2, add=TRUE, type="b")
plot(Var(Sample), col=3, lwd=2, add=TRUE, type="b")

S = Sd(Sample)
S
PLFN.to.cuts(S, knot.n)
plot(S, col=2, lwd=2, add=TRUE, type="b")
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
\keyword{ CV }

\seealso{
DISTRIB
FuzzyNumbers
FuzzyNumbers.Ext.2
Calculator.LR.FNs
}
