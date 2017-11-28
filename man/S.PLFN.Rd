\name{S.PLFN}
\alias{S.PLFN}

\title{
Simulate a random sample from Piecewise Linear Fuzzy Numbers
}
\description{
This function is able to produce / simulate a random sample from Piecewise Linear Fuzzy Numbers (PLFNs).
}
\usage{
S.PLFN(n, knot.n, type = "PLFN", X.dist, X.dist.par, slX.dist, slX.dist.par,
       srX.dist, srX.dist.par )
}
\arguments{
  \item{n}{
the size of random sample of PLSNs.
}
  \item{knot.n}{
the number of knots; see package \code{FuzzyNumbers} for more details.
}
  \item{type}{
The possible values of this argument is	\code{ type = c("Tri", "Tra", "PLFN", "PLFI") }. 
In other words, this function returned one of following fuzzy numbers: 
(1) Triangular Fuzzy Number ( when \code{ type = "Tri" }),
(2) Trapezoidal Fuzzy Number ( when \code{ type = "Tra" }),
(3) Piecewise Linear Fuzzy Number ( when \code{ type = "PLFN" }), and
(4) Piecewise Linear Fuzzy Interval ( when \code{ type = "PLFI" }).
}
  \item{X.dist}{
The distribution name of the random variable (for simulate the core of random fuzzy number) is determined by characteristic element \code{T.dist}. The names of distributions is similar to \code{stats} package.
}
  \item{X.dist.par}{
A vector of distribution parameters (for simulate the core of random fuzzy number) with considered ordering in \code{stats} package.
}
  \item{slX.dist}{
The distribution name of the random variable (for simulate the left spread value of random fuzzy number) is determined by characteristic element \code{T.dist}. The names of distributions is similar to \code{stats} package.
}
  \item{slX.dist.par}{
A vector of distribution parameters (for simulate the left spread value of random fuzzy number) with considered ordering in \code{stats} package.
}
  \item{srX.dist}{
The distribution name of the random variable (for simulate the right spread value of random fuzzy number) is determined by characteristic element \code{T.dist}. The names of distributions is similar to \code{stats} package.
}
  \item{srX.dist.par}{
A vector of distribution parameters (for simulate the right spread value of random fuzzy number) with considered ordering in \code{stats} package.
}
}
\value{
Considering the \code{type} argument, this function returned/simulate/create one of following fuzzy numbers: 
(1) Triangular Fuzzy Number,
(2) Trapezoidal Fuzzy Number,
(3) Piecewise Linear Fuzzy Number, and
(4) Piecewise Linear Fuzzy Interval.
}
\references{
Gagolewski, M., Caha, J. (2015) FuzzyNumbers Package: Tools to deal with fuzzy numbers in R. R package version 0.4-1, https://cran.r-project.org/web/packages=FuzzyNumbers

Gagolewski, M., Caha, J. (2015) A guide to the FuzzyNumbers package for R (FuzzyNumbers version 0.4-1) http://FuzzyNumbers.rexamine.com
}
\examples{
library(FuzzyNumbers)

# Let  x ~~ ( X~N(0,1) ; s_X^l~Exp(3) ; s_X^r~beta(1,3) )  

n=3; knot.n=3

Sam <- S.PLFN( n=3, knot.n=4, type="Tra",
	X.dist="norm", X.dist.par=c(0,1),
	slX.dist="exp", slX.dist.par=3,
	srX.dist="beta", srX.dist.par=c(1,3)
	)

Sam
Sam[,,"X3"]

# For plotting random fuzzy sample:
xlim = c( min(Sam), max(Sam) )
  plot( cuts.to.PLFN(Sam[,,1]), type="b", col=1, xlim=xlim ) 
  plot( cuts.to.PLFN(Sam[,,2]), type="b", col=2, add=TRUE ) 
  plot( cuts.to.PLFN(Sam[,,3]), type="b", col=3, add=TRUE ) 
  abline( h=round((knot.n+1):0/(knot.n+1),4), lty=3, col="gray70") 
}

\keyword{ TriangularFuzzyNumber }
\keyword{ TrapezoidalFuzzyNumber }
\keyword{ FuzzyNumber }
\keyword{ PowerFuzzyNumber }
\keyword{ PLFN }
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
