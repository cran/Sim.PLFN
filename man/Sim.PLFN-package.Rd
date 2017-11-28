\name{Sim.PLFN-package}
\alias{Sim.PLFN-package}
\alias{Sim.PLFN}
\docType{package}
\title{
\packageTitle{Sim.PLFN}
}
\description{
This package is organized based on a special definition of fuzzy random variable and simulate fuzzy random variable by Piecewise Linear Fuzzy Numbers (PLFNs). 
Some important statistical functions are considered for obtaining the membership function of main statistics, such as mean, variance, summation, standard deviation and coefficient of variance. 
Some of applied advantages of 'Sim.PLFN' Package are:  
	(1) Easily generating / simulation a random sample of PLFN, 
	(2) drawing the membership functions of the simulated PLFNs or the membership function of the statistical result, and 
	(3) Considering the simulated PLFNs for arithmetic operation or importing into some statistical computation. 
}
\author{
Abbas Parchami
}

\references{
Coroianua, L., Gagolewski, M., Grzegorzewski, P. (2013) Nearest piecewiselinearapproximationoffuzzynumbers. Fuzzy Sets and Systems 233, 26-51.

Gagolewski, M., Caha, J. (2015) FuzzyNumbers Package: Tools to deal with fuzzy numbers in R. R package version 0.4-1, https://cran.r-project.org/web/packages=FuzzyNumbers

Gagolewski, M., Caha, J. (2015) A guide to the FuzzyNumbers package for R (FuzzyNumbers version 0.4-1) http://FuzzyNumbers.rexamine.com
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

\examples{
library(FuzzyNumbers)

# Example 1: Let  x ~~ ( X~N(0,.2) ; s_X^l~Exp(3) ; s_X^r~beta(1,3) )  
n=3; knot.n=3
Sample <- S.PLFN( n, knot.n, type="PLFI",
			X.dist="norm", X.dist.par=c(0,.2),
			slX.dist="exp", slX.dist.par=3,
			srX.dist="beta", srX.dist.par=c(1,3)
			)
Sample
Sample[,,3]

# For plotting random fuzzy sample:
xlim = c( min(Sample), max(Sample) )
  plot( cuts.to.PLFN(Sample[,,1]), type="o", xlim=xlim ) 
  plot( cuts.to.PLFN(Sample[,,2]), type="o", add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,3]), type="o", add=TRUE ) 
  abline( h=round((knot.n+1):0/(knot.n+1),4), lty=3, col="gray70") 



# Example 2: 
n=9; knot.n=9
Sample <- S.PLFN( n, knot.n, 
	X.dist="norm", X.dist.par=c(5,2),
	slX.dist="chisq", slX.dist.par=1,
	srX.dist="unif", srX.dist.par=c(0,3)
	)
Sample
Sample[,,9]

# For plotting random fuzzy sample:
xlim = c( min(Sample), max(Sample) )
  plot( cuts.to.PLFN(Sample[,,1]), type="b", col=1, xlim=xlim ) 
  plot( cuts.to.PLFN(Sample[,,2]), type="b", col=2, add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,3]), type="b", col=3, add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,4]), type="b", col=4, add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,5]), type="b", col=5, add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,6]), type="b", col=6, add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,7]), type="b", col=7, add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,8]), type="b", col=8, add=TRUE ) 
  plot( cuts.to.PLFN(Sample[,,9]), type="b", col=9, add=TRUE ) 

  abline( h=round((knot.n+1):0/(knot.n+1),4), lty=3, col="gray70") 



# Example 3: (Four Arithmatic Operations on PLFNs)

if(!require(FuzzyNumbers)){install.packages("FuzzyNumbers")}
library(FuzzyNumbers)

Sample <- S.PLFN( n=2, knot.n=9,
			X.dist="unif", X.dist.par=c(1,3),
			slX.dist="beta", slX.dist.par=c(4,3),
			srX.dist="beta", srX.dist.par=c(1/3,2/3)
			)
Sample

# For plotting random fuzzy sample:
xlim = c( min(Sample), max(Sample) )
  plot( cuts.to.PLFN(Sample[,,1]), type="b", xlim=xlim ) 
  plot( cuts.to.PLFN(Sample[,,2]), type="b", add=TRUE ) 
  abline( h=round((knot.n+1):0/(knot.n+1),4), lty=3, col="gray70") 


X1 = cuts.to.PLFN( Sample[,,1] )
X2 = cuts.to.PLFN( Sample[,,2] )

### Now working with four arithmatic operations +, _, * and / are simple as follows:

summ = X1 + X2 ;  summ
dist = X1 - X2 ;  dist
prod = X1 * X2 ;  prod
divi = X1 / X2 ;  divi

xlim = c(min(summ["a1"],dist["a1"],prod["a1"],divi["a1"]),
         max(summ["a4"],dist["a4"],prod["a4"],divi["a4"]) ) 

# For plotting random fuzzy sample:
  plot( cuts.to.PLFN(Sample[,,1]), type="b", xlim=xlim ) 
  plot( cuts.to.PLFN(Sample[,,2]), type="b", add=TRUE ) 
  abline( h=round((knot.n+1):0/(knot.n+1),4), lty=3, col="gray70") 

plot(summ, type="b", xlim=c(0, 12), add=TRUE, col=2, lwd=2)
plot(dist, type="b", xlim=c(0, 12), add=TRUE, col=3, lwd=2)
plot(prod, type="b", xlim=c(0, 12), add=TRUE, col=4, lwd=2)
plot(divi, type="b", xlim=c(0, 12), add=TRUE, col=5, lwd=2)

abline(v=c(X1["a2"],X2["a2"],summ["a2"],dist["a2"],prod["a2"],divi["a2"]), col=
       c(1,1,2,3,4,5), lty=3)
legend( "topright", c("X1 & X2", "X1 + X2", "X1 - X2", "X1 * X2", "X1 / X2"), col=1:5, 
       text.col = 1, lwd=2 )
}
