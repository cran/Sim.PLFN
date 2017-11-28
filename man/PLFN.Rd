\name{PLFN}
\alias{PLFN}

\title{
Simulate a random Piecewise Linear Fuzzy Number
}
\description{
This function is able to produce / simulate a Piecewise Linear Fuzzy Number (PLFN).
}
\usage{
PLFN(knot.n, type = "PLFN", X.dist, X.dist.par, slX.dist, slX.dist.par, 
     srX.dist, srX.dist.par)
}
\arguments{
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
# Example:  Let x ~~ ( X~N(3,0.2) ; s_X^l~Exp(3) ; s_X^r~U(0,2) )  

if(!require(FuzzyNumbers)){install.packages("FuzzyNumbers")}
library(FuzzyNumbers)

knot.n = 2
P <- PLFN( knot.n, type="Tri",
	     X.dist="norm", X.dist.par=c(3,.2),
	     slX.dist="exp", slX.dist.par=3,
	     srX.dist="unif", srX.dist.par=c(0,2)
	    )
P
plot(P, lwd=3, type="b")
abline( h=round((knot.n+1):0/(knot.n+1),4), v=alphacut(P, round((knot.n+1):0/(knot.n+1),4) ),
        lty=3, col=2  ) 


P <- PLFN( knot.n, type="Tra",
	     X.dist="norm", X.dist.par=c(3,1),
	     slX.dist="exp", slX.dist.par=3,
	     srX.dist="unif", srX.dist.par=c(0,2)
	    )
plot(P, lwd=3, type="b")
abline( h=round((knot.n+1):0/(knot.n+1),4), v=alphacut(P, round((knot.n+1):0/(knot.n+1),4) ),
                lty=3, col=2  ) 


knot.n = 2
P <- PLFN( knot.n,			#Defult:  type="PLFN" 
	     X.dist="norm", X.dist.par=c(3,1),
	     slX.dist="exp", slX.dist.par=3,
	     srX.dist="unif", srX.dist.par=c(0,2)
	    )
plot(P, lwd=3)
abline( h=round((knot.n+1):0/(knot.n+1),4), v=alphacut(P, round((knot.n+1):0/(knot.n+1),4) ),
        lty=3, col=2  ) 
#Try once again by knot.n=10


knot.n = 2
P <- PLFN( knot.n, type="PLFI",
	     X.dist="norm", X.dist.par=c(3,1),
	     slX.dist="exp", slX.dist.par=3,
	     srX.dist="unif", srX.dist.par=c(0,2)
	    )
plot(P, lwd=3, type="b")
abline( h=round((knot.n+1):0/(knot.n+1),4), v=alphacut(P, round((knot.n+1):0/(knot.n+1),4) ),
        lty=3, col=2  ) 

plot(P, type="b", col=2, lty=1, lwd=3, add=FALSE)
# Some of possible types are:
  #  "p" for points, 
  #  "l" for lines, 
  #  "b" for both, 
  #  "c" for the lines part alone of "b", 
  #  "o" for both over plotted, 
  #  "h" for histogram like (or high-density) vertical lines, 
  #  "s" for stair steps, 
  #  "S" for other steps, 
  #  "n" for no plotting. 


P
P["a1"] #First point of support
P["a2"] #First point of core
P["a3"] #End point of core
P["a4"] #End point of support

core(P)
supp(P)
alphacut(P, 0.5)
abline(h=.5, lty=3)

evaluate(P, 3.5)
round( evaluate(P, seq(0,4, by=.5)), 2)
}

\keyword{ TriangularFuzzyNumber }
\keyword{ TrapezoidalFuzzyNumber }
\keyword{ FuzzyNumber }
\keyword{ PowerFuzzyNumber }
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
