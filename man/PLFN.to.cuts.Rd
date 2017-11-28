\name{PLFN.to.cuts}
\alias{PLFN.to.cuts}

\title{
Convert Piecewise Linear Fuzzy Number to a cuts matrix
}
\description{
This function can easily covert a Piecewise Linear Fuzzy Number (PLFN) into a cuts matrix.
}
\usage{
PLFN.to.cuts(P, knot.n)
}
\arguments{
  \item{P}{
A Piecewise Linear Fuzzy Number (PLFN).
}
  \item{knot.n}{
the number of knots; see package \code{FuzzyNumbers} for more details.
}
}
\value{
This function returned a matrix which contains core, support and \code{knot.n} cuts of the considered PLFN.
}
\examples{

knot.n = 2
T <- PLFN( knot.n=knot.n,
	X.dist="norm", X.dist.par=c(0,1),
	slX.dist="exp", slX.dist.par=3,
	srX.dist="beta", srX.dist.par=c(1,3)
	)

T
FuzzyNumbers::plot(T, type="b")
cuts = PLFN.to.cuts(T, knot.n)
cuts
cuts[,"L"]  #Lower bounds of cuts
cuts[,"U"]  #Upper bounds of cuts
cuts[2,]    #Or equivalently  cuts["0.6667",]
}

\keyword{ TriangularFuzzyNumber }
\keyword{ TrapezoidalFuzzyNumber }
\keyword{ FuzzyNumber }
\keyword{ PowerFuzzyNumber }
\keyword{ PLFN }
\keyword{ S.PLFN }
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
