\name{Trunc}
\alias{Trunc}
\title{
Simulation from a Truncated Distribution
}
\description{
This function can simulation from a truncated distribution which is used for generating the left and right spreads of Piecewise Linear Fuzzy Numbers (PLFNs).
}
\usage{
Trunc(n, T.dist, T.dist.par, L = -Inf, R = Inf)
}
\arguments{
  \item{n}{
The sample size.
}
  \item{T.dist}{
The distribution name of random variable (which one needs its truncated version) is determined by characteristic element \code{T.dist}. The names of distributions is similar to \code{stats} package.
}
  \item{T.dist.par}{
A vector of distribution parameters (which one needs its truncated version) with considered ordering in \code{stats} package.
}
  \item{L}{
The left point of truncation of distribution.
}
  \item{R}{
The right point of truncation of distribution.
}
}
\details{
The goal of introducing \code{Trunc} function in this package is only using in \code{PLFN} and \code{S.PLFN} functions.
}
\value{
A vector of random data from the considered truncated distribution.
}

\examples{
# Truncated Normal Distribution: 

data1 = Trunc(n=10^4, T.dist="norm", T.dist.par=c(5,2), L=3, R=10)
hist(data1)

data2 = Trunc(n=1000, T.dist="chisq", T.dist.par=4, L=0, R=12)
hist(data2)

data3 = Trunc(n=10^4, T.dist="norm", T.dist.par=c(5,2), L=3) 
hist(data3)
}

\keyword{ PLFN }
\keyword{ S.PLFN }

\seealso{
DISTRIB
}
