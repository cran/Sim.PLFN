S.PLFN <-
function(n,
      knot.n,
      type="PLFN",
      X.dist, X.dist.par,
      slX.dist, slX.dist.par,
      srX.dist, srX.dist.par){

 PLFNs <- array(rep(NA, (knot.n+2)*2*n), dim=c(knot.n+2,2,n), 
                dimnames = list( round((knot.n+1):0/(knot.n+1),4), c("L", "U"), noquote(paste("X", 1:n, sep="")) )) 

 for(i in 1:n){
     X <- PLFN( knot.n=knot.n,
    type=type, 
    X.dist=X.dist, X.dist.par=X.dist.par,
    slX.dist=slX.dist, slX.dist.par=slX.dist.par,
    srX.dist=srX.dist, srX.dist.par=srX.dist.par
    )
    PLFNs[,,i] <- PLFN.to.cuts(X, knot.n)
    }

 return( PLFNs )
}
