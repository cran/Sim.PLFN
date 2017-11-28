PLFN <-
function( knot.n,
type="PLFN",
X.dist, X.dist.par,
slX.dist, slX.dist.par,
srX.dist, srX.dist.par){

 x1 = rd(n=1, T.dist=X.dist, T.dist.par=X.dist.par)
 x2 = rd(n=1, T.dist=X.dist, T.dist.par=X.dist.par)
  sort.x.1 = min(x1,x2)
  sort.x.2 = max(x1,x2)
 slx = rd(n=1, T.dist=slX.dist, T.dist.par=slX.dist.par)
 srx = rd(n=1, T.dist=srX.dist, T.dist.par=srX.dist.par)

### For create a Triangular PLFN:
 if(type=="Tri") {
return( as.PiecewiseLinearFuzzyNumber(TrapezoidalFuzzyNumber(x1-slx,x1,x1,x1+srx), knot.n=knot.n) ) 
}

### For create a Trapezoidal PLFN:
 if(type=="Tra") {
return( as.PiecewiseLinearFuzzyNumber(TrapezoidalFuzzyNumber(sort.x.1-slx,sort.x.1,sort.x.2,sort.x.2+srx), knot.n=knot.n) ) 
}

### For create a PLFN:
 if(type=="PLFN"){
   cuts <- matrix(rep(NA, (knot.n+2)*2), ncol=2, byrow=FALSE, 
                   dimnames = list( round((knot.n+1):0/(knot.n+1),4), c("L", "U"))) 
  # Step 1: 
cuts[1,1] <- cuts[1,2] <- x1    #Core
cuts[knot.n+2,1] <- x1-slx        #First of support
cuts[knot.n+2,2] <- x1+srx        #End of support

  # Step 2: 
cuts[2:(knot.n+1),1] <- x1 - sort( Trunc( n=knot.n, T.dist=slX.dist, T.dist.par=slX.dist.par, L=0, R=slx ) ) 
cuts[2:(knot.n+1),2] <- x1 + sort( Trunc( n=knot.n, T.dist=srX.dist, T.dist.par=srX.dist.par, L=0, R=srx ) )

   P <- PiecewiseLinearFuzzyNumber(x1-slx,x1,x1,x1+srx,
     knot.n=knot.n, knot.alpha=round(1:(knot.n)/(knot.n+1),4),
     knot.left=cuts[(knot.n+1):2,1], 
     knot.right=cuts[2:(knot.n+1),2] )

    return( P ) 
  }

### For create a PLFI:
 if(type=="PLFI"){
   cuts <- matrix(rep(NA, (knot.n+2)*2), ncol=2, byrow=FALSE, 
                   dimnames = list( round((knot.n+1):0/(knot.n+1),4), c("L", "U"))) 
  # Step 1: 
cuts[1,1] <- sort.x.1    #Core[1]
cuts[1,2] <- sort.x.2   #Core[2]
cuts[knot.n+2,1] <- sort.x.1-slx    #First of support
cuts[knot.n+2,2] <- sort.x.2+srx    #End of support

  # Step 2: 
cuts[2:(knot.n+1),1] <- sort.x.1 - sort( Trunc( n=knot.n, T.dist=slX.dist, T.dist.par=slX.dist.par, L=0, R=slx ) ) 
cuts[2:(knot.n+1),2] <- sort.x.2 + sort( Trunc( n=knot.n, T.dist=srX.dist, T.dist.par=srX.dist.par, L=0, R=srx ) )

   P <- PiecewiseLinearFuzzyNumber(sort.x.1-slx,sort.x.1,sort.x.2,sort.x.2+srx,
     knot.n=knot.n, knot.alpha=round(1:(knot.n)/(knot.n+1),4),
     knot.left=cuts[(knot.n+1):2,1], 
     knot.right=cuts[2:(knot.n+1),2] )

    return( P ) 
  }

}
