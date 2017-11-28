cuts.to.PLFN <-
function(cuts){
  knot.n = dim(cuts)[1] - 2
  P <- PiecewiseLinearFuzzyNumber(cuts[knot.n+2,1], cuts[1,1], cuts[1,2], cuts[knot.n+2,2],
       knot.n=knot.n, knot.alpha=round(1:(knot.n)/(knot.n+1),4),
       knot.left=cuts[(knot.n+1):2,1], 
       knot.right=cuts[2:(knot.n+1),2] )

  return(P)
}
