Mean <-
function(S.PLFN){
mean <- apply( S.PLFN, c(1,2), mean )
return( cuts.to.PLFN(mean) )
}
