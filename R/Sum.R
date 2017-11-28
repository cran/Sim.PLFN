Sum <-
function(S.PLFN){
s <- apply( S.PLFN, c(1,2), sum )
return( cuts.to.PLFN(s) )
}
