CV <-
function(S.PLFN){
M <- Mean(S.PLFN)
s.d <- Var(S.PLFN)
cv = s.d / M
return( cv )
}
