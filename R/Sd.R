Sd <-
function(S.PLFN){
s2 <-Var(S.PLFN)
s.d = FuzzyNumbers::fapply( s2, function(x) sqrt(x) )  #  Standard Deviation
return( s.d )
}
