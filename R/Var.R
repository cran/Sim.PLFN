Var <-
function(S.PLFN){

n <- dim(S.PLFN)[3]
knot.n <- dim(S.PLFN)[1] - 2
M = Mean(S.PLFN)
Sq = 0

for(k in 1:n) {
Sq <- Sq + ( cuts.to.PLFN(S.PLFN[,,k]) - M )^2 
}

v <- (1/n) * Sq
return(v)
}
