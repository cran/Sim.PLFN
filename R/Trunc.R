Trunc <-
function(n, T.dist, T.dist.par, L=-Inf, R=Inf){
 Trunc.data = c()
 while(length(Trunc.data)<n){
   x = rd(1, T.dist=T.dist, T.dist.par=T.dist.par)
   if(L<x & x<R)  Trunc.data=c(Trunc.data,x)
   }
 return(Trunc.data)
}
