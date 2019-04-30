my.catalan <- function(n){
  1/(n+1)*choose(2*n,n)
}

my.nck <- function(n,k){
  1/n * choose(n,k) * choose(n,k-1)
}

n <- 6

nckout <- rep(0,n)
for(i in 1:n){
  nckout[i] <- my.nck(n,i)
}
sum(nckout)
my.catalan(n)
