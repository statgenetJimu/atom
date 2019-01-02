# R
my.f <- function(x){
   return(runif(x))
}
x <- 10
my.f(x)

my.f2 <- function(y){
  return(rnorm(y))
}
my.f2(x)
plot(my.f2(x))
