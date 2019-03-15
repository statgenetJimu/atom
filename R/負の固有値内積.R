x <- t <- seq(from=-2,to=2,length=50)
xt <- expand.grid(x,t)

n <- length(xt[,1])
#d <- length(xt[1,])
s <- sample(1:n,1)
#Sigma <- diag(rep(1,d-1),-1)

my.ipM <- function(x,y,s){
	sum(x*x*s) + sum(y*y*s) - 2*sum(x*y*s)
}
my.d <- function(x1,x2){
	s <- rep(1,length(x1))
	s[length(s)] <- -1
	return(my.ipM(x1,x2,s))
}
my.d.s <- function(x1,x2,s){
	#s <- rep(1,length(x1))
	#s[length(s)] <- -1
	return(my.ipM(x1,x2,s))
}

s1 <- c(0,0)
s2 <- c(1,0)

d1 <- apply(xt,1,my.d,s1)
d2 <- apply(xt,1,my.d,s2)

d1. <- d1
d1.[which(d1. < 0)] <- 0

col1 <- ceiling( sqrt(d1.)/max(sqrt(d1.)) * 10 ) 
d2. <- d2
d2.[which(d2. < 0)] <- 0

col2 <- ceiling( sqrt(d2.)/max(sqrt(d2.)) * 10 ) 
#col[which(d < 0)] <- 0
par(mfcol=c(1,2))
plot(xt,col=col1,pch=20)
points(s1[1],s1[2],pch=20,cex=3)
plot(xt,col=col2,pch=20)
points(s2[1],s2[2],pch=20,cex=3)

s3 <- c(2,0.5)

my.d(s1,s2)
my.d(s1,s3)
my.d(s2,s3)


n.pt <- 1000
d <- 3
X <- matrix(runif(n.pt*d),ncol=d)
Sigma <- diag(c(rep(1,d-1),-1))

#X <- as.matrix(xt)
#X <- X[1:n.pt,]
ipmat <- X %*% Sigma %*% t(X)

diagmat <- matrix(rep(diag(ipmat),n.pt),ncol=n.pt)

dmat <- diagmat + t(diagmat) - 2 * ipmat


inout <- rep(0,n.pt)
inout[1] <- 1
for(i in 1:(n.pt-1)){
	ins <- which(inout == 1)
	outs <- which(inout == 0)
	tmp <- matrix(dmat[outs,ins],ncol=length(ins))
	tmp2 <- apply(tmp,1,min)
	cnds <- which(tmp2>0)
	#print("tmp2")
	#print(tmp2)
	if(length(cnds) == 0){
		break
	}else{
	#if(length(cnds) >= 1){
		print(cnds)
		print(tmp[cnds,])
		pck <- sample(cnds,1)
		print("outs[pck]")
		print(outs[pck])
		print(tmp[pck,])
		inout[outs[pck]] <- 1
	#}else{
	#	break
	}
	print(range(dmat[ins,ins]))
}
library(rgl)
plot3d(X)
spheres3d(X[ins,],radius=0.05,color=2)

plot(X,pch=20,cex=0.1)
points(X[ins,],pch=20,cex=1)
dmat[ins,ins]
