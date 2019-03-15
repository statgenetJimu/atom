library(Ronlyryamada)
library(igraph)
library(rgl)
library(RFOC)

n.mesh <- 16
n <- 5
k <- 5
A. <- matrix(runif(n^2), n, n)
    A.[1, 1] <- k
    B <- matrix(rnorm(n^2), n, n)
    xxx <- my.spherical.harm.mesh(A = A., B = B, n = n.mesh)
e.len <- rep(0,length(xxx$edge[,1]))
for(i in 1:length(e.len)){
	e.len[i] <- sqrt(sum(xxx$v[xxx$edge[i,1],]-xxx$v[xxx$edge[i,2],])^2)
}
g <- graph.edgelist(xxx$edge,directed=FALSE)
gd <- distances(g,weights=e.len)


hc <- hclust(as.dist(gd))
plot(hc)
max.k <- 10
ctout <- cutree(hc, k = 1:max.k) #k = 1 is trivial
#cutree(hc, h = 250)

K <- 8
    plot3d(xxx$v)
    segments3d(xxx$v[c(t(xxx$edge)), ])

for(i in 1:K){
	tmp <- which(ctout[,K]==i)
	spheres3d(xxx$v[tmp,],color=i,radius=0.1)
}


admat <- get.adjacency(g)
# 隣接行列のべき乗は以下のように、固有値・固有ベクトルごとに計算した項の加算として計算できる

my.matpower.simple2 <- function(A,k){
  eigen.out <- eigen(A)
  ret <- matrix(0,length(A[1,]),length(A[,1]))
  for(i in 1:length(ret[1,])){
    tmp <- eigen.out[[2]][,i]
    ret <- ret + eigen.out[[1]][i]^k * matrix(tmp,ncol=1) %*% matrix(tmp,nrow=1)
  }
  return(ret)
}

k <- 100
Ak <- my.matpower.simple2(admat,k)

gd2 <- 1/Ak
hc <- hclust(as.dist(gd2))
plot(hc)
max.k <- 10
ctout <- cutree(hc, k = 1:max.k) #k = 1 is trivial
#cutree(hc, h = 250)

K <- 9
    plot3d(xxx$v)
    segments3d(xxx$v[c(t(xxx$edge)), ])

for(i in 1:K){
	tmp <- which(ctout[,K]==i)
	spheres3d(xxx$v[tmp,],color=i,radius=0.1)
}

plot(gd,gd2)


## Compare the 2 and 4 grouping:
#g24 <- cutree(hc, k = c(2,4))
#table(grp2 = g24[,"2"], grp4 = g24[,"4"])



