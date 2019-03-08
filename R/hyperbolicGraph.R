my.ip <- function(x1,x2,A){
  x1.1 <- matrix(x1,nrow=1)
  x1.2 <- matrix(x1,ncol=1)
  x2.1 <- matrix(x2,nrow=1)
  x2.2 <- matrix(x2,ncol=1)
  d1 <- x1.1 %*% A %*% x1.2
  d2 <- x2.1 %*% A %*% x2.2
  d12 <- x1.1 %*% A %*% x2.2
  return(d1 + d2 - 2 * d12)
}
library(igraph)
x <- y <- 0:20
A <- matrix(c(1,0,0,-1),2,2)
xy <- as.matrix(expand.grid(x,y))
D <- matrix(0,length(xy[,1]),length(xy[,1]))
for(i in 1:length(D[,1])){
  for(j in 1:length(D[,1])){
    #print(xy[i,])
    #print(xy[j,])
    D[i,j] <- my.ip(xy[i,],xy[j,],A)
  }
}

B <- matrix(0,length(xy[,1]),length(xy[,1]))
B[which(D <= 3 & D>0)] <- 1
diag(B) <- 0

g <- graph.adjacency(B)
plot(g,vertex.size=0.01,vertex.label=NA,edge.arrow.mode=0)

degree(g)
