# グラフのフォック空間基底

library(igraph)
n <- 10
A <- matrix(sample(0:1,n^2,replace=TRUE,prob=c(0.3,0.7)),n,n)
A <- A + t(A)
diag(A) <- 0
A[which(A!=0)] <- 1
g <- graph.adjacency(A)
plot(g)
image(A)
