library(igraph)

d <- 101
M <- matrix(1,d,d)
diag(M) <- 0

eigen.out <- eigen(M)
eigen.out[[1]]
matplot(eigen.out[[2]],type="l")

M <- diag(rep(1,d))
M <- M[c(2:d,1),]
M <- M + t(M)
#M
eigen.out <- eigen(M)
par(mfcol=c(1,2))
plot(eigen.out[[1]])
matplot(eigen.out[[2]],type="l")


d <- 101
M <- matrix(1,d,d)
diag(M) <- 0



M <- diag(rep(1,d))
M <- M[c(2:d,1),]
M[d,1] <- 0
M <- M + t(M)

k <- 1
for(i in 1:k){
s <- sort(sample(1:(d-1),2))
M[s[1],s[1]+1] <- M[s[1]+1,s[1]] <- 0
M[s[1],s[2]] <- M[s[2],s[1]] <- 1
}
eigen.out <- eigen(M)
plot(eigen.out[[1]])
matplot(eigen.out[[2]],type="l")
g <- graph.adjacency(M,mode="undirected")
plot(g)

x <- y <- 0:39
xy <- expand.grid(x,y)
tmp <- as.matrix(dist(xy))
M <- matrix(0,length(xy[,1]),length(xy[,1]))
M[which(tmp==1)] <- 1
g <- graph.adjacency(M,mode="undirected")
plot(g)
eigen.out <- eigen(M)
plot(eigen.out[[1]])
#matplot(eigen.out[[2]],type="l")

k <- 4
q <- eigen.out[[2]][,k]

col <- floor(10*0.99* (q-min(q))/(max(q)-min(q))) + 1

plot(xy,pch=20,col=col%%2 + 1)
