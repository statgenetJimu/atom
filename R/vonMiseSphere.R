# Place points on a unit sphere
# Generate Convexhul on the sphere
# The convex hull is a mesh graph
# When you handle the graph whose edges have the same length,
# The graph represents a graph of a closed surface object whose edges have the same length

# Based on this idea, we can generate mesh graphs corresponding to various "shapes"

# Utility functions
my.col.st <- function(x,N=5){
  floor((x-min(x))/(max(x)-min(x)) * 0.99 * N)+1
}

my.color.point <- function(X,col,cex=0.5){
  n <- length(unique(col))
  ucol <- unique(col)
  for(i in 1:n){
    tmp <- which(col==ucol[i])
    points(X[tmp,],pch=20,col=ucol[i],cex=cex)
  }
}
my.color.sphere <- function(X,col,radius=1){
  n <- length(unique(col))
  ucol <- unique(col)
  for(i in 1:n){
    tmp <- which(col==ucol[i])
    spheres3d(X[tmp,],color=ucol[i],radius=radius)
  }
}



# install.packages("movMF") # distribution functions on sphere
# install.packages("geometry") # convexhull
library(movMF)
library(rgl)
library(geometry)
library(igraph)

mu <- matrix(rnorm(9),3,3)
#mu <- diag(rep(1,3))
kappa <- c(4,4,4) # variance of points on the sphere
theta <- kappa * mu
theta
alpha <- c(0.48, 0.52,0.25)
## Generate a sample of size n = 50 from the von Mises-Fisher mixture
## with the above parameters.

ps1 <- rmovMF(1000, theta, alpha)
mu <- matrix(rnorm(9),3,3)
#mu <- diag(rep(1,3))
kappa <- c(4,4,4) # variance of points on the sphere
theta <- kappa * mu
theta
alpha <- c(0.48, 0.52,0.25)
## Generate a sample of size n = 50 from the von Mises-Fisher mixture
## with the above parameters.

ps2 <- rmovMF(1000, theta, alpha)

R <- matrix(c(0,1,0,-1,0,0,0,0,1),3,3)
ps2 <- ps1 %*% R
ps <- rbind(ps1,ps2)
plot3d(ps)


ts.surf <- t(convhulln(ps))  # see the qhull documentations for the options
## Not run:
rgl.triangles(ps[ts.surf,1],ps[ts.surf,2],ps[ts.surf,3],col="blue",alpha=.2)

el <- cbind(c(ts.surf[1,],ts.surf[2,],ts.surf[3,]),c(ts.surf[2,],ts.surf[3,],ts.surf[1,]))
el <- t(apply(el,1,sort))
el <- unique(el)
g <- graph.edgelist(el,directed=FALSE)
adj <- get.adjacency(g)
eigen.out <- eigen(adj)




plot3d(ps)
p <- 1
col <- my.col.st(eigen.out[[2]][,p])
my.color.sphere(ps,col,radius=0.1)
