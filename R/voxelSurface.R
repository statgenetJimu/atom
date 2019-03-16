library(devtools)
install.packages("RFOC")
install_github("ryamada22/Ronlryamada")
library(Ronlyryamada)
library(rgl)
library(MCMCpack)
library(RColorBrewer)
n <- 5
k <- 5
n.mesh <- 10
A. <- matrix(runif(n^2), n, n)
A.[1, 1] <- k
B <- matrix(rnorm(n^2), n, n)
xxx <- my.spherical.harm.mesh(A = A., B = B, n = n.mesh)
plot3d(xxx$v)
segments3d(xxx$v[c(t(xxx$edge)), ])
my.voxel.from.trimesh <- function(xxx,Npt=10^3,scale=10){
  vs <- matrix(0,0,3)
  rnd <- rdirichlet(Npt,rep(1,3))
  for(i in 1:length(xxx$f[,1])){
    f <- xxx$f[i,]
    vtri <- t(xxx$v[f,])
    tmpX <- floor(vtri %*% t(rnd) * scale)
    vs <- rbind(vs,t(tmpX))
    vs <- unique(vs)
  }
  return(vs)
}

outVox <- my.voxel.from.trimesh(xxx)

gd <- as.matrix(dist(outVox,method="manhattan"))
gd <- gd==1
g <- graph.adjacency(gd,mode="undirected")
degrees(g)

eigen.out <- eigen(gd)

plot(eigen.out[[1]])

my.plot.colSurface <- function(X,v,ncol=10,radius=1){
  plot3d(X)
  col <- floor((v-min(v))/(max(v)-min(v)) * (1-0.01/ncol) * ncol)+1
  uniquecol <- unique(col)
  colpallette <- brewer.pal(ncol,"Spectral")
  for(i in 1:length(uniquecol)){
    tmp <- which(col==uniquecol[i])
    spheres3d(X[tmp,],col=colpallette[i],radius=radius)
  }
}
my.plot.colSurface(outVox,eigen.out[[2]][,1])

################
library(VoxR)
data(treecloud)
#-voxelisation
treecloud_vox <- vox(treecloud,res=0.02)
plot3d(treecloud_vox,size=0.1)


my.onlyone <- function(x){
	n <- length(x[,1])
	a <- !duplicated(x)
	b <- !duplicated(x[n:1,])[n:1]
	return(x[a & b,])
}
x <- matrix(c(0,0,0,1,0,1,1,1),byrow=TRUE,ncol=2)
my.onlyone(x)
# ボクセルはその座標最小頂点(x,y,z)座標で登録する

# 各ボクセルは６面を持つ
# 面(正方形)はその座標最小頂点(x,y,z)と、面の方向で区別する
# xy平面、yz平面、zx平面に広がるものを、1,2,3と呼び分ける

# 各頂点は４頂点を持つ

Vox.list <- as.matrix(expand.grid(0:2,0:2,0:2))

my.vox.faces <- function(xyz){
	rbind(c(xyz,1),c(xyz,2),c(xyz,3),c(xyz+c(1,0,0),2),c(xyz+c(0,1,0),3),c(xyz+c(0,0,1),1))
}

my.face.nodes <- function(xyzw){
	xyz <- xyzw[1:3]
	w <- xyzw[4]
	if(w==1){
		ret <- rbind(xyz,xyz+c(1,0,0),xyz+c(0,1,0),xyz+c(1,1,0))
	}else if(w==2){
		ret <- rbind(xyz,xyz+c(0,1,0),xyz+c(0,0,1),xyz+c(0,1,1))
	}else{
		ret <- rbind(xyz,xyz+c(0,0,1),xyz+c(1,0,0),xyz+c(1,0,1))
	}
	return(ret)
}

my.onlyone <- function(x){
	n <- length(x[,1])
	a <- !duplicated(x)
	b <- !duplicated(x[n:1,])[n:1]
	return(x[a & b,])
}
my.vox.surface <- function(Vlist){
	faces <- matrix(0,0,4)
	for(i in 1:length(Vlist[,1])){
		faces <- rbind(faces,my.vox.faces(Vlist[i,]))
	}
	return(my.onlyone(faces))

}

#faces <- my.vox.surface(Vox.list)
faces <- my.vox.surface(outVox)
my.surface.nodes <- function(faces){
	vs <- matrix(0,0,3)
	for(i in 1:length(faces[,1])){
		vs <- rbind(vs,my.face.nodes(faces[i,]))
	}
	return(unique(vs))
}

nodes <- my.surface.nodes(faces)

library(rgl)
plot3d(nodes)
