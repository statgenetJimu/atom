library(Ronlyryamada)
library(RFOC)
k <- 5
n <- 5
n.mesh = 16
scale.shift = 0.1
scale.rotation1 = 0.1
scale.rotation2 = 0.1
scale.shear = 0.1

A. <- matrix(runif(n^2), n, n)
A.[1, 1] <- k
B <- matrix(rnorm(n^2), n, n)
xxx <- my.spherical.harm.mesh(A = A., B = B, n = n.mesh)
plot3d(xxx$v)
segments3d(xxx$v[c(t(xxx$edge)), ])
M <- diag(rep(1, 4))
M[1:3, 4] <- runif(3) * scale.shift
theta1 <- runif(1) * scale.rotation1
theta2 <- runif(1) * scale.rotation2
Mm <- diag(rep(1, 3))
Mm[1:2, 1:2] <- my.2d.rot(theta1) %*% Mm[1:2, 1:2]
Mm[2:3, 2:3] <- my.2d.rot(theta2) %*% Mm[2:3, 2:3]
M[1:3, 1:3] <- Mm
M[4, 1:3] <- rnorm(3) * scale.shear

n.step <- 30
for (i in 1:n.step) {
        A. <- A. + rnorm(n^2, 0, 0.05)
        xxx <- my.spherical.harm.mesh(A = A., n = n.mesh)
        xxxx <- cbind(xxx$v, rep(1, length(xxx$v[, 1])))
        rot.xxxx <- t(M %*% t(xxxx))
        rot.xxxx. <- rot.xxxx[, 1:3]/rot.xxxx[, 4]
}

xxx1 <- rot.xxxx.
plot3d(xxx1)
segments3d(xxx1[c(t(xxx$edge)), ])

A. <- matrix(runif(n^2), n, n)
A.[1, 1] <- k
B <- matrix(rnorm(n^2), n, n)
xxx <- my.spherical.harm.mesh(A = A., B = B, n = n.mesh)
plot3d(xxx$v)
segments3d(xxx$v[c(t(xxx$edge)), ])
M <- diag(rep(1, 4))
M[1:3, 4] <- runif(3) * scale.shift
theta1 <- runif(1) * scale.rotation1
theta2 <- runif(1) * scale.rotation2
Mm <- diag(rep(1, 3))
Mm[1:2, 1:2] <- my.2d.rot(theta1) %*% Mm[1:2, 1:2]
Mm[2:3, 2:3] <- my.2d.rot(theta2) %*% Mm[2:3, 2:3]
M[1:3, 1:3] <- Mm
M[4, 1:3] <- rnorm(3) * scale.shear

n.step <- 30
for (i in 1:n.step) {
        A. <- A. + rnorm(n^2, 0, 0.05)
        xxx <- my.spherical.harm.mesh(A = A., n = n.mesh)
        xxxx <- cbind(xxx$v, rep(1, length(xxx$v[, 1])))
        rot.xxxx <- t(M %*% t(xxxx))
        rot.xxxx. <- rot.xxxx[, 1:3]/rot.xxxx[, 4]
}
plot3d(rot.xxxx.)
segments3d(rot.xxxx.[c(t(xxx$edge)), ])

xxx2 <- rot.xxxx.
plot3d(xxx2)
segments3d(xxx2[c(t(xxx$edge)), ])

mat.list <- list()

for(i in 1:length(xxx$f[,1])){
	X1 <- xxx1[xxx$f[i,],]
	X2 <- xxx2[xxx$f[i,],]
	mat.list[[i]] <- my.affine3d(X1,X2)
}

eigen.out <-lapply(mat.list,eigen)

eval <- matrix(0,length(eigen.out),3)
evecs <- matrix(0,length(eigen.out),9)

for(i in 1:length(eigen.out)){
	eval[i,] <- eigen.out[[i]][[1]]
	evecs[i,] <- eigen.out[[i]][[2]]
}


mat.list2 <- list()

for(i in 1:length(xxx$f[,1])){
	X1 <- xxx1[xxx$f[i,],]
	X2 <- xxx2[xxx$f[i,],]
	mat.list2[[i]] <- my.affine3d(X1,X1)
}

v1 <- c(0.3,0.6)
v2 <- c(0.6,-0.1)
v3 <- c(-0.8,0.1)
v4 <- c(0.9,-0.1)

u1 <- c(1,0)
u2 <- c(0,1)
u3 <- c(-1,0)
u4 <- c(0,-1)

R12 <- cbind(u1,u2) %*% solve(cbind(v1,v2))
R23 <- cbind(u2,u3) %*% solve(cbind(v2,v3))
R34 <- cbind(u3,u4) %*% solve(cbind(v3,v4))
R41 <- cbind(u4,u1) %*% solve(cbind(v4,v1))

R12 %*% cbind(v1,v2)
R34 %*% cbind(v3,v4)

M <- matrix(0,8,8)
M[1:2,1:2] <- R12
M[3:4,3:4] <- R23
M[5:6,5:6] <- R34
M[7:8,7:8] <- R41
M
det(M)

V <- matrix(0,8,8)
V[1:2,1] <- v1
V[1:2,2] <- v2
V[3:4,3] <- v2
V[3:4,4] <- v3
V[5:6,5] <- v3
V[5:6,6] <- v4
V[7:8,7] <- v4
V[7:8,8] <- v1

M %*% V

vtandem <- c(v1,v2,v3,v4)
utandem <- c(u1,u2,u3,u4)

VV <- diag(rep(1,8))
UU <- diag(rep(1,8))
#VV <- matrix(0,8,8)
#UU <- matrix(0,8,8)
VV[,1] <- vtandem
UU[,1] <- utandem
RR <- UU %*% solve(VV)
