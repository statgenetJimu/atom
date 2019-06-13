
library(igraph)
library(raster)
library(pheatmap)
library(RColorBrewer)

#Model: Connahs et al 2017 - Gray Scott Model: Butterfly eyespots
#Numerical algorithm: Crank Nicolson - ADI


#A1 -> reaction term: K*A1^2*A2-k1*A1
fu <- function(K, U, V, k1){
  X<-U
  for(i in 2:(nrow(U)-1)){
    for(j in 2:(ncol(U)-1)){
      X[i,j]<-dt*(K*(U[i,j]^2)*V[i,j] - k1*U[i,j])
    }
  }
  return(X)
}

#A2 -> reaction term: alpha-K*A1^2*A2-k2*A2
fv <- function(alpha, K, U, V, k2){
  Y<-V
  for(i in 2:(nrow(V)-1)){
    for(j in 2:(ncol(V)-1)){
      Y[i,j]<-dt*(alpha-K*(U[i,j]^2)*V[i,j]-k2*V[i,j])
    }
  }
  return(Y)
}

#Diffusion solver
CN<-function(Nx,Ny,dx,dy,dt,t,D,f,C,C2,M){
  S <- matrix(0,Nx,Ny)
  P <- matrix(0,Nx,Ny)
  Z <- matrix(0,Nx,Ny)

  bx <- 0.5*(D*dt/(dx^2))
  by <- 0.5*(D*dt/(dy^2))

  U<-C
  V<-C2

  if(M == 1){
    f <- fu(K, U, V, k1)
  }
  else if(M == 2){
    f <- fv(alpha, K, V, U, k2)
  }

  for(j in 2:(Ny-1)){
    for(i in 1:Nx){
      S[i,j]<-U[i,j]+by*(U[i,j+1]-2*U[i,j]+U[i,j-1])
    }
  }

  for(j in 2:(Ny-1)){
    for(i in 2:(Nx-1)){
      P[i,j]<-S[i,j]+bx*(S[i+1,j]-2*S[i,j]+S[i-1,j])+0.5*dt*f[i,j]
    }
  }

  for(j in 2:(Ny-1)){
    a<-NULL
    b<-NULL
    c<-NULL
    r<-NULL
    q<-NULL
    for(i in 2:Nx-1){
      a[i]<- -bx
      b[i]<- 1+2*bx
      c[i]<- -bx
      r[i]<- P[i,j]
    }

    #Boundary 
    ua <- U[1,j] - by*(U[1,j+1]-2*U[1,j]+U[1,j-1])
    ub <- U[Nx,j] - by*(U[Nx,j+1] - 2*U[Nx,j] + U[Nx,j-1])

    r[2] <- r[2] - a[2] * ua
    r[Nx-1] <- r[Nx-1] - c[Nx-1] * ub

    #tdma
    for(i in 2:(Nx-1)){
      b[i]<-b[i]-a[i]/b[i-1]*c[i-1]
      r[i]<-r[i]-a[i]/b[i-1]*r[i-1]
    }

    q[Nx-1]<-r[Nx-1]/b[Nx-1]
    for(i in seq(Nx-2,1,-1)){
      q[i]<-(r[i]-c[i]*q[i+1])/b[i]
    }

    for(i in 2:(Nx-1)){
      Z[i,j]<-q[i]
    }
    #Boundary - useless?
    if(M==2){
      Z[1,j]<-Z[2,j]
      Z[Nx,j]<-ub
    }
    else{
      Z[1,j]<-ua
      Z[Nx,j]<-ub
    }

  }## X end

  for(j in 2:(Nx-1)){
    a<-NULL
    b<-NULL
    c<-NULL
    r<-NULL
    q<-NULL
    for(i in 2:Ny-1){
      a[i]<- -by
      b[i]<- 1+2*by
      c[i]<- -by
      r[i]<- Z[j,i]
    }

    ua <- U[j,1]
    ub <- U[j,Ny]

    r[2] <- r[2] - a[2] * ua
    r[Ny-1] <- r[Ny-1] - c[Ny-1] * ub

    #tdma
    for(i in 2:(Ny-1)){
      b[i]<-b[i]-a[i]/b[i-1]*c[i-1]
      r[i]<-r[i]-a[i]/b[i-1]*r[i-1]
    }

    q[Ny-1]<-r[Ny-1]/b[Ny-1]
    for(i in seq(Ny-2,1,-1)){
      q[i]<-(r[i]-c[i]*q[i+1])/b[i]
    }

    for(i in 2:(Ny-1)){
      U[j,i]<-q[i]
    }
  }

  if(M==2){
    U[Nx,]<-U[Nx-1,]
  }
  return(U)

}##end CN

#grid points
Nx <- 105
Ny <- 60

#length (in micrometer)
Lx <- 1
Ly <- 1

x0 <- 0
xL <- Lx
y0 <- 0
yL <- Ly

#spatial steps
dx <- (xL-x0)/(Nx-1)
dy <- (yL-y0)/(Ny-1)

#time steps
Tmax <- 144
dt <- 0.0005
nt <- Tmax/dt
t<-0

#Diffusion constants
Du <- 0.01
Dv <- 0.12

#init - A1 = U; A2 = V
U<-matrix(0,Nx,Ny)
U[Nx,]<-65
V<-matrix(0,Nx,Ny)

#reaction parameter
alpha <- (5.5*10^-3)
K <- 2.22*10^-7
k1 <- 0.1*10^-3
k2 <- 0.08*10^-3   


while(t<Tmax){

  if(t==60){
    alpha<-alpha*0.75
  }

  du <- CN(Nx,Ny,dx,dy,dt,t,Du,f_u,U,V,1)
  dv <- CN(Nx,Ny,dx,dy,dt,t,Dv,f_v,V,U,2)

  print(t)

  U<-du
  V<-dv

  t <- t+dt
}

pheatmap(V, cluster_row = FALSE, cluster_col = FALSE, color= brewer.pal(5, "YlGnBu"),border_color = NA)