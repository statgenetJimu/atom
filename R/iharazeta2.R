library(igraph)
# �O���t�͖����O���t
#el <- rbind(c(1,2),c(2,3),c(3,4),c(4,1),c(1,3))
#el <- rbind(c(1,2),c(2,3),c(3,1),c(1,4))
el <- rbind(c(1,2),c(2,3),c(3,4),c(4,1),c(1,5),c(2,5),c(5,6),c(3,6),c(4,6))
g <- graph.edgelist(el,directed =FALSE)
plot(g)
#A <- matrix(1,3,3)
#diag(A) <- 0
#g <- graph.adjacency(A,mode="undirected")

my.iharazeta.A <- function(g,u,q){
	m <- length(E(g))
	n <- length(V(g))
	I <- diag(rep(1,n))
	A <- as.matrix(get.adjacency(g))
	(1-u^2)^(m-n) * Det(I - u * A + q * u^2 * I)
}


my.bigraph <- function(g){
  el <- as_edgelist(g)
  el2 <- rbind(el,cbind(el[,2],el[,1]))
  ret <- graph.edgelist(el2)
  return(ret)
}
g.bi <- my.bigraph(g)
plot(g.bi,edge.label=1:(2 * length(E(g))),edge.curved=TRUE)
my.WE <- function(g){
  el <- as_edgelist(g)
  n.e <- length(el[,1])
  edge.mat <- matrix(0,n.e,n.e)
	for(i in 1:n.e){
		st <- el[i,1]
		ed <- el[i,2]
		tmp <- which(el[,1]==ed & el[,2]!=st)
		edge.mat[i,tmp] <- 1
	}
  return(edge.mat)
}
WE <- my.WE(g.bi)
WE
# WE2 <- WE[,c(2,7,3,4,5,6,1,8)]
# WE2
# u <- 3
# WE2. <- WE2*u
# WE2.
# IWE2. <- (diag(rep(1,8))-WE*u)[,c(2,7,3,4,5,6,1,8)]
# IWE2.
# prod(diag(IWE2.))
# (-u)^3
library(complexplus) # ���f�s���v�Z�p
my.Ihara.zeta.e <- function(g,u){
  g.bi <- my.bigraph(g) # �������O���t�ɂ���
  we <- my.WE(g.bi) # WE �s��������
  return(Det(diag(rep(1,length(we[,1]))) - we * u)) # �s�񎮂��v�Z���ĕԂ�
}
my.sign <- function(s){
  I <- diag(rep(1,length(s)))
  return(det(I[,s]))
}
u <- 0.3 + 1i * 0.2 # �K���ȕ��f��
my.Ihara.zeta.e(g,u)
# �n���Ɍv�Z�����Ƃ��͂S�̒u��
# (1,2,3,4,5,6,7,8) �u���Ȃ��B�I���W�i��
# (2,7,3,4,5,6,1,8) (1,2,7)
# (1,2,6,4,3,5,7,8) (3,6,5)
# (2,7,6,4,3,5,1,8) (1,2,7,3,6,5)
# 1 + (-u)^3 + (-u)^3 + (-u)^6
# el <- rbind(c(1,2),c(2,3),c(3,4),c(4,1))
# g <- graph.edgelist(el,directed =FALSE)
# plot(g)
# g.bi <- my.bigraph(g)
# plot(g.bi,edge.label=1:10,edge.curved=TRUE)
# WE <- my.WE(g.bi)
# WE
# u <- 3
# WE2. <- WE2*u
# WE2.
#u <- 0.3 + 1i * 0.2 # �K���ȕ��f��
my.Ihara.zeta.e(g,u)

#1  +(-1)* 2*(-u)^4 + (-u)^8
my.Ihara.zeta.poly <- function(g){
  n.e <- length(E(g))*2
  us <- rnorm(n.e)
  U <- matrix(0,n.e,n.e)
  for(i in 1:length(us)){
    U[i,] <- (-us[i])^(1:n.e)
  }
  vs <- rep(0,n.e)
  for(i in 1:n.e){
    vs[i] <- my.Ihara.zeta.e(g,us[i])
  }
  A <- solve(U) %*% (vs-1)
  return(c(1,A))
}

my.Ihara.zeta.poly.calc <- function(g,u){
  A <- round(my.Ihara.zeta.poly(g))
  v <- sum(A * (-u)^(0:(length(A)-1)))
  return(list(v=v,A=A))
}
A <- my.Ihara.zeta.poly(g)
round(A,10)
my.Ihara.zeta.e(g,u)
my.Ihara.zeta.poly.calc(g,u)
