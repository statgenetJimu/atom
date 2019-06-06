k <- 8
k6 <- k * 6

my.R <- function(k){
	k6 <- k * 6
	ret <- diag(k6)
	tmp <- rep(c(2,3,1),2*k)
	tmp2 <- rep(1:(2*k),each=3) * 3 -3
	tmp3 <- tmp + tmp2
	ret <- ret[,tmp3]
	return(ret)
}

R <- my.R(k)

my.rL <- function(k){
	k6 <- k * 6
	tmp <- matrix(sample(1:k6),ncol=2)
	L <- matrix(0,k6,k6)
	for(i in 1:length(tmp[,1])){
		L[tmp[i,1],tmp[i,2]] <- L[tmp[i,2],tmp[i,1]] <- 1
	}
	return(L)
}

my.rRL <- function(k){
	R <- my.R(k)
	rL <- my.rL(k)
	R %*% rL
}

rRL <- my.rRL(k)
image(rRL)

plot(hclust(dist(Mod(t(eigen.out[[2]]))))->hout)

apply(Mod(eigen.out[[2]][,hout$order]),2,mean)

# 6kを3kペアにする場合の数は、たった(6k(6k-1)/2) なので、全列挙可能
# これで、k+2個に分かれ、すべてのクラスタが3以上なものを列挙すれば、それが、「全三角閉多面体(の候補・・・入込問題とかを解決する必要アリ)