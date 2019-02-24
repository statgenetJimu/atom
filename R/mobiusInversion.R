# 割り切る数 divisorを計算する関数 divisors()を有するパッケージ
library(numbers)
# 適当に、自然数を台とする関数fを与える
my.f <- function(x){
	sin(x) + 1i * x^2
}
# １からNまで処理すｒ
N <- 100
# 関数gは以下のように定まる
my.g <- function(f,n){
	ret <- 0
	d <- divisors(n)
	for(i in 1:length(d)){
		ret <- ret + my.f(d[i])
	}
	ret
}
# 1からNまで関数f,gの値を出しておく
gout <- rep(0,N)
fout <- rep(0,N)
for(i in 1:N){
	fout[i] <- my.f(i)
	gout[i] <- my.g(f,i)
}

plot(gout)
# muの値を計算する
# mu[1]はすぐに求まる
# それ以外のmuはdivisorsの具合によって、再帰的に決めていく
# 複数のdivisorsのうち１個を除いてmuが確定していれば、最後の１個のdivisorに対するmuが決まる
my.mu <- function(f,N){
	dvs <- list()
	for(i in 1:N){
		dvs[[i]] <- divisors(i)
	}
	fout <- rep(0,N)
	gout <- rep(0,N)
	for(i in 1:N){
		fout[i] <- my.f(i)
		gout[i] <- my.g(f,i)
	}
	ret <- rep(NA,N)
	ret[1] <- fout[1]/gout[1]
	loop <- TRUE
	while(loop){
		for(i in 1:N){
			if(is.na(ret[i])){
				if(length(which(is.na(ret[dvs[[i]]])))==1){
					tmp <- which(!is.na(ret[dvs[[i]]]))
					tmp3 <- which(is.na(ret[dvs[[i]]]))
					tmp2 <- fout[i] - sum(ret[dvs[[i]][tmp]] * gout[i/dvs[[i]][tmp]])
					ret[dvs[[i]][tmp3]] <- tmp2/gout[i/dvs[[i]][tmp3]]
				}
			}
		}
		if(length(which(is.na(ret))) == 0){
			loop <- FALSE
		}
	}
	return(list(fout=fout,gout=gout,mu=ret))
}
mu.out <- my.mu(f,N)
plot(mu.out$mu)
# 検算

fout2 <- rep(0,N)
for(i in 1:N){
	d <- divisors(i)
	fout2[i] <- sum(mu.out$mu[d] * gout[i/d])
}

plot(fout,fout2)


# mobius inversion
library(numbers)
my.g <- function(n){
  n^2 * (3+1i*0.2) + pi
}

my.f <- function(g,n){
  ds <- divisors(n)
  #ds <- ds[-1]
  ret <- 0
  for(i in 1:length(ds)){
    ret <- ret + g(ds[i])
  }
  return(ret)
}
my.mobius <- function(n){
  if(n==1){
    return(1)
  }
  pf <- primeFactors(n)
  tab <- table(pf)
  ret <- 0
  if(length(which(tab>=2))>0){
    ret <- 0
  }else{
    tmp <- unique(pf)
    ret <- (-1)^length(tmp)
  }
  return(ret)
}
my.g.check <- function(f,g,n){
  ds <- divisors(n)
  #ds <- ds[-1]
  ret <- 0
  for(i in 1:length(ds)){
    #ret <- ret + f(g,ds[i]) * my.mobius(n/ds[i])
    ret <- ret + f(g,n/ds[i]) * my.mobius(ds[i])

  }
  return(ret)
}

n <- 102

my.g(n)
my.f(my.g,n)
my.g.check(my.f,my.g,n)

# Dirichlet convolution

my.dirichlet.convolution <- function(f,g,n){
  ds <- divisors(n)
  ret <- 0
  for(i in 1:length(ds)){
    ret <- ret + f(ds[i]) * g(n/ds[i])
  }
  return(ret)
}

n <- 18
my.g(n)
my.ff <- function(n){
  my.f(my.g,n)
}

my.dirichlet.convolution(my.ff,my.mobius,n)
