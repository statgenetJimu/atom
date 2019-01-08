# 2x3 table

my.enumerate.table <- function(x){
  rsum <- apply(x,1,sum)
  csum <- apply(x,2,sum)
  r11 <- 0:min(rsum[1],csum[1])
  r12 <- 0:min(rsum[1],csum[2])
  r1112 <- expand.grid(r11,r12)

  tabs <- cbind(r1112[,1],r1112[,2],rsum[1]-r1112[,1]-r1112[,2],csum[1]-r1112[,1],csum[2]-r1112[,2],csum[3]-(rsum[1]-r1112[,1]-r1112[,2]))
  mins <- apply(tabs,1,min)
  tabs <- tabs[which(mins>=0),]
  return(tabs)
}

x <- matrix(c(10,20,30,40,50,60),2,3)



tabs <- my.enumerate.table(x)

v1 <- c(1,0)
v2 <- c(1/2,sqrt(3)/2)
vv <- cbind(v1,v2)

coords <- t(vv %*% t(tabs[,1:2]))

chis <- matrix(0,length(tabs[,1]),3)

for(i in 1:length(tabs[,1])){
  chis[i,1] <- prop.trend.test(tabs[i,1:3],tabs[i,1:3]+tabs[i,4:6],score=c(0,0,1))[[1]]
  chis[i,2] <- prop.trend.test(tabs[i,1:3],tabs[i,1:3]+tabs[i,4:6],score=c(0,0.5,1))[[1]]
  chis[i,3] <- prop.trend.test(tabs[i,1:3],tabs[i,1:3]+tabs[i,4:6],score=c(0,1,1))[[1]]
}

chi.sum <- apply(chis,1,sum)
plot(coords,col=round(chi.sum*0.1),pch=20)

library(rgl)

plot3d(coords[,1],coords[,2],chi.sum)

#plot(coords)

help(prop.trend.test)
