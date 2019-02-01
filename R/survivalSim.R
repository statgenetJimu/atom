library(survival)
library(ggkm)
N <- 1000
nt <- 100
id <- group <- status <- time <- c()
for(i in 1:N){
  id <- c(id,rep(i,nt))
  group <- c(group,rep(sample(1:3,1),nt))
  tmp <- rexp(1)*5
  tmp2 <- rep(1,nt)
  tmp2[which(1:nt > tmp)] <- 0
  status <- c(status,tmp2)
  time <- c(time,1:nt)
}
my.data <- data.frame(id = id, group = group, status=status)

fit <- survfit(Surv(time,status)~group)
ggkm(fit, ystratalabs=c(1,2,3))

plot(status)
