# devtools::install_github("jtleek/tidypvals")
library("tidypvals")



sim_and_test <- function(x){
  datA <- rnorm(50, mean=.5, sd=1)
  datB <- rnorm(50, mean=0, sd=1)
  tt <- t.test(datA, datB)
  return(list(tval=tt$statistic, pval=tt$p.value))
}

res <- lapply(1:1000, sim_and_test)
tvals <- sapply(1:1000, function(x) unname(unlist(res[[x]]$tval)))
pvals <- sapply(1:1000, function(x) unname(unlist(res[[x]]$pval)))



hist(tvals)
hist(pvals)

plot(seq_along(pvals), pvals)
plot(density(pvals))

library("ggplot2")

dat <- data.frame(
  num = 1:1000, 
  tval=round(tvals, 2),
  pval=round(pvals, 2), 
  trank=rank(tvals),
  prank=rank(pvals)
)

ggplot(dat, aes(x=trank, y=tval)) + geom_point()
ggplot(dat, aes(x=prank, y=pval)) + geom_point()

hist(dat$pval)


ggplot(dat, aes(x=pval)) + 
  geom_density() + geom_vline(aes(xintercept=.05), color="red")

ggplot(dat, aes(x=tval)) + 
  geom_density() + geom_vline(aes(xintercept=.5), color="red")





sd_pool <- function(x,y){
  sdx <- sd(x)
  sdy <- sd(y)
  sqrt((sdx^2 + sdy^2)/2)
}

get_d <- function(x,y){
  (mean(x) - mean(y)) / sd_pool(x,y)
}


get_d(rnorm(1e5,.5,1), rnorm(1e5,0,1))

