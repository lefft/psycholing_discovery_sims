
# these are over-plotted, need to aggregate...
# pvals <- ggplot(cont, aes(x=trim_total, y=pval, color=strat)) + 
#   geom_point() + theme(legend.position="top")
# tvals <- ggplot(cont, aes(x=trim_total, y=tval, color=strat)) + 
#   geom_point() + theme(legend.position="top")


## v inefficient way to make sim shell
simshell <- data.frame(
  sim=integer(50), 
  trim_top=rep(thetas, each=10),trim_bot=rep(thetas, times=10),
  strat=rep(strats, each=5, times=5),
  diffmeans_raw=numeric(50),diffmeans_cut=numeric(50),
  trimmed_top=numeric(50),trimmed_bot=numeric(50),
  confint=character(50),tval=numeric(50),pval=numeric(50),
  stringsAsFactors=FALSE
)
cont <- simshell[0,]
# View(simshell); View(cont)

# TODO: rewrite this so it's like 1000x faster (shd be easy)
for (x in 1:num_sims){
  temp <- simshell
  temp$sim <- x
  cont <- rbind(cont, temp)
}; rm(temp)


