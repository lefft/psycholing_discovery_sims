require("dplyr"); require("magrittr"); require("ggplot2"); require("gridExtra")




### 1. functions written for `psycholing_discovery_sims.rmd` ------------------


# sim funcs etc.

# calls generate data and introduce outliers
# simulate_experiment <- function(n_subj, n_item, n_cond, derived_vars=TRUE){
simulate_experiment <- function(){
  dat <- generate_data()
  dat <- introduce_outliers(dat=dat)
  dat <- transform_rts(dat=dat)
  return(dat)
}


# NOT WORKING AS-IS -- NEEDS CHAR CONVERSION + IMPLEMENT RT CHOICE
# plot the data w a specific col for reaction time (e.g. log/scaled/etc.)
plot_results <- function(dat, rt_col=NULL){
  
  col_dict <- setNames(c("raw RT","log RT","RT, centered","RT, z-scored"), 
                       nm=c("rt", "rt_log", "rt_ctd", "rt_zsc"))
  
  # description of the sim setup
  descript <- attributes(dat)$comment
  subj_info <- attributes(dat)$bot
  
  # plot the data to make sure it looks as desired
  rt_by_cond <- ggplot(
    dat, aes_string(x=rt_col, fill="cond", color="cond")
    ) + 
    geom_density(alpha=.1) + # or: # geom_histogram(bins=20, position="dodge")
    labs(title=paste0("1. measure: ", as.character(col_dict[rt_col])), 
         subtitle="by condition") + 
    theme(legend.position="bottom", axis.text.y=element_blank()) + 
    scale_fill_manual(values=c("#dbc13f","#176614")) + 
    scale_color_manual(values=c("#dbc13f","#176614")) # yellow, green
  
  rt_by_cond_outlier <- ggplot(
    dat, aes_string(x=rt_col, fill="cond", color="cond")
    ) + 
    geom_density(alpha=.1) + facet_wrap(~is_outlier) + 
    theme(legend.position="none", axis.text.y=element_blank()) + 
    labs(title=paste0("3. measure: ", as.character(col_dict[rt_col])), 
         subtitle="by outlier and condition") + 
    scale_fill_manual(values=c("#dbc13f","#176614")) + 
    scale_color_manual(values=c("#dbc13f","#176614")) # yellow, green
  
  rt_by_outlier <- ggplot(
    dat, aes_string(x=rt_col, fill="is_outlier", color="is_outlier")
    ) + 
    geom_density(alpha=.1) + 
    theme(legend.position="bottom", axis.text.y=element_blank()) + 
    labs(title=paste0("2. measure: ", as.character(col_dict[rt_col])), 
         subtitle="by outlier status") + 
    scale_fill_manual(values=c("#ba671f","blue")) + 
    scale_color_manual(values=c("#ba671f","blue"))
  
  rt_by_outlier_cond <- ggplot(
    dat, aes_string(x=rt_col, fill="is_outlier", color="is_outlier")
    ) + 
    geom_density(alpha=.1) + facet_wrap(~cond) + 
    theme(legend.position="none", axis.text.y=element_blank()) + 
    labs(title=paste0("4. measure: ", as.character(col_dict[rt_col])), 
         subtitle="by condition and outlier") + 
    scale_fill_manual(values=c("#ba671f","#1264ce")) + 
    scale_color_manual(values=c("#ba671f","#1264ce")) # orange, blue
  
  out <- grid.arrange(
    # the grobs
    rt_by_cond, rt_by_cond_outlier, rt_by_outlier, rt_by_outlier_cond, 
    # their relative position/layout
    ncol=2, layout_matrix = cbind(c(1,2), c(3,4)), 
    top=paste0(descript,"\n"), bottom=subj_info
  )
  
  return(out)
}


# NOT WORKING AS-IS -- NEEDS CHAR CONVERSION + IMPLEMENT RT CHOICE
# plot the data w a specific col for reaction time (e.g. log/scaled/etc.)
summarize_results <- function(dat, rt_col=NULL){
  
  # summary tables
  
  # just grouping by condition
  dat %>% group_by(cond) %>% summarize(
    num_obs = n(), 
    rt_mean = mean(rt),
    rt_sd   = sd(rt)
  ) %>% knitr::kable(digits=0, caption=comment(dat)) -> summary_by_cond
  
  # just grouping by outlier status
  dat %>% group_by(is_outlier) %>% summarize(
    num_obs = n(), 
    rt_mean = mean(rt),
    rt_sd   = sd(rt)
  ) %>% knitr::kable(digits=0, caption=comment(dat)) -> summary_by_outlier
  
  # group by condition and by subj
  dat %>% group_by(is_outlier, cond) %>% summarize(
    num_obs = n(), 
    rt_mean = mean(rt),
    rt_sd   = sd(rt)
  ) %>% knitr::kable(digits=0, caption=comment(dat)) -> summary_by_outlier_cond
  
  # return all three as a list
  return(list(summary_by_cond=summary_by_cond, 
              summary_by_outlier=summary_by_outlier, 
              summary_by_outlier_cond=summary_by_outlier_cond))
}



# function to simulate some reasonable data
generate_data <- function(n_subj=100, n_item=20, n_cond=2, 
                          mean_rt=500, sd_rt=100, effect=100){
  # get the params
  # n_subj <- 100
  # n_item <- 20
  # n_cond <- 2
  
  # set up most of the data, other than rt's
  dat <- data.frame(
    subj = rep(sprintf("subj_%03d", seq(len=n_subj)), each=n_item),
    item = rep(sprintf("item_%02d", seq(len=n_item)), times=n_subj),
    cond = rep(c("related", "_unrelated"), each=n_item/n_cond, times=n_subj),
    rt   = numeric(n_subj*(n_item/n_cond)), 
    stringsAsFactors=FALSE
  )
  
  # params for the response data
  # mean_rt <- 500
  # sd_rt <- 100
  # effect <- 100
  
  # randomly generate rt's for each condition
  rt_unrelated <- rnorm(n_subj*(n_item/n_cond), mean=mean_rt, sd=sd_rt) 
  rt_related   <- rnorm(n_subj*(n_item/n_cond), mean=mean_rt-effect, sd=sd_rt) 
  
  # now add the response data to the df
  dat$rt[dat$cond=="related"] <- rt_related
  dat$rt[dat$cond=="_unrelated"] <- rt_unrelated
  
  # attach metadata for plotting/modeling in the future
  comment(dat) <- paste0(
    "mean RT: ", mean_rt, ";  ",
    "sd of RT: ", sd_rt, ";  ",
    "effect: ", effect, "ms"
  )
  attr(dat, "bot") <- paste0(
    "num subj: ", n_subj, ";  ", "num item: ", n_item, 
    " (", n_subj*n_item, " data points)"
  )
  
  # return the simulated data
  return(dat)
}


# this will cause some of the subj to have a different effect
introduce_outliers <- function(dat=dat, n_outlier=10, outlier_effect=-100){
  
  # now replace the random rt's with weird rt's for outlier subj's
  # n_outlier <- 10
  # outlier_effect <- -100
  
  # for development, have the outliers be 01-10
  # outlier_subj <- sample(dat$subj, size=n_outlier, replace=FALSE)
  outlier_subj <- sort(unique(as.character(dat$subj)))[seq(len=n_outlier)]
  
  # gotta get number of items etc [THIS IS SLOPPY]
  n_item <- length(unique(dat$item))
  n_cond <- length(unique(dat$cond))
  mean_rt <- 500
  sd_rt <- 100
  
  
  rt_related_outlier <- 
    rnorm(n_outlier*(n_item/n_cond), mean=mean_rt-outlier_effect, sd=sd_rt)
  
  dat$rt[dat$subj %in% outlier_subj & dat$cond=="related"] <- rt_related_outlier
  
  # record who the outliers are for plotting
  dat$is_outlier <- ifelse(dat$subj %in% outlier_subj, "_outlier", "reggie")
  
  # add info about the outliers
  comment(dat) <- paste0(
    comment(dat), "\n", 
    "num outlier subj: ", n_outlier, ";  ", 
    "effect for outliers: ", outlier_effect, "ms"
  )
  
  # return the df
  return(dat)
}


# function to do some common rt transformations
transform_rts <- function(dat=dat){
  # make a few derived transformations of the rt's for analysis
  dat$rt_log <- as.numeric(log(dat$rt))
  dat$rt_ctd <- as.numeric(scale(dat$rt, center=TRUE, scale=FALSE)) # zero mean
  dat$rt_zsc <- as.numeric(scale(dat$rt, center=TRUE, scale=TRUE))  # z-score
  
  # and return the data
  return(dat)
}




### 2. functions written for `noeffect_sim.rmd` --------------------

# simulate a simple experiment arbitrarily many times
# return result as a matrix
nes_sim_data <- function(num_sims, num_data_points, mu, sigma){
  # TODO: allow for different prob dist's other than normal...
  matrix(replicate(
    num_sims, 
    rnorm(num_data_points, mean=mu, sd=sigma), 
    simplify=TRUE
  ), nrow=num_data_points, ncol=num_sims)
}

# make a shell with a summary of each sim iteration
make_simshell <- function(num_sims, thetas, strats){
  rows_per_sim <- length(thetas)^2 * length(strats)
  nrow_out <- rows_per_sim * num_sims
  n_ths <- length(thetas)
  out <- data.frame(
    sim=rep(seq_len(num_sims), each=rows_per_sim), 
    trim_top=rep(rep(thetas, each=n_ths*2), times=num_sims),
    trim_bot=rep(rep(thetas, times=n_ths*2), times=num_sims),
    strat=rep(rep(strats, each=n_ths, times=n_ths), times=num_sims),
    diffmeans_raw=numeric(nrow_out),
    diffmeans_cut=numeric(nrow_out),
    trimmed_top=numeric(nrow_out),
    trimmed_bot=numeric(nrow_out),
    confint=character(nrow_out),
    tval=numeric(nrow_out),
    pval=numeric(nrow_out),
    stringsAsFactors=FALSE
  )
  return(out)
}






