
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






