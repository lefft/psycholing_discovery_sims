### functions written for `noeffect_sim.rmd` ----------------------------------

# simulate a simple experiment num_sims many times, return result as a matrix
nes_sim_data <- function(num_sims, num_data_points, mu, sigma){
  # TODO: allow for different prob dist's other than normal...
  # TODO: allow to have different params for diff groups/splits/etc.
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

# filename template for plot and df output
get_fname <- function(folder="out/", type="noeffect", ftype, nsim, n, mean, sd){
  skel <- paste0(
    folder, "sim_results-", type, 
    "-nsim", nsim, "-n", n, "-mean", mean, "-sd", sd
  )
  return(paste(skel, ftype, sep="."))
}

# quick func to get the most frequent value from a vector
get_mode <- function(vec, useNA="ifany", return_count=FALSE){
  # get the mode and its associated count
  mostfreq_val <- names(sort(table(vec, useNA=useNA), decreasing=TRUE)[1])
  mfv_count <- unname(sort(table(vec, useNA=useNA), decreasing=TRUE)[1])
  
  # check if there's a tie; warn if so
  # TODO: DEAL W TIES (PROB DONT MATTER HERE BUT DOES IN GENERAL)
  if (length(table(vec)[table(vec)==max(table(vec))]) > 1){
    message(paste0("careful, there's a tie for first place: \n", 
                   list(names(table(vec)[table(vec)==max(table(vec))]))))
  }
  
  # return the count if requested
  if (return_count) return(mfv_count)
  
  # return val as numeric if it's numeric; character otherwise
  if ("numeric" %in% class(vec) | "integer" %in% class(vec)){
    return(as.numeric(mostfreq_val))
  } else {
    return(mostfreq_val)
  }
}

# quickly save plot obj
qsave <- function(fname){
  fname <- paste0("plots/", gsub("[[:punct:]]| ", "_", fname), ".pdf")
  message(paste0("saving plot as: \n  >> ", fname))
  ggsave(plot=last_plot(), filename=fname, width=9, height=6, units="in")
}

# the one from lefftpack:: has rounding built in, so redefine here
se_prop <- function(p, n, dig=4){
  round(sqrt((p*(1-p)) / n), digits=dig)
}



