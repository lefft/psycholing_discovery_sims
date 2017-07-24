###############################################################################
# tim leffel, july23/2017
# 
# this is a light rewrite/modification of a note by emmanuel chemla: 
#  > post: http://www.emmanuel.chemla.free.fr/thresholds/OutliersCriterion.html
#  > code: http://www.emmanuel.chemla.free.fr/thresholds/significantthresholds.R
#  > original code is also at bottom of this document, after `if (FALSE){ ...`
# 
############################################################################### 


# global params
Part <- 40; numsims <- 1000
quants <- c(0, .025, .05, .075, .1, .15)
#'

# 25 rows per sim w three thresholds
# 46 rows per sim w four thresholds
#'

# case 1 (Lfirst), valid for all ql/qh:
#   - calls pvaluefromreplace()
#   - replacement = TRUE
#   - differentmeasure = FALSE
# 
# case 2 (Lsecond), valid for only non-zero ql/qh:
#   - calls pvaluefromreplace()
#   - replacement = FALSE
#   - differentmeasure = FALSE
# 
# case 3 (Lthird), valid for only non-zero ql/qh:
#   - calls pvaluefrom()
#   - replacement = FALSE
#   - differentmeasure = TRUE
#   - arg R --> 
#     X$EXCL > quantile(X$EXCL, ql) & X$EXCL < quantile(X$EXCL, 1 - qh)


pvfr <- function(D=X, ql, qh, repl=TRUE){
  # find the lower threshold
  Qlo <- ifelse(ql!=0, 
                quantile(D[["RT"]], ql), 
                min(D[["RT"]]) - 1)
  # find the upper threshold
  Qhi <- ifelse(qh!=0, 
                quantile(D[["RT"]], 1 - qh), 
                max(D[["RT"]]) + 1)
  # should we replace vals outside of thresholds, or delete?!
  if (repl==FALSE){
    # if replace is false, just filter out "extreme" rt's
    dat <- D[D[["RT"]] > Qlo & D[["RT"]] < Qhi, ]
  } else {
    # if replace is true, then replace extreme vals with the min/max
    dat                <- D
    # replace the low values
    RTlo               <- dat[["RT"]] <  Qlo
    dat[["RT"]][RTlo]  <- Qlo
    # replace the hi values
    RThi               <- dat[["RT"]] >  Qhi
    dat[["RT"]][RThi]  <- Qhi
  }
  # then do a t-test/anova
  analysis <- summary(aov(dat[["RT"]] ~ dat[["Cond"]]))
  # and extract the p-value and return
  return(analysis[[1]][["Pr(>F)"]][1])
}

pvf <- function(D=X, logical_condition){
  dat <- D[logical_condition, ]
  analysis <- summary(aov(dat[["RT"]] ~ dat[["Cond"]]))
  return(analysis[[1]][["Pr(>F)"]][1])
}


# the data to be ...
X <- data.frame(Cond = rep(1:2, each = Part / 2))

out <- data.frame(sim=NULL, repl=NULL, diffmeas=NULL, 
                  exclLo=NULL, exclHi=NULL, pval=NULL)

for (x in seq(len=numsims)){
  X$RT <- rnorm(Part)
  X$EXCL <- rnorm(Part)
  
  for (qLo in quants){
    for (qHi in quants){
      
      # add a new row for ...
      new_first <- data.frame(
        sim=x, repl=TRUE, diffmeas=FALSE, exclLo=qLo, exclHi=qHi, 
        pval=pvfr(D=X, ql=qLo, qh=qHi, repl=TRUE)
      )
      out <- rbind(out, new_first)
      
      # and *if* the thresholds aren't zero, we will add two more rows
      # for each simulation
      if (qLo > 0 | qHi > 0){
        
        # add a new row for ...
        new_second <- data.frame(
          sim=x, repl=FALSE, diffmeas=FALSE, exclLo=qLo, exclHi=qHi,
          pval=pvfr(D=X, ql=qLo, qh=qHi, repl=FALSE)
        )
        out <- rbind(out, new_second)
        
        # and then add a new row for ...
        new_third <- data.frame(
          sim=x, repl=FALSE, diffmeas=TRUE, exclLo=qLo, exclHi=qHi,
          pval=pvf(D=X, logical_condition=(
            X$EXCL > quantile(X$EXCL, qLo) & 
            X$EXCL < quantile(X$EXCL, 1-qHi)
          ))
        )
        out <- rbind(out, new_third)
      }
    }
  }
}; rm(new_first); rm(new_second); rm(new_third)

# save the data for later inspection
write.csv(out, 
          paste0("out/chemla-sims_", Part, "points_", numsims, "sims.csv"), 
          row.names=FALSE)

# inspect data...
summary(out)

sum(out$pval < .05) / length(out$pval)

# barplot(table(round(out$pval, 2), useNA="ifany"))
hist(out$pval)
#'

# "single p-value" 
#   --> just pval from the simulated data
res_single <- out[out$exclLo==0 & out$exclHi==0, ]

# barplot(table(round(res_single$pval, 2), useNA="ifany"))
hist(res_single$pval)

table(res_single$pval < .05)
sum(res_single$pval < .05) / length(res_single$pval)

quantile(res_single$pval, .05)
#'

# "multiple thresholds, w/o replacement" (just toss outliers)
#   --> toss everything outside of the threshold
res_mult_NOrepl <- out[
  out$repl==FALSE & 
    out$diffmeas==FALSE | 
    (out$exclLo==0 & out$exclHi==0), 
]
# barplot(table(round(res_mult_NOrepl$pval, 2), useNA="ifany"))
hist(res_mult_NOrepl$pval)

res_mult_NOrepl_aggr <- aggregate(pval ~ sim, min, data=res_mult_NOrepl)

table(res_mult_NOrepl_aggr$pval < .05)
sum(res_mult_NOrepl_aggr$pval < .05) / length(res_mult_NOrepl_aggr$pval)

quantile(res_mult_NOrepl_aggr$pval, .05)
#'

# "multiple thresholds, with replacement" (replace outliers w threshold)
#   --> replace everything outside threshold w the threshold val itself
res_mult_repl <- out[out$repl==TRUE & out$diffmeas==FALSE, ]

# barplot(table(round(res_mult_repl$pval, 2), useNA="ifany"))
hist(res_mult_repl$pval)

res_mult_repl_aggr <- aggregate(pval ~ sim, min, data=res_mult_repl)

table(res_mult_repl_aggr$pval < .05)
sum(res_mult_repl_aggr$pval < .05) / length(res_mult_repl_aggr$pval)

quantile(res_mult_repl_aggr$pval, .05)
#'

# "multiple thresholds, diff measure, w/o replacement" 
#   --> toss everything outside of externally defined threshold
res_mult_diff <- out[
  out$repl==FALSE & 
    out$diffmeas==TRUE | 
    (out$exclLo==0 & out$exclHi==0), 
]
# barplot(table(round(res_mult_diff$pval, 2), useNA="ifany"))
hist(res_mult_diff$pval)

res_mult_diff_agg <- aggregate(pval ~ sim, min, data=res_mult_diff)

table(res_mult_diff_agg$pval < .05)
sum(res_mult_diff_agg$pval < .05) / length(res_mult_diff_agg$pval)

quantile(res_mult_diff_agg$pval, .05)
#'

#'<hr><hr><br><br><br><br><br><br>
#'


### BELOW HERE IS ORIGINAL CODE... --------------------------------------------
### BELOW HERE IS ORIGINAL CODE... --------------------------------------------
### BELOW HERE IS ORIGINAL CODE... --------------------------------------------
if (FALSE){
### BELOW HERE IS ORIGINAL CODE... --------------------------------------------
Part <- 40
REP <- 100
  
pvaluefromreplace <- function (M, C, D, ql, qh, replace = T) {
  # if ql =/= 0
  if (ql) {
    QL <- quantile(D[[M]], ql)
  } else {
    QL <- min(D[[M]]) - 1
  }
  # if qh =/= 0
  if (qh) {
    QH <- quantile(D[[M]], 1 - qh)
  } else {
    QH <- max(D[[M]]) + 1
  }
  # replace set to FALSE
  if (!replace) {
    DD <- subset(D, D[[M]] > QL & D[[M]] < QH)
  }
  else {
    DD <- D
    RL <- DD[[M]] < QL
    DD[[M]][RL] <- QL
    RH <- DD[[M]] > QH
    DD[[M]][RH] <- QH
  }
  a <- summary(aov(DD[[M]] ~ DD[[C]]))
  return(a[[1]][["Pr(>F)"]][1])
}

# ex: 
# M --> "RT"; C --> "Cond"; 
# D --> X; 
# R --> X$EXCL > quantile(X$EXCL, ql) & X$EXCL < quantile(X$EXCL, 1 - qh)
# R = "a condition to subset the data by"
pvaluefrom <- function (M, C, D, R) {
  DD <- subset(D, R)
  a <- summary(aov(DD[[M]] ~ DD[[C]]))
  return(a[[1]][["Pr(>F)"]][1])
}

# breaking down: 
#   X$EXCL > quantile(X$EXCL, ql) & X$EXCL < quantile(X$EXCL, 1 - qh)
# 
# "a random val is greater than lower quantile threshold" [ql'th percentile]
# X$EXCL > quantile(X$EXCL, ql)
# 
# "a random val is less than 1 - higher quantile threshold" [1-qh'th percentile]
# X$EXCL < quantile(X$EXCL, 1 - qh)
# 
# so basically we toss all the rows that have extreme $EXCL values

X <- data.frame(Cond = rep(1:2, each = Part / 2))


RESULTS <-
  data.frame(
    simulation = NULL,
    replacement = NULL,
    differentmeasure = NULL,
    lowexclusion = NULL,
    highexclusion = NULL,
    pvalue = NULL
  )

for (x in 1:REP) {
  #	print(x)
  X$RT <- rnorm(Part)
  X$EXCL <- rnorm(Part)
  
  for (ql in c(0, .025, .05, .075, .1)) {
    for (qh in c(0, .025, .05, .075, .1)) {
      Lfirst <-
        data.frame(
          simulation = x,
          replacement = TRUE,
          differentmeasure = FALSE,
          lowexclusion = ql,
          highexclusion = qh,
          pvalue = pvaluefromreplace("RT", "Cond", X, ql, qh, replace = T)
        )
      RESULTS <- rbind(RESULTS, Lfirst)
      
      if (ql > 0 | qh > 0) {
        Lsecond <-
          data.frame(
            simulation = x,
            replacement = FALSE,
            differentmeasure = FALSE,
            lowexclusion = ql,
            highexclusion = qh,
            pvalue = pvaluefromreplace("RT", "Cond", X, ql, qh, replace = F)
          )
        RESULTS <- rbind(RESULTS, Lsecond)
        
        Lthird <-
          data.frame(
            simulation = x,
            replacement = FALSE,
            differentmeasure = TRUE,
            lowexclusion = ql,
            highexclusion = qh,
            pvalue = pvaluefrom(
              "RT",
              "Cond",
              X,
              X$EXCL > quantile(X$EXCL, ql) & X$EXCL < quantile(X$EXCL, 1 - qh)
            )
          )
        RESULTS <- rbind(RESULTS, Lthird)
      }
      
    }
  }
  
}



# Single p-value
SUBRES <- subset(RESULTS, lowexclusion == 0 & highexclusion == 0)
with(SUBRES, table(pvalue < .05))
with(SUBRES, quantile(pvalue, .05))

# Multiple thresholds, without replacement
SUBRES <-
  subset(
    RESULTS,
    (replacement == FALSE &
       differentmeasure == FALSE) | (lowexclusion == 0 &
                                       highexclusion == 0)
  )
SUBRES <- aggregate(pvalue ~ simulation, min, data = SUBRES)
with(SUBRES, table(pvalue < .05))
with(SUBRES, quantile(pvalue, .05))

# Multiple thresholds, with replacement
SUBRES <-
  subset(RESULTS, replacement == TRUE & differentmeasure == FALSE)
SUBRES <- aggregate(pvalue ~ simulation, min, data = SUBRES)
with(SUBRES, table(pvalue < .05))
with(SUBRES, quantile(pvalue, .05))

# Multiple thresholds, different measure (without replacement)
SUBRES <-
  subset(
    RESULTS,
    (replacement == FALSE &
       differentmeasure == TRUE) | (lowexclusion == 0 & highexclusion == 0)
  )
SUBRES <- aggregate(pvalue ~ simulation, min, data = SUBRES)
with(SUBRES, table(pvalue < .05))
with(SUBRES, quantile(pvalue, .05))



}
#'
#'<link rel="stylesheet" type="text/css"
#'href="https://fonts.googleapis.com/css?family=Open+Sans:300,400,400i,700">
#'
#'<link href="https://fonts.googleapis.com/css?family=Roboto+Mono:300,400,500" rel="stylesheet">
#'
#'  <style>
#'body {
#'  padding: 10px;
#'  font-size: 12pt;
#'  font-family: 'Open Sans', sans-serif;
#'}
#'
#'h1 { 
#'  font-size: 20px;
#'  color: DarkGreen;
#'  font-weight: bold;
#'}
#'
#'h2 { 
#'    font-size: 16px;
#'    color: green;
#'}
#'
#'h3 { 
#'  font-size: 24px;
#'  color: green;
#'  font-weight: bold;
#'}
#'
#'h4 { 
#'  font-size: 18px;
#'  color: green;
#'  font-weight: bolder;
#'  padding-top: 30px;
#'}
#'
#'li {
#'  padding: 3px;
#'}
#'
#'code {
#'  font-family: 'Roboto Mono', monospace;
#'  font-size: 14px;
#'}
#'
#'pre {
#'  font-family: 'Roboto Mono', monospace;
#'  font-size: 14px;
#'}
#'
#'p {
#'  margin-top: 30px;
#'  margin-bottom: 15px;
#'}
#'
#'</style>
#'
