### THIS PART IS LOOKING AT PROCESSED RT DATA FROM NEXT STEP BELOW -------



boosh <- read.csv("in/rt_data_au16.csv")
boosh <- boosh %>% mutate(condition = gsub("cond_", "", condition))
boosh$is_word <- ifelse(grepl("_nonword", boosh$condition), "nonword", "word")
summary(boosh$RT)
quantile(boosh$RT, seq(from=0, to=1, by=.05))

# in reality, would prob toss subj w median rt >1000 ms
subj_table <- 
  boosh %>% group_by(subjid) %>% summarize(
    meanrt = mean(RT),
    medianrt = median(RT),
    minrt = min(RT),
    maxrt = max(RT),
    age = unique(age)
)

# 20pct of rt's are above 1k ms
boosh %>% summarize(prop_1kup = sum(RT>=1000) / n())
# 6pct of trials have bad answers (doesnt change much after remove 1k)
boosh %>% summarize(prop_wrong = sum(!correct) / n())
boosh %>% summarize(meanrt=mean(RT), minrt=min(RT), maxrt=max(RT))

# grand mean rt by condition 
boosh %>% filter(RT < 1000) %>% filter(RT > 300) %>% filter(correct) %>% 
  filter(!grepl("filler_", condition)) %>% 
  filter(condition %in% c("rel_word","unr_word")) %>% 
  group_by(condition, target) %>%
  summarize(
    numobs = n(),
    meanrt = mean(RT), sdrt=sd(RT), mdnrt=median(RT)
  ) %T>% print(knitr::kable(.)) %>% 
  ggplot(aes(x=target, y=meanrt, fill=condition)) + 
    geom_bar(stat="identity", position="dodge")

get_md <- function(df, rtcol="RT", th_lo, th_hi){
  
}


boosh %>% filter(RT < 1000) %>% filter(RT > 300) %>% filter(correct) %>% 
  filter(!grepl("filler_", condition)) %>% 
  filter(condition %in% c("rel_word","unr_word")) %>% 
  group_by(condition, target) %>%
  summarize(
    numobs = n(),
    meanrt = mean(RT), sdrt=sd(RT), mdnrt=median(RT)
  ) %>% 
  group_by(condition) %>% summarize(
    meanrt = mean(meanrt)
  )



boosh %>% filter(RT < 1000) %>% 
  ggplot(aes(x=RT, fill=condition)) + 
  geom_histogram(binwidth=25) #geom_density()

boosh %>% filter(RT < 2000) %>% filter(correct) %>% 
  ggplot(aes(x=RT, color=is_word)) + geom_density()



### THIS PART IS FOR REPROCESSING THE DATA W/O SCREENING IT --------

#### LOAD DEPENDENCIES ########################################################
## === === === === === === === === === === === === === === === === === ===
library("plyr")
library("ggplot2"); library("dplyr"); library("magrittr"); library("reshape2")

#### DATA WRANGLING ###########################################################
## === === === === === === === === === === === === === === === === === ===
cols <- c("time","ip","controller","itemNum","elementNum","condition","group",
          "wordNum","word","RT","response","newline","target")

dat <- read.csv("results-nov22.txt",
                comment.char="#",header=FALSE, sep=",")
names(dat) <- cols
rm(cols)

keepers <- 
  c("time","ip","controller","condition","wordNum","word","RT","response","target")

dat <- dat[, keepers]

dat$subjid <- paste(dat$time, dat$ip, sep="|")

subjInfo <- droplevels(dat[dat$controller=="Form", c(keepers, "subjid")])
rm(keepers)
length(levels(subjInfo$ip)) # 55 subjs

# forgot to change language label in html
# check out what languages we have
levels(droplevels(subjInfo[subjInfo$wordNum=="age", ])$word)
langs <- c("Bosnian","Dutch","Georgian","Hindi","Russian",
           "english","English")
notEng <- langs[1:(length(langs)-2)]

# set wordNum to "language" for these vals
subjInfo$wordNum <- as.character(subjInfo$wordNum)
subjInfo[subjInfo$word %in% langs, ]$wordNum <- "language"
rm(langs)

# get ages ready to join w trial results
age <- subjInfo[subjInfo$wordNum=="age", c("subjid","word")]
names(age)[2] <- "age"
age$age <- as.numeric(as.character(age$age))

# get the trial results
dat <- droplevels(dat[dat$controller=="OnlineJudgment", ])

# use subjid instead of ip, bc theres legit ppl w same ip but not same person
dat %>% group_by(subjid) %>% summarize(n=n()) %>% knitr::kable()

# these all have 114 rows instead of 57... 
#   ~~~> but have diff times! so used as id
# 4454fedbe871f772f0c42a2a261a6938
# 5b324d4107af023c968c9bb09eded2f2
# 9e6257e3e34010d79b3f3263726132fd
# a28bb77a3395e6393c3d97807ff654d5

# join w ages
dat <- left_join(x=dat, y=age, by="subjid")
rm(age)

# get trial/item info
trials <- read.csv("items-and-fillers-final.csv", sep="\t")

# join trial/item info w results
dat <- left_join(x=dat, y=trials, by=c("condition","target"))
rm(trials)

dat$response <- ifelse(dat$response=="F","word","nonword")
dat$response %>% table(useNA="always")

# check dist of RT's -- outliers skewing it
hist(log(dat$RT), plot=TRUE)
hist(dat$RT, plot=TRUE)

# eliminate all trials w incorrect response
wConds <- c("cond_filler_rel_word","cond_filler_unr_word",
            "cond_rel_word","cond_unr_word")
nwConds <- c("cond_filler_nonword","cond_rel_nonword","cond_unr_nonword")

dat$correct <- ifelse(
  (dat$response=="word" & dat$condition %in% wConds) |
    (dat$response=="nonword" & dat$condition %in% nwConds), TRUE, FALSE
)
rm(wConds); rm(nwConds)

table(dat$correct, useNA="always")


#### MAKE SOME PLOTS + LOOK AT CONDITION MEANS ETC ############################
## === === === === === === === === === === === === === === === === === ===

# look at reaction times -- means by condition
cMeans <- aggregate(RT ~ condition, FUN="mean", data=dat)
names(cMeans)[2] <- "rt_mean"
cSD <- aggregate(RT ~ condition, FUN="sd", data=dat)
names(cSD)[2] <- "rt_sd"
cMeans <- left_join(x=cMeans, y=cSD, by="condition")
cMeans
rm(cSD)
# save condition means
# write.csv(cMeans, "condition-means.csv", row.names=FALSE)

# set age bins
table(dat$age, useNA="always")
dat$age_group <- 
  ifelse(dat$age < 18, "<18 yo", ifelse(
    dat$age < 35, "18-34 years old", "35+ years old"
  ))

# eliminate unnecessary columns + rearrange
dat <- dat[, c("subjid","age","age_group","item_id","condition",
               "prime","target","RT","response","correct")]


# recode subj id's so they're nice
idlkup <- setNames(
  paste0("subj", sprintf("%02d", seq_along(unique(dat$subjid)))),
  nm=unique(dat$subjid)
)

dat$subjid %>% unique %>% length
dat$subjid <- idlkup[dat$subjid]

# save the cleaned data
# write.csv(dat, "../../../in/rt_data_au16.csv", row.names=FALSE)




### BELOW HERE IS SKETCHPADDDE ---------

expfile <- "ignore/data/results-teaching_expt/results-nov22-clean.csv"
exp_dat <- read.csv(expfile, stringsAsFactors=FALSE); rm(expfile)
library("ggplot2"); library("dplyr"); library("magrittr"); library("reshape2")

head(exp_dat)




boosh <- read.csv("realdat-res.csv", stringsAsFactors=FALSE)
boosh <- unique(boosh[, -1])
dim(unique(boosh)); dim(unique(boosh[,-1]))
boosh[boosh$trim_top==0 & boosh$trim_bot==0 & boosh$strat=="nothing" & boosh$sim<5, ]
boosh[boosh$trim_top==.1 & boosh$strat=="exclude", c("trim_top","trim_bot","diffmeans_raw","diffmeans_cut")]


boosh[c(3,12), c("trim_top","trim_bot","diffmeans_raw","diffmeans_cut")]
dat %>% group_by(cond) %>%  summarize(meanrt=mean(data))
# %>% filter(data>quantile(data,.1))


dd <- data.frame(c1=c(1,1,3,5),c2=c(1,1,2,3),c3=c(1,1,2,3))
unique(dd)
dd[,-2]









### FROMS CRIPT 

# use rt data from psycholing class
expfile <- "ignore/data/results-teaching_expt/results-nov22-clean.csv"
exp_dat <- read.csv(expfile, stringsAsFactors=FALSE)

# bad ones already tossed: table(exp_dat$correct, useNA="ifany")
# table(exp_dat$condition, useNA="ifany"); hist(exp_dat$RT)

# relabel the conditions we want to compare
cond_lkup <- c(cond_rel_word="A", cond_unr_word="B")

# build a df
dat <- data.frame(
  data = exp_dat$RT[exp_dat$condition %in% names(cond_lkup)],
  cond = cond_lkup[exp_dat$condition[exp_dat$condition %in% names(cond_lkup)]],
  stringsAsFactors=FALSE
)
# explore data a bit
hist(dat$data)
library("ggplot2")
ggplot(dat, aes(x=cond, y=data)) + geom_boxplot()
ggplot(dat, aes(x=data, fill=cond)) + geom_density()
ggplot(dat, aes(x=data, fill=cond)) + geom_histogram(binwidth=20)
dat %>% group_by(cond) %>% summarize(
  mean=mean(data), sd=sd(data),
  q05=quantile(data, .05), q25=quantile(data, .25), q50=quantile(data, .5),
  q75=quantile(data, .75), q95=quantile(data, .95)
) %>% knitr::kable(digits=2)

t.test(dat$data[dat$cond=="A"], 
       dat$data[dat$cond=="B"], alternative="less")
t.test(log(dat$data[dat$cond=="A"]), 
       log(dat$data[dat$cond=="B"]), alternative="less")

dat_trim <- dat[dat$data < quantile(dat$data, .95), ]

ggplot(dat_trim, aes(x=cond, y=data)) + geom_boxplot()
ggplot(dat_trim, aes(x=data, fill=cond)) + geom_density()
ggplot(dat_trim, aes(x=data, fill=cond)) + geom_histogram(binwidth=20)

t.test(dat_trim$data[dat_trim$cond=="A"], 
       dat_trim$data[dat_trim$cond=="B"], alternative="less")
t.test(log(dat_trim$data[dat_trim$cond=="A"]), 
       log(dat_trim$data[dat_trim$cond=="B"]), alternative="less")
### ----------

# hyp is that A is faster/smaller than B

# thresholds, regions, strategies to consider
thetas  <- c(0, .025, .05, .075, .1)
regions <- c("hi", "lo") # inactive
strats  <- c("exclude", "impute_theta", "impute_gmean", "impute_cmean") 
# total trials per dataset, and num simmed datasets
num_data_points <- length(dat$data)
num_sims        <- 1
# grand mean and sd within each sim
grand_mu    <- mean(dat$data)
grand_sigma <- sd(dat$data)
# make sim_data -- just a num_data_points*1 matrix since this is real data
sim_data <- matrix(dat$data, ncol=1)
# make cont -- just will have slots for one sim
cont <- make_simshell(num_sims=num_sims, thetas=thetas, strats=strats)
# reset dat since we're storing the real data in sim_data
dat <- data.frame(
  data = numeric(num_data_points), 
  cond = cond_lkup[exp_dat$condition[exp_dat$condition %in% names(cond_lkup)]],
  stringsAsFactors=FALSE
)



