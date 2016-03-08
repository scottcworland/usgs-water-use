
setwd("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\USGSwaterUse\\usgs-water-use\\mlm covariates")

covariates <- read.csv("cnty_wu_covariates_combined.csv")
covariates$cntyFIPS <- sprintf("%05d", covariates$cntyFIPS)
covariates$year <- as.factor(covariates$year)

# load updated water use data
load("USGSWU1985_2010cnty.rda") 

# load states and regions
levels <- read.csv("cnty_state_region_msa.csv")
levels$cntyFIPS <- sprintf("%05d", levels$cntyFIPS)

# subsample freshwater public supply withdrawals
wtot = WUcnty[,c(1,5,10,15,20,25,30)]
wtotm <- melt(wtot, id.vars="cntyFIPS")
wtotm$year <- as.factor(str_sub(wtotm$variable,-4))
wtotm = merge(wtotm,levels,by="cntyFIPS")
wtotm <- wtotm[,-2]
colnames(wtotm)[2] <- "wtot"
wtotm$year <- as.factor(wtotm$year)

# combine withdrawal data with covariates
wulist <- list(covariates, wtotm)
d <- join_all(wulist, by = c("cntyFIPS","year"), type="inner") 
cnty_wu_data <- cbind(d[,c(1,29:ncol(d)-2,2)], d$wtot/d$pop*1000000, d[,c(4:ncol(covariates),ncol(d))])
colnames(cnty_wu_data)[9] <- "wn"

# replace high values with min of cnty
loc <- which(cnty_wu_data$wn>800)

## there has to be a better way to do this
for (i in 1:length(loc)){
 sub <- which(cnty_wu_data$cntyFIPS == cnty_wu_data$cntyFIPS[loc[i]])
 cnty_wu_data$wn[loc[i]] = as.numeric(aggregate(wn~cntyFIPS, data=cnty_wu_data[sub,], min)[2])
}

# export csv
write.csv(cnty_wu_data, file = "cnty_wu_data_complete_march2016.csv")
