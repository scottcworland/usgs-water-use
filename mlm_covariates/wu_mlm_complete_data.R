
library(stringr)
library(reshape2)

setwd("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\USGSwaterUse\\usgs-water-use\\mlm_covariates")

# source fips clean function
source("https://raw.githubusercontent.com/scworland-usgs/usgs-water-use/master/fips_clean.R")

# load covariates
covariates <- read.csv("cnty_wu_covariates_combined.csv")
covariates$cntyFIPS <- sprintf("%05d", covariates$cntyFIPS)
covariates$year <- as.factor(covariates$year)
covariates <- fips.clean(covariates)

# load updated water use data
wtot <- read.csv("revised_wn.csv")
wtot$cntyFIPS <- sprintf("%05d", wtot$cntyFIPS)
wtot <- fips.clean(wtot)

# load states and regions
levels <- read.csv("cnty_state_region_msa.csv")
levels$cntyFIPS <- sprintf("%05d", levels$cntyFIPS)
levels <- fips.clean(levels)

cdc_urban <- read.csv("cdc_urban_class.csv")
cdc_urban$cntyFIPS <- sprintf("%05d", cdc_urban$cntyFIPS)
cdc_urban <- fips.clean(cdc_urban)

# subsample freshwater public supply withdrawals
wtotm <- melt(wtot, id.vars="cntyFIPS")
wtotm$year <- as.factor(str_sub(wtotm$variable,-4))
wtotm = merge(wtotm,levels,by="cntyFIPS")
wtotm = merge(wtotm,cdc_urban,by="cntyFIPS")
wtotm <- wtotm[,-2]
colnames(wtotm)[2] <- "wtot"
wtotm$year <- as.factor(wtotm$year)

# combine withdrawal data with covariates
wulist <- list(covariates, wtotm)
d <- join_all(wulist, by = c("cntyFIPS","year"), type="inner") 
cnty_wu_data <- cbind(d[,c(1,29:ncol(d)-2,ncol(d),2)], d$wtot, d[,c(3:ncol(covariates),ncol(d)-1)])
colnames(cnty_wu_data)[11] <- "wn"

# replace high values with min of cnty
loc <- which(cnty_wu_data$wn>800)

## there has to be a better way to do this
for (i in 1:length(loc)){
 sub <- which(cnty_wu_data$cntyFIPS == cnty_wu_data$cntyFIPS[loc[i]])
 cnty_wu_data$wn[loc[i]] = as.numeric(aggregate(wn~cntyFIPS, data=cnty_wu_data[sub,], min)[2])
}

# export csv
write.csv(cnty_wu_data, file = "cnty_wu_data_complete_march2016.csv", row.names=F)
