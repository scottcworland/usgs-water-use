
library(readxl); library(reshape2); library(dplyr); library(stringr);
library(doParallel); library(plyr)

setwd("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\USGSwaterUse\\usgs-water-use\\mlm_covariates")

# read in each sheet from the master excel file
vars <- excel_sheets("usgswu_all_covariates_1985_2010_master.xlsx")

for (i in 2:length(vars)){
  d <- readxl::read_excel("usgswu_all_covariates_1985_2010_master.xlsx", sheet = vars[i], skip = 0)
  d <- d[,!apply(d , 2 , function(x) all(is.na(x)))] # remove columns that are all NA
  if(any(colnames(d) == "cntyFIPS" & is.numeric(d$cntyFIPS))){ # convert cntyFIPS to 5 characters
    d$cntyFIPS <- sprintf("%05d", d$cntyFIPS)
  }
  assign(vars[i], data.frame(d)) # assign the name of the sheet as variable name
  rm(d)
}

# merge every data frame using dplyr to subset by complete county fips

## create list of dataframes
cntylist <- list(temp,precip,pdsi,landcover,edu,pop_age,voting_pop,pvi,mhi,poverty,building_permits)

## use type="full" to include everything, type="inner" to only keep complete
allvars <- join_all(cntylist, by = "cntyFIPS", type="inner") 

# PRECIPITATION ----
precip.sub <- subset(precip, cntyFIPS %in% allvars$cntyFIPS)
precipm <- melt(precip.sub, id.vars="cntyFIPS")
precipm$year <- as.numeric(str_sub(precipm$variable,-4))
precipm$month <- str_sub(precipm$variable,0,-6)
precipm <- subset(precipm, year > 1980)

## create class of 5 year intervals (1985-1990, 1990-1995...etc)
precipm$fiveyr <- cut(precipm$year, breaks=c(1980,1985,1990,1995,2000,2005,2010),
                      labels = c("1985","1990","1995","2000","2005","2010"),include.lowest=T)

table(precipm[,c(4,6)]) # see if it worked

## create class of seasons
precipm$seasons <- ifelse(precipm$month %in% c("Jun","Jul","Aug","Sep"), "summer","notsummer")

table(precipm[,c(5,7)]) # see if it worked

## take the mean of the last 5 years
precip2 <- aggregate(value ~ cntyFIPS + fiveyr + seasons, data=precipm, mean)
precip2 <- subset(precip2, seasons=="summer")
precip2 <- precip2[,-3]
colnames(precip2)[2:3] <- c("year","mean_summer_precip_5yr")
precip2$year <- as.factor(precip2$year)

# TEMPERATURE ----
temp.sub <- subset(temp, cntyFIPS %in% allvars$cntyFIPS)
tempm <- melt(temp.sub, id.vars="cntyFIPS")
tempm $year <- as.numeric(str_sub(tempm $variable,-4))
tempm$month <- str_sub(tempm$variable,0,-6)
tempm <- subset(tempm, year > 1980)

## create class of 5 year intervals (1985-1990, 1990-1995...etc)
tempm$fiveyr <- cut(tempm$year, breaks=c(1980,1985,1990,1995,2000,2005,2010),
                      labels = c("1985","1990","1995","2000","2005","2010"),include.lowest=T)

table(tempm[,c(4,6)]) # see if it worked

## create class of seasons
tempm$seasons <- ifelse(tempm$month %in% c("Jun","Jul","Aug","Sep"), "summer","notsummer")

table(tempm[,c(5,7)]) # see if it worked

## take the mean of max temp for last 5 years
temp2 <- aggregate(value ~ cntyFIPS + year + fiveyr, data=tempm, max)
colnames(temp2)[4] <- "yrmax"
temp2 <- aggregate(yrmax ~ cntyFIPS + fiveyr, data=temp2, mean)
colnames(temp2)[2:3] <- c("year","mean_max_temp_5yr")
temp2$year <- as.factor(temp2$year)

# PDSI ----
pdsi.sub <- subset(pdsi, cntyFIPS %in% allvars$cntyFIPS)
pdsim <- melt(pdsi.sub, id.vars="cntyFIPS")
pdsim$year <- as.numeric(str_sub(pdsim $variable,-4))
pdsim$month <- str_sub(pdsim$variable,0,-6)
pdsim <- subset(pdsim, year > 1980)

## create class of 5 year intervals (1985-1990, 1990-1995...etc)
pdsim$fiveyr <- cut(pdsim$year, breaks=c(1980,1985,1990,1995,2000,2005,2010),
                    labels = c("1985","1990","1995","2000","2005","2010"),include.lowest=T)

table(pdsim[,c(4,6)]) # see if it worked

# mean min pdsi for 5 year intervals
pdsi2 <- aggregate(value ~ cntyFIPS + year + fiveyr, data=pdsim, min)
colnames(pdsi2)[4] <- "yrmin"
pdsi2 <- aggregate(yrmin ~ cntyFIPS + fiveyr, data=pdsi2, mean)
colnames(pdsi2)[2:3] <- c("year","mean_min_pdsi_5yr")

# number of crossings below threshold
thold <- -4
pdsim$cross_class <- pdsim$value < thold
num_cross_pdsi <- aggregate(cross_class ~ cntyFIPS + fiveyr, data=pdsim, sum)
pdsi2$num_cross_pdsi_5yr <- num_cross_pdsi[,3]

#length of longest excursion (months)
length_cross_pdsi_5yr <- matrix(data=NA, nrow=length(unique(pdsim$cntyFIPS)), ncol=length(unique(pdsim$fiveyr)))
for (j in 1:length(unique(pdsim$fiveyr))) { 
  for (i in 1:length(unique(pdsim$cntyFIPS))){
      y <- subset(pdsim, cntyFIPS %in% pdsi2$cntyFIPS[i] & pdsim$fiveyr == unique(pdsim$fiveyr)[j])
      y2 <- rle(y$cross_class == TRUE)
      length_cross_pdsi_5yr[i,j] <- min(tapply(y2$lengths, y2$values, max))
      length_cross_pdsi_5yr[i,j] <- ifelse(length_cross_pdsi_5yr[i,j] == 60, 0, length_cross_pdsi_5yr[i,j])
  }
}

dfcross <- data.frame(length_cross_pdsi_5yr)
dfcrossm <- melt(dfcross)

pdsi2$length_cross_pdsi_5yr <- dfcrossm$value
pdsi2$year <- as.factor(pdsi2$year)


#If never drops below thold, make run = 0... quality control step
pdsi2$length_cross_pdsi_5yr[which(pdsi2$num_cross_pdsi_5yr==0)] <- 0

# LANDCOVER ----
landcover.sub <- subset(landcover[,-2], cntyFIPS %in% allvars$cntyFIPS)
landcoverm <- melt(landcover.sub, id.vars="cntyFIPS")
landcoverm$year.actual <- as.numeric(str_sub(landcoverm$variable,-4))
landcoverm$type <- as.character(str_sub(landcoverm$variable,0,-6))

## repeat years for 1992 --> 1990 and 1995 and 2002 --> 2000 and 2005
landcoverm2 <- data.frame(rbind(subset(landcoverm,year.actual==1982),
                                subset(landcoverm,year.actual==1992),
                                subset(landcoverm,year.actual==1992),
                                subset(landcoverm,year.actual==2002),
                                subset(landcoverm,year.actual==2002),
                                subset(landcoverm,year.actual==2012)))

landcoverm2$year <- c(rep(1985, nrow(subset(landcoverm,year.actual==1982))),
                          rep(1990, nrow(subset(landcoverm,year.actual==1992))),
                          rep(1995, nrow(subset(landcoverm,year.actual==1992))),
                          rep(2000, nrow(subset(landcoverm,year.actual==2002))),
                          rep(2005, nrow(subset(landcoverm,year.actual==2002))),
                          rep(2010, nrow(subset(landcoverm,year.actual==2012))))

landcoverm2 <- landcoverm2[,-c(2,4)]

# recast into the common format
landcover3 <- dcast(landcoverm2, cntyFIPS + year ~ type, value.var="value")
landcover3$year <- as.factor(landcover3$year)

# PVI ----
pvi.sub <- subset(pvi, cntyFIPS %in% allvars$cntyFIPS)
pvim <- melt(pvi.sub, id.vars="cntyFIPS")
pvim$pvi.year <- as.factor(str_sub(pvim$variable,-4))

pvi2 <- subset(pvim[,-2], pvi.year %in% c(1984,1988,1992,2000,2004,2008))
pvi2$year <- precip2$year
pvi2 <- pvi2[,-3]
colnames(pvi2)[2] <- "pvi"
pvi2$year <- as.factor(pvi2$year)

# Voting pop ----
voting_pop.sub <- subset(voting_pop, cntyFIPS %in% allvars$cntyFIPS)
voting_popm <- melt(voting_pop.sub, id.vars="cntyFIPS")
voting_popm$voting_pop.year <- as.factor(str_sub(voting_popm$variable,-4))

voting_pop2 <- subset(voting_popm[,-2], voting_pop.year %in% c(1984,1988,1992,2000,2004,2008))
voting_pop2$year <- precip2$year
voting_pop2 <- voting_pop2[,-3]
colnames(voting_pop2)[2] <- "voting_pop"
voting_pop2$year <- as.factor(voting_pop2$year)

# MHI ----
colnames(mhi)[3] <- "MHI_1990"
mhi.sub <- subset(mhi, cntyFIPS %in% allvars$cntyFIPS)
mhim <- melt(mhi.sub, id.vars="cntyFIPS")
mhim$year <- as.factor(str_sub(mhim$variable,-4))

mhi2 <- mhim[,-2]
colnames(mhi2)[2] <- "mhi"
mhi2$year <- as.factor(mhi2$year)

# POVERTY ----
colnames(poverty)[2:3] <- c("percent_povert_1985","percent_povert_1990")
poverty.sub <- subset(poverty, cntyFIPS %in% allvars$cntyFIPS)
povertym <- melt(poverty.sub, id.vars="cntyFIPS")
povertym$year <- as.factor(str_sub(povertym$variable,-4))

poverty2 <- povertym[,-2]
colnames(poverty2)[2] <- "poverty"
poverty2$year <- as.factor(poverty2$year)

# EDU ----
edu.sub <- subset(edu[,-c(2:3)], cntyFIPS %in% allvars$cntyFIPS)
edum <- melt(edu.sub, id.vars="cntyFIPS")
edum$year <- as.numeric(str_sub(edum$variable,-4))
edum$type <- as.character(str_sub(edum$variable,0,-6))
edum <- subset(edum, year > 1980)

edu2 <- dcast(edum, cntyFIPS + year ~ type, value.var="value")
edu2$year <- as.factor(edu2$year)

# POPULATION AGE ----
pop_age <- data.frame(lapply(pop_age,as.numeric))
pop_age$cntyFIPS <- sprintf("%05d", pop_age$cntyFIPS)

pop_age$popGrowth_1985 <- (pop_age$pop1985 - pop_age$pop1980)/pop_age$pop1980
pop_age$popGrowth_1990 <- (pop_age$pop1990 - pop_age$pop1985)/pop_age$pop1985
pop_age$popGrowth_1995 <- (pop_age$pop1995 - pop_age$pop1990)/pop_age$pop1990
pop_age$popGrowth_2000 <- (pop_age$pop2000 - pop_age$pop1995)/pop_age$pop1995
pop_age$popGrowth_2005 <- (pop_age$pop2005 - pop_age$pop2000)/pop_age$pop2000
pop_age$popGrowth_2010 <- (pop_age$pop2010 - pop_age$pop2005)/pop_age$pop2005

pop_age.sub <- subset(pop_age[,-c(2:3)], cntyFIPS %in% allvars$cntyFIPS)
pop_agem <- melt(pop_age.sub, id.vars="cntyFIPS")
pop_agem$year <- as.numeric(str_sub(pop_agem$variable,-4))
pop_agem$type <- as.character(str_sub(pop_agem$variable,0,-6))
pop_agem <- subset(pop_agem, year > 1980)

pop_age2 <- dcast(pop_agem, cntyFIPS + year ~ type, value.var="value")
colnames(pop_age2)[c(3:6)] <- c("pop_over50","pop","popGrowth_5yr","pop_under20")
pop_age2$year <- as.factor(pop_age2$year)
                                  

# BUILDING PERMITS ----
building_permits.sub <- subset(building_permits[,-c(7,8)], cntyFIPS %in% allvars$cntyFIPS)
building_permitsm <- melt(building_permits.sub, id.vars="cntyFIPS")
building_permitsm$year.actual <- as.numeric(str_sub(building_permitsm$variable,-4))

## repeat years for 1990 for 1985 and 1990
building_permitsm2 <- data.frame(rbind(subset(building_permitsm,year.actual==1990),
                                subset(building_permitsm,year.actual==1990),
                                subset(building_permitsm,year.actual==1995),
                                subset(building_permitsm,year.actual==2000),
                                subset(building_permitsm,year.actual==2005),
                                subset(building_permitsm,year.actual==2010)))

building_permitsm2$year <- precip2$year
building_permits2 <- building_permitsm2[,-c(2,4)]
colnames(building_permits2)[2] <- "building_permits"
building_permits2$year <- as.factor(building_permits2$year)

# COMBINE COVARIATE DATA ----

cntylist2 <- list(precip2,
                  temp2,
                  landcover3,
                  pvi2,
                  voting_pop2,
                  mhi2,
                  poverty2,
                  edu2,
                  pop_age2)

cnty_wu_covariates_combined <- join_all(cntylist2, by = c("cntyFIPS","year"), type="inner") 

cnty_wu_covariates_combined <- cnty_wu_covariates_combined[order(cnty_wu_covariates_combined$cntyFIPS,
                                                                 cnty_wu_covariates_combined$year), ]


write.csv(cnty_wu_covariates_combined, file ="cnty_wu_covariates_combined.csv", row.names=F)


