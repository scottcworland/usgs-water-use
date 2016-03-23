library(reshape2)
library(plyr)
library(dplyr)

# Calculate Wn
load("USGSWU1985_2010cnty.rda")
d <- WUcnty
pop = d[,c(1,2,7,12,17,22,27)]
total = d[,c(1,5,10,15,20,25,30)]
wn = cbind(pop[,1],total[,2:length(total)]/pop[,2:length(pop)]*1000)
names = c("cntyFIPS","1985","1990","1995","2000","2005","2010")
colnames(wn) = names

wn <- melt(wn, id.vars=c("cntyFIPS"))
colnames(wn)[2:3] <- c("year","wn")

# calculate Wn* (variable wn2=wn*)
psdel <- read.csv("ps_del.csv")
psdel$wn2 <- psdel$PS_del/psdel$County_pop * 1000
psdel <- dcast(psdel, cntyFIPS ~ year, value.var="wn2")
psdel[,5] <- (psdel[,4]+psdel[,6])/2

wn2 <- melt(psdel, id.vars=c("cntyFIPS"))
colnames(wn2)[2:3] <- c("year","wn2")
wn2$cntyFIPS <- sprintf("%05d", wn2$cntyFIPS)


# function to clean up FIPS
fips.clean <- function(cntydata){
  
  ## counties to be removed due to changing FIPS
  remove <- c("08014","08001","08013","08059",
              "08123","51560","51005","51780",
              "51083","30113","30031","30067",
              "24031","24031","24003","51015",
              "51790","51143","51590")
  
  wn <- wn[-which(wn$cntyFIPS %in% remove),]
  
  ## counties to be renamed due to changing FIPS
  #wn[which(wn$cntyFIPS==12025)] = "12086"
}

# calculate 5% and 95% percentiles
wn <- wn[complete.cases(wn$wn),]
t5 <- 5*round(quantile(wn$wn, .05)/5)
t95 <- 5*round(quantile(wn$wn, .95)/5)

# merge wn and wn*
wnn <- inner_join(wn, wn2, by=c("cntyFIPS","year"))

# index of anomalous values
anom.index <- which(wnn$wn < t5 | wnn$wn > t95)

# replace just wn values outside thold with wn* values
wn.out <- wnn
wn.out$wn[anom.index] <- wnn$wn2[anom.index]

# replace every year for county
anom.index2 <- which(wnn$cntyFIPS %in% wnn$cntyFIPS[anom.index])
wn.out2 <- wnn
wn.out2$wn[anom.index2] <- wnn$wn2[anom.index2]

# reformat
wn.out2 <- dcast(wn.out2[,1:3], cntyFIPS ~ year, value.var="wn")

# export new data file
write.csv(wn.out2, file = "revised_wn.csv", row.names = F)


