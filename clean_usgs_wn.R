library(reshape2)
library(plyr)
library(dplyr)

# Calculate Wn
load("USGSWU1985_2010cnty.rda")
d <- WUcnty
pop = d[,c(1,2,7,12,17,22,27)] # subset population columns
total = d[,c(1,5,10,15,20,25,30)] # subset total freshwater withdrawals
wn = cbind(pop[,1],total[,2:length(total)]/pop[,2:length(pop)]*1000) # calculate Wn
names = c("cntyFIPS","1985","1990","1995","2000","2005","2010")
colnames(wn) = names # rename columns

wn <- melt(wn, id.vars=c("cntyFIPS")) # convert to long format
colnames(wn)[2:3] <- c("year","wn") # rename new columns

# calculate Wn* (variable wn2=wn*)
psdel <- read.csv("ps_del.csv") # load delivery data
psdel$wn2 <- psdel$PS_del/psdel$County_pop * 1000 # calculate Wn*
psdel <- dcast(psdel, cntyFIPS ~ year, value.var="wn2") # convert to wide format
psdel[,5] <- (psdel[,4]+psdel[,6])/2 # interpolate Wn* for year 2000

wn2 <- melt(psdel, id.vars=c("cntyFIPS")) # convert to long format
colnames(wn2)[2:3] <- c("year","wn2") # rename columns
wn2$cntyFIPS <- sprintf("%05d", wn2$cntyFIPS) # add leading zero to cnty FIPS


# function to clean up FIPS
fips.clean <- function(d){
  
  ## counties to be removed due to changing FIPS
  remove <- c("08014","08001","08013","08059",
              "08123","51560","51005","51780",
              "51083","30113","30031","30067",
              "24031","24031","24003","51015",
              "51790","51143","51590")
  
  ### remove counties in above list
  if (any(d[,1] %in% remove) == TRUE) {
    d <- d[-which(d[,1] %in% remove),] 
  } else {
    d <- d
  }
  
  
  ## Change FIPS for Miami-Dade County, FL 
  if (any(d[,1]=="12025") == TRUE) {
    d[which(d[,1]=="12025")] = "12086"
  } else {
    d <- d
  }
  
}

wn <- fips.clean(wn)

# calculate 5% and 95% percentiles rounded to nearest 5 or 10
# IQR of 90%
wn <- wn[complete.cases(wn$wn),] # remove NAs (if any)
t5 <- 5*round(quantile(wn$wn, .05)/5) # 5% percentile
t95 <- 5*round(quantile(wn$wn, .95)/5) # 95% percentile

# merge wn and wn*
wnn <- inner_join(wn, wn2, by=c("cntyFIPS","year")) # merge Wn and Wn* by cnty FIPS

# index of anomalous values
anom.index <- which(wnn$wn < t5 | wnn$wn > t95)

# replace just Wn values outside thold with Wn* values
wn.out <- wnn
wn.out$wn[anom.index] <- wnn$wn2[anom.index]

# replace every year for county if at least one year if outside tholds
anom.index2 <- which(wnn$cntyFIPS %in% wnn$cntyFIPS[anom.index])
wn.out2 <- wnn
wn.out2$wn[anom.index2] <- wnn$wn2[anom.index2]

# Remove any NAs
wn.out2 = wn.out2[complete.cases(wn.out2),]

# reformat to wide matrix
wn.out2 <- dcast(wn.out2[,1:3], cntyFIPS ~ year, value.var="wn")

# export new data file
write.csv(wn.out2, file = "revised_wn.csv", row.names = F)


