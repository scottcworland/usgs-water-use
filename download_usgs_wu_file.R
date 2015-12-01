
# read in the file with the urls
urlfile <- read.csv('NWIS_wu_urls.csv', header=F, colClasses = 'character')

# read in alabama as a starter
start <- data.frame(read.table(url(urlfile[1,1]), header=T, sep = "\t", na.strings = "-"))
start <- start[-1,]
 
# for loop to add each state
for (i in 2:nrow(urlfile)) {
X <- read.table(url(urlfile[i,1]), header=T, sep = "\t", na.strings = "-")
X <- X[-1,]
combined <- rbind(start,X)
start <- combined
}


# add fips codes
combined$cntyFIPS <- paste0(combined$state_cd,combined$county_cd)

# write the file to a csv !!! add leading zeros in excel
write.csv(combined,"NWIS_national_wateruse_1985_2010_Rout.csv",row.names=F,na="-")