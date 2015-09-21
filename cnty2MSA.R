
# function for aggregating county data in MSAs

## 1. requires a file with cnty FIPS and MSA FIPS codes
## 2. requires reshape2 package
## 3. The cnty file has to be a dataframe formatted as:

#   cntyFIPS | data1 | data2 | ... 
#   --------------------------------
#     fips   | value | value | ... 


cnty2MSA <- function(cntydata, type){

# load libraries and file
library(reshape2)
load("MSA_cnty_ST_FIPS_REGION.rda")

# find number of columns
n = ncol(cntydata)

# returns counties, MSA, state, and region.
cntys_in_MSA = merge(MSA_cnty_Region,cntydata,"cntyFIPS") 

# create list for later
MSAlist = list(GEOID = cntys_in_MSA$GEOID, MSA_NAME = cntys_in_MSA$MSA_NAME,
               State = cntys_in_MSA$State, Region = cntys_in_MSA$Region)

# aggregate
MSAcnty = aggregate(cntys_in_MSA[,6:n],MSAlist,type)
}

# example
# out = cnty2MSA(cntydata, "sum")
# out = cnty2MSA(cntydata, "mean")