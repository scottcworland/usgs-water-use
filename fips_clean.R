
# function to clean up FIPS
# county FIPS has to be the first column

fips.clean <- function(d){
  
  if (any(d[,1] %in% remove) == TRUE) {
  ## counties to be removed due to changing FIPS
  remove <- c("08014","08001","08013","08059",
              "08123","51560","51005","51780",
              "51083","30113","30031","30067",
              "24031","24031","24003","51015",
              "51790","51143","51590")
  
  d <- d[-which(d[,1] %in% remove),] 
  } else {
    d <- d
  }

  
  ## counties to be renamed due to changing FIPS
  if (any(d[,1]=="12025") == TRUE) {
  d[which(d[,1]=="12025")] = "12086"
  } else {
    d <- d
  }
  
}

# example
# d2 <- fips.clean(d)