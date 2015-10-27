
library(reshape2); library(ggplot2); library(rgdal); library(ggmap)
library(RColorBrewer); library(plyr)

## load original data
load("USGSWU1985_2010cnty.rda")
d <- WUcnty

# Data processing ----

## subsample
pop = d[,c(1,2,7,12,17,22,27)]
ground = d[,c(1,3,8,13,18,23,28)]
surface = d[,c(1,4,9,14,19,24,29)]
total = d[,c(1,5,10,15,20,25,30)]
wn = cbind(pop[,1],total[,2:length(total)]/pop[,2:length(pop)]*1000)


## rename for simpler indexing
names = c("cntyFIPS","1985","1990","1995","2000","2005","2010")
colnames(pop) = names
colnames(ground) = names
colnames(surface) = names
colnames(total) = names
colnames(wn) = names

pop2 = melt(pop, variable.name = "year", value.name = "pop")
ground2 = melt(ground, variable.name = "year", value.name = "ground")
surface2 = melt(surface, variable.name = "year", value.name = "surface")
total2 = melt(total, variable.name = "year", value.name = "total")
wn2 = melt(wn, variable.name = "year", value.name = "wn")

d2 = cbind(pop2,ground2[,3],surface2[,3],total2[,3],wn2[,3])
colnames(d2)[4:7] = c("ground","surface","total","wn")
d3 = melt(d2, id.vars = c("cntyFIPS", "year","pop"),variable.name = "type",value.name="withdrl")
d3$wn = d3$withdrl/d3$pop * 1000

## extract high values
h.index <- numeric()
hold <- numeric()
for (i in 2:7){
hold <- which(wn[,i]>1000)
h.index <- c(h.index,hold)
}

wn.high <- wn[h.index,]
rownames(wn.high) = NULL


### plot high values
ggplot(melt(wn.high), aes(variable, log10(value))) +
  geom_violin(fill = "grey", alpha = 0.7) + xlab("year") + ylab("log10(wn)") +
  geom_boxplot(width=0.2, fill ="dodgerblue") +
  #geom_dotplot(fill = "grey", binaxis = "y", alpha = 0.5, binwidth = 0.03, stackdir = "center") +
  theme_bw(base_size=20)

## extract low values
l.index <- numeric()
hold <- numeric()
for (i in 2:7){
  hold <- which(wn[,i]<10)
  l.index <- c(l.index,hold)
}

wn.low <- wn[l.index,]
rownames(wn.low) = NULL

### plot low values
ggplot(melt(wn.low), aes(variable, log10(value))) +
  geom_violin(fill = "grey", alpha = 0.7) + xlab("year") + ylab("log10(wn)") +
  geom_boxplot(width=0.2, fill ="dodgerblue") +
  #geom_dotplot(fill = "grey", binaxis = "y", alpha = 0.5, binwidth = 0.03, stackdir = "center") +
  theme_bw(base_size=20)

# boxplot of log10 wn through time and by type ----
p <- ggplot(d3) 
p <- p + geom_hline(aes(yintercept=3), color="red",linetype = 2, size = 1) 
p <- p + geom_hline(aes(yintercept=0), color="red",linetype = 2, size = 1)
p <- p + geom_boxplot(aes(type, log10(wn) , fill = year)) + theme_bw(base_size=16)
p


# maps ----

counties <- readOGR("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\MSA\\county_shapefiles",
                    layer="cb_2013_us_county_500k");
counties2 <- spTransform(counties, CRS("+proj=longlat + datum=WGS84"));
county.fips <- fortify(counties2, region="GEOID");
colnames(county.fips)[7] <- "GEOID";
counties3 <- merge(county.fips,counties2@data,by="GEOID");
cnty.poly <- subset(counties3, long > -130 & long < 0 & lat > 20 & lat < 50);
colnames(cnty.poly)[1] <- "cntyFIPS";
rm(counties,counties2,county.fips,counties3) # remove old county files for space

state.poly <- map_data("state")

### breaks for withdrawals
d4 <- subset(d3, year==2010 & type == "total")
d4$brk <- cut(d4$wn, breaks=c(0, 50, 100, 200, 500, 750, 1000, 10000, 25000), 
              labels=c("0-50", "50-100", "100-200", "200-500", "500-750", "750-1k", "1k-10k", ">10k"), 
              include.lowest=TRUE)

d4$brk2 <- cut(d4$withdrl, breaks=c(0, 5, 10, 20, 40, 80, 160, 320, 640), 
              labels=c("0-5", "5-10", "10-20", "20-40", "40-80", "80-160", "160-320", ">320"), 
              include.lowest=TRUE)

cnty.WU <- merge(cnty.poly,d4,by="cntyFIPS")
cnty.WU <- cnty.WU[order(cnty.WU$order),]

## Lower 48 normalized
m = ggplot(data=cnty.WU) + coord_fixed(1.3)
m = m + geom_polygon(aes(long,lat, group=group,fill=brk), color=NA)
m = m + geom_polygon(data=state.poly,aes(long,lat, group=group), color="grey", fill=NA, size=0.5)
m = m + scale_fill_manual(values=rev(brewer.pal(8, 'YlGnBu')))
m = m + ggtitle("2010 Fresh Water Puplic Supply Withdrawal/2010 County Pop")
m = m + labs(fill="gal/p/d") + theme_blank()
m

## Lower 48 raw
m = ggplot(data=cnty.WU) + coord_fixed(1.3)
m = m + geom_polygon(aes(long,lat, group=group,fill=brk2), color=NA)
m = m + geom_polygon(data=state.poly,aes(long,lat, group=group), color="grey", fill=NA, size=0.5)
m = m + scale_fill_manual(values=rev(brewer.pal(8, 'YlGnBu')))
m = m + ggtitle("2010 Fresh Water Puplic Supply Withdrawal")
m = m + labs(fill="Mgal/day") + theme_blank()
m

## lower 48 norm high and low
wn.high$thold <- ">1000 gal/p/d"
wn.high2 <- melt(wn.high)
wn.high2 <- subset(wn.high2, variable==2010 & value > 1000)
high.map <- merge(cnty.poly,wn.high2,by="cntyFIPS")
high.map <- high.map[order(high.map$order),]

wn.low$thold <- "<10 gal/p/d"
wn.low2 <- melt(wn.low)
wn.low2 <- subset(wn.low2, variable==2010 & value <10)
low.map <- merge(cnty.poly,wn.low2,by="cntyFIPS")
low.map <- low.map[order(low.map$order),]

m = ggplot(data=cnty.WU) + coord_fixed(1.3)
m = m + geom_polygon(data=state.poly,aes(long,lat, group=group), color="black", fill=NA, size=1)
m = m + geom_polygon(data=high.map, aes(long,lat, group=group, fill = thold), alpha=1)
m = m + geom_polygon(data=low.map, aes(long,lat, group=group, fill = thold), alpha=1)
m = m + scale_fill_manual(values = c("steelblue","orange3"))
m = m + theme_blank() + ggtitle("2010 Fresh Water Puplic Supply Withdrawal/2010 County Pop")
m


## New york
NY.WU <- subset(cnty.WU, STATEFP==36)
NY.poly <- subset(state.poly, region == "new york")
NJ.poly <- subset(state.poly, region == "new jersey")
PA.poly <- subset(state.poly, region == "pennsylvania")

## NY city watershed (catskills)
NYCws <- readOGR("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\MSA\\NY_watershed",
                    layer="New_York_City_Watershed");
NYCws2 <- spTransform(NYCws, CRS("+proj=longlat + datum=WGS84"));
NYCws2@data$id = rownames(NYCws2@data)
NYC.ws <- fortify(NYCws2, region="id");

## Deleware River basin shapefile
DRB <- readOGR("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\MSA\\DRB_shapefile",
                 layer="drb_bnd_polygon");
DRB2 <- spTransform(DRB, CRS("+proj=longlat + datum=WGS84"));
DRB2@data$id = rownames(DRB2@data)
DRB.poly <- fortify(DRB2, region="id");

## normalized withdrawals
m = ggplot(data=NY.WU) + coord_fixed(1.3)
m = m + geom_polygon(aes(long,lat, group=group,fill=brk), color="grey", size=0.125)
m = m + geom_polygon(data=NYC.ws, aes(long,lat, group=group), fill="chocolate1",alpha=0.7, color = "chocolate4")
m = m + geom_polygon(data=NY.poly,aes(long,lat, group=group), color="black", fill=NA,size=0.5)
m = m + scale_fill_manual(values=rev(brewer.pal(5, 'YlGnBu')))
m = m + ggtitle("New York withdrawals/pop 2010")
m = m + labs(fill="gal/p/d") + theme_blank()
m

## Deleware river basin
m = ggplot(data=NY.WU) + coord_fixed(1.3)
m = m + geom_polygon(aes(long,lat, group=group,fill=brk), color="grey", size=0.125)
m = m + geom_polygon(data=NY.poly,aes(long,lat, group=group), color="black", fill=NA,size=0.5)
m = m + geom_polygon(data=NJ.poly,aes(long,lat, group=group), color="black", fill="grey",size=0.5)
m = m + geom_polygon(data=PA.poly,aes(long,lat, group=group), color="black", fill="grey",size=0.5)
m = m + geom_polygon(data=DRB.poly, aes(long,lat, group=group), fill="darkorchid1",alpha=0.5, color = "darkorchid4")
m = m + scale_fill_manual(values=rev(brewer.pal(5, 'YlGnBu')))
m = m + ggtitle("New York withdrawals/pop 2010")
m = m + labs(fill="gal/p/d") + theme_blank()
m

## withdrawals
m = ggplot(data=NY.WU) + coord_fixed(1.3)
m = m + geom_polygon(aes(long,lat, group=group,fill=brk2), color="grey", size=0.125)
m = m + geom_polygon(data=NY.poly,aes(long,lat, group=group), color="black", fill=NA,size=0.5)
m = m + geom_polygon(data=NYC.ws, aes(long,lat, group=group), fill="darkorchid1",alpha=0.7, color = "darkorchid4")
m = m + scale_fill_manual(values=rev(brewer.pal(8, 'YlGnBu')))
m = m + ggtitle("New York withdrawals 2010")
m = m + labs(fill="Mgal/day") + theme_blank()
m

## big users in New York
high.NY <- data.frame(NY.WU[which(NY.WU$wn>1000),])
high.NY <- high.NY[,c(1,12,17,19,20)]
high.NY <- high.NY[!duplicated(high.NY),]
rownames(high.NY) = NULL
high.NY

## Texas
TX.WU <- subset(cnty.WU, STATEFP==48)
TX.poly <- subset(state.poly, region == "texas")
TX.cities <- geocode(c("Houston, TX", "San Antonio, TX","Dallas, TX", "Austin, TX"))

### normalized withdrawals
m = ggplot(data=TX.WU) + coord_fixed(1.3)
m = m + geom_polygon(aes(long,lat, group=group,fill=brk), color="grey", size=0.125)
m = m + geom_polygon(data=TX.poly,aes(long,lat, group=group), color="black", fill=NA,size=0.5)
m = m + geom_point(data=TX.cities, aes(lon, lat), color = "black", fill="red", size = 4, pch=23)
m = m + scale_fill_manual(values=rev(brewer.pal(8, 'YlGnBu')))
m = m + ggtitle("Texas withdrawals/pop 2010")
m = m + labs(fill="gal/p/d") + theme_blank()
m

### withdrawals
m = ggplot(data=TX.WU) + coord_fixed(1.3)
m = m + geom_polygon(aes(long,lat, group=group,fill=brk2), color="grey", size=0.125)
m = m + geom_polygon(data=TX.poly,aes(long,lat, group=group), color="black", fill=NA,size=0.5)
m = m + geom_point(data=TX.cities, aes(lon, lat), color = "black", fill="red", size = 4, pch=23)
m = m + scale_fill_manual(values=rev(brewer.pal(8, 'YlGnBu')))
m = m + ggtitle("Texas withdrawals 2010")
m = m + labs(fill="Mgal/day") + theme_blank()
m

### clunky way to find high values
high.TX <- data.frame(TX.WU[which(TX.WU$wn>1000),])
high.TX <- high.TX[,c(1,12,17,19,20)]
high.TX <- high.TX[!duplicated(high.TX),]
rownames(high.TX) = NULL
high.TX
