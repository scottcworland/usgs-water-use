
library(reshape2); library(ggplot2); library(rgdal); library(ggmap)
library(RColorBrewer); library(plyr); library(rgeos); library(maptools);
library(gridExtra)

setwd("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\USGSwaterUse\\usgs-water-use")

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
wn2 = wn2[complete.cases(wn2),]
       
t5 <- 5*round(quantile(wn2$wn, .05)/5)
t95 <- 5*round(quantile(wn2$wn, .95)/5)

d2 = cbind(pop2,ground2[,3],surface2[,3],total2[,3])
colnames(d2)[4:6] = c("ground","surface","total")
d3 = melt(d2, id.vars = c("cntyFIPS", "year","pop"),variable.name = "type",value.name="withdrl")
d3$wn = d3$withdrl/d3$pop * 1000
d3 = na.omit(d3)


## extract high values
h.index <- numeric()
hold <- numeric()
for (i in 2:7){
hold <- which(wn[,i]>t95)
h.index <- c(h.index,hold)
}

wn.high <- wn[h.index,]
wn.high <- wn.high[!duplicated(wn.high$cntyFIPS),]
rownames(wn.high) = NULL
write.csv(wn.high, file = "wn_high.csv", row.names=F)

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
  hold <- which(wn[,i]<t5)
  l.index <- c(l.index,hold)
}

wn.low <- wn[l.index,]
wn.low <- wn.low[!duplicated(wn.low$cntyFIPS),]
rownames(wn.low) = NULL
write.csv(wn.low, file = "wn_high.csv", row.names=F)

### population for low and high
pop.anom <- pop[c(h.index,l.index),]
pop.anom[,2:7] <- pop.anom[,2:7]*1000
pop.anom  <- pop.anom[!duplicated(pop.anom$cntyFIPS),]
rownames(pop.anom) = NULL

pop.index <- numeric()
hold <- numeric()
for (i in 2:7){
  hold <- which(pop.anom[,i]>500000)
  pop.index <- c(pop.index,hold)
}

pop.anom2 <- pop.anom[pop.index,]
pop.anom2  <- pop.anom2[!duplicated(pop.anom2$cntyFIPS),]
rownames(pop.anom2) = NULL

### plot low values
ggplot(melt(wn.low), aes(variable, log10(value))) +
  geom_violin(fill = "grey", alpha = 0.7) + xlab("year") + ylab("log10(wn)") +
  geom_boxplot(width=0.2, fill ="dodgerblue") +
  #geom_dotplot(fill = "grey", binaxis = "y", alpha = 0.5, binwidth = 0.03, stackdir = "center") +
  theme_bw(base_size=20)

# plots log10 wn through time and by type ----
## there are issues with the medians

test <- ddply(d3, c("year","type"), summarise, mean = mean(wn, na.rm=T))
d3.2 <- merge(d3,test,by=c("year","type"))

#d3.2$type <- factor(d3.2$type, levels = c("total","surface","ground"))

## boxplots               
p <- ggplot(subset(d3.2, type == "total"), aes(year, wn)) + geom_boxplot(aes(fill = as.factor(round(med,0))))
p <- p + scale_y_log10(breaks=c(1,10,100,1000,10000),labels=c(1,10,100,1000,10000)) 
p <- p + scale_fill_manual(values = (brewer.pal(n=9,name = 'GnBu')))
p <- p + labs(y = "Wn [gal/p/d]",fill="median") + theme_bw(base_size=18)
p <- p + theme(plot.margin=unit(c(3,3,3,3),"cm"))
p

## Histograms
p <- ggplot(subset(d3.2, type == "total")) 
p <- p + geom_histogram(aes(x=wn, y=..density..,fill=mean), bins=50, color="black", size=0.5)
#p <- p + geom_density(aes(x=wn),alpha=1, adjust = 1, size=1, color = "black")
p <- p + scale_x_log10(breaks=c(1,10,100,1000,10000),labels=c(1,10,100,1000,10000)) 
p <- p + geom_vline(aes(xintercept=t5), linetype=2) + geom_vline(aes(xintercept=t95),linetype=2)
p <- p + scale_fill_gradientn(colours = (brewer.pal(n=9,name = 'GnBu')))
p <- p + theme_bw(base_size=12) + labs(x = "Normalized Withdrawals [gal/p/d]",fill="mean")
p <- p + facet_wrap(~year)
p <- p + theme(plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"))
p

## Density and type
p <- ggplot(d3.2) 
p <- p + geom_histogram(aes(x=wn, y=..density..,fill=type), bins=50, color="black")
#p <- p + geom_density(aes(x=wn, fill=type),alpha=0.4, adjust = 3/4, size=0.5, color = "black")
p <- p + scale_x_log10(breaks=c(1,10,100,1000,10000),labels=c(1,10,100,1000,10000)) 
p <- p + geom_vline(aes(xintercept=t5), linetype=2) + geom_vline(aes(xintercept=t95),linetype=2)
p <- p + scale_fill_brewer(palette="Blues")
p <- p + theme_bw(base_size=12) + labs(x = "Normalized Withdrawals [gal/p/d]",fill="type")
p <- p + facet_wrap(~year)
p <- p + theme(plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"))
p


# maps ----

counties <- readOGR("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\MSA\\county_shapefiles",
                    layer="cb_2013_us_county_500k");
counties2 <- spTransform(counties, CRS("+proj=longlat + datum=WGS84"));
county.fips <- fortify(counties2, region="GEOID");
colnames(county.fips)[6] <- "GEOID";
counties3 <- merge(county.fips,counties2@data,by="GEOID");
cnty.poly <- subset(counties3, long > -130 & long < 0 & lat > 20 & lat < 50);
colnames(cnty.poly)[1] <- "cntyFIPS";
rm(counties,counties2,county.fips,counties3) # remove old county files for space

state.poly <- map_data("state")

## source blank map theme
source("https://raw.githubusercontent.com/scworland-usgs/usgs-water-use/master/map_blank_theme.R")

### breaks for 2010 withdrawals
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
m = m + scale_fill_manual(values=(brewer.pal(8, 'YlGnBu')))
m = m + ggtitle("2010 Normalized Withdrawal")
m = m + labs(fill="gal/p/d") + theme_blank()
m

## Lower 48 raw
m2 = ggplot(data=cnty.WU) + coord_fixed(1.3)
m2 = m2 + geom_polygon(aes(long,lat, group=group,fill=brk2), color=NA)
m2 = m2 + geom_polygon(data=state.poly,aes(long,lat, group=group), color="grey", fill=NA, size=0.5)
m2 = m2 + scale_fill_manual(values=(brewer.pal(8, 'YlGnBu')))
m2 = m2 + ggtitle("2010 Withdrawal")
m2 = m2 + labs(fill="Mgal/day") + theme_blank()
m2

grid.arrange(m2,m,ncol=1)

## lower 48 norm high and low
wn.high$thold <- "> 300"
wn.high2 <- melt(wn.high)
wn.high2 <- subset(wn.high2, value > t95)
high.map <- merge(cnty.poly,wn.high2,by="cntyFIPS")
high.map <- high.map[order(high.map$order),]

wn.low$thold <- "< 15"
wn.low2 <- melt(wn.low)
wn.low2 <- subset(wn.low2, value <t5)
low.map <- merge(cnty.poly,wn.low2,by="cntyFIPS")
low.map <- low.map[order(low.map$order),]

pop.map <- merge(cnty.poly, pop.anom2,by="cntyFIPS")
pop.map <- pop.map[order(pop.map$order),]


m = ggplot() + coord_fixed(1.3)
m = m + geom_polygon(data=state.poly,aes(long,lat, group=group), color="black", fill=NA, size=0.5)
m = m + geom_polygon(data=high.map, aes(long,lat, group=group, fill = thold, color = thold))
m = m + geom_polygon(data=low.map, aes(long,lat, group=group, fill = thold, color = thold))
#m = m + geom_polygon(data=pop.map, aes(long,lat, group=group),color = "red",fill=NA)
m = m + scale_fill_manual(values = c("steelblue","orange3"))
m = m + scale_color_manual(values = c("steelblue","orange3")) + labs(fill="gal/p/day", color = "gal/p/day")
m = m + facet_wrap(~variable, ncol=2) + xlab(NULL) + ylab(NULL)
m = m + theme_blank() + theme(plot.margin=unit(c(0,1.5,0,1.5),"cm"),legend.position="bottom")
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
m = m + scale_fill_manual(values=rev(brewer.pal(6, 'YlGnBu')))
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
m = m + scale_fill_manual(values=rev(brewer.pal(6, 'YlGnBu')))
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
