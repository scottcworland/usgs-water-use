
# Load libraries and set the workspace ----

libs <- c("ggplot2", "ggmap","maps", "acs","rgdal", "rgeos", "zoo","xts", "plyr",
          "RColorBrewer","maptools","reshape2","gridExtra","abind","grid");
lapply(libs, library, character.only=T);

setwd("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\USGSwaterUse\\usgs-water-use")

# File preparation, skip to line 45 to load dataframes ----

# load data for each year. The combined file is a .rda and can
# be loaded in line 36
d85 = read.csv("County_PS_1985.csv", header=T)
d90 = read.csv("County_PS_1990.csv", header=T)
d95 = read.csv("County_PS_1995.csv", header=T)
d00 = read.csv("County_PS_2000.csv", header=T)
d05 = read.csv("County_PS_2005.csv", header=T)
d10 = read.csv("County_PS_2010.csv", header=T)

# load county and MSA data (GEOID=MSA fips)
MSA_cnty_Region=read.csv("MSA_cnty_Region.csv",header=T)
MSA_cnty_Region$cntyFIPS <- sprintf("%05d", MSA_cnty_Region$cntyFIPS)
save(MSA_cnty_Region,file="MSA_cnty_ST_FIPS_REGION.rda")

load("MSA_cnty_ST_FIPS_REGION.rda") # load cnty_regions
load("MSA_ST_FIPS_REGION.rda") # load MSA_regions

# merge time series by cntyFIPS codes
D = merge(d85,d90,"cntyFIPS")
D = merge(D,d95,"cntyFIPS")
D = merge(D,d00,"cntyFIPS")
D = merge(D,d05,"cntyFIPS")
D = merge(D,d10,"cntyFIPS")
D$cntyFIPS = sprintf("%05d",D$cntyFIPS)

WUcnty = D
save(WUcnty,file="USGSWU1985_2010cnty.rda")

# Aggregate into MSA
load("USGSWU1985_2010cnty.rda")
D2 = merge(MSA_cnty_Region,WUcnty,"cntyFIPS") #check
D3 = aggregate(D2[,6:35],list(D2$GEOID,D2$State),sum) #check
colnames(D3)[1:2] = c("GEOID","State") #check
D4 = merge(MSA_Region[,2:4],D3,"GEOID") #check

WUMSAcnty = D2
WUMSA = D4
save(WUMSAcnty,file="USGSWU1985_2010MSAcnty.rda")
save(WUMSA,file="USGSWU1985_2010MSA.rda")

# load dataframes ----
load("USGSWU1985_2010cnty.rda")
load("USGSWU1985_2010MSA.rda")
load("USGSWU1985_2010MSAcnty.rda")

# set 0 to NA for domestic deliveries in 2000
WUMSA$DOPSDel2000=NA

# subsample the MSA data ----
pop = WUMSA[,c(5,10,15,20,25,30)]
WGW = WUMSA[,c(6,11,16,21,26,31)]
WSW = WUMSA[,c(7,12,17,22,27,32)]
Wtot = WUMSA[,c(8,13,18,23,28,33)]
domestic = WUMSA[,c(9,14,19,24,29,34)] 

# normalize to population
WGWn = WGW/pop * 1000
WSWn = WSW/pop * 1000
Wtotn = Wtot/pop * 1000
Wtotn[Wtotn>1000] = NA
Wn = Wtotn

colnames(Wn) = c("Wn1985","Wn1990","Wn1995","Wn2000","Wn2005","Wn2010")

# Basic exploratory plots ----
Sums = data.frame(colSums(Wtot))
colnames(Sums) = "Total"
Sums$domestic = colSums(domestic)
Sums$year = c("1985","1990","1995","2000","2005","2010")

p = ggplot(data=melt(Sums, id = "year"),aes(x=year,y=value,fill = variable)) 
p = p + geom_bar(position = "dodge", stat="identity", width=.5,color="black",size=1)
p = p + ylab("b/gal/day") + xlab("") + theme_bw(base_size=20)
p = p + scale_fill_manual(values=c(Total="grey55",domestic="lightblue"),name = "Type")
p = p + ggtitle("Public Supply Withdrawals")
p

PStotn = cbind(WUMSA$GEOID,Wn)
colnames(PStotn) = c("GEOID","1985","1990","1995","2000","2005","2010")

## boxplot
p = ggplot(melt(PStotn, id="GEOID")) 
p = p + geom_boxplot(aes(variable,value), fill = "dodgerblue")
p = p + theme_bw(base_size=20)
p = p + xlab("") + ylab("gal/p/day")
p = p + ggtitle("Normalized Fresh Water PS Withdrawals")
p

# percent domestic deliveries ----
PerDel = domestic/Wtot * 100
PerDel[PerDel>100] = 100

# Add GEOID and rename and plot
PerDel = cbind(WUMSA$GEOID,PerDel)
colnames(PerDel) = c("GEOID","Y1985","Y1990","Y1995","Y2000","Y2005","Y2010")

## boxplot
p = ggplot(melt(PerDel, id="GEOID")) 
p = p + geom_boxplot(aes(variable,value), fill = "dodgerblue")
p = p + theme_bw(base_size=20)
p = p + xlab("") + ylab("% domestic delivery")
p

# percent change in water use ----
dw=numeric() # % change in WU through time (% diff)
for (i in 1:5){
h = (Wtot[,i+1]/Wtot[,i] * 100)-100 
dw = cbind(dw,h)
}

# Clean up dw above
colnames(dw) = c("dw1990","dw1995","dw2000","dw2005","dw2010")
dw = cbind(WUMSA[,c(1:4)], dw)
dw2 = subset(dw, dw1990 < 100 & dw1995 < 100 & dw2000 < 100 & 
               dw2005 < 100 & dw2010 < 100) # remove MSAs w high values

# multidimensional array ----

x1 = as.matrix(WGW[,1:6])
x2 = as.matrix(WSW[,1:6])
x3 = as.matrix(Wtot[,1:6])
x4 = as.matrix(pop[,1:6])

X1 = array(x1,dim=c(374,1,6))
X2 = array(x2,dim=c(374,1,6))
X3 = array(x3,dim=c(374,1,6))
X4 = array(x4,dim=c(374,1,6))

## create dimension names
type = c("ground","surface","total","pop")
Y = c("Y1985","Y1990","Y1995","Y2000","Y2005","Y2010")

## create the array and save
dat = abind(X1,X2,X3,X4,along=2, new.names=list(NULL,type,Y))
save(dat, file="TSarray.rda")

# change column names to year and add a type column
years = c("1985","1990","1995","2000","2005","2010")
colnames(pop) = years; 
colnames(WGW) = years; WGW$type = "ground"
colnames(WSW) = years; WSW$type = "surface"
#colnames(Wtot) = years; Wtot$type = "total"

# I am not sure what this all is for ----
WGW1 = melt(cbind(WUMSA[,c(1,4)],WGW), id = c("GEOID","Region","type"))
WSW1 = melt(cbind(WUMSA[,c(1,4)],WSW), id = c("GEOID","Region","type"))
NW_type = rbind(WGW1,WSW1)
colnames(NW_type)[4:5] = c("year", "withdrawal")

# Get ready to plot
pop2 = cbind(WUMSA[1],pop)
pop3 = melt(pop2, id.vars = "GEOID")
colnames(pop3)[2:3] = c("year","pop")

wthdrwl2 = cbind(WUMSA[1],wthdrwl)
Wthdrwl3 = melt(wthdrwl2, id.vars = "GEOID")
colnames(Wthdrwl3)[2:3] = c("year","withdrawal")

# Multilevel model ----
library(lme4); library(arm); library(coefplot2)
#install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R")

## Prepare data for null model
d = data.frame(WUMSA$State)
d$GEOID = WUMSA$GEOID
d$Wn=Wn$Wn2010
d = na.omit(d) #kingston NY has value > 1000
colnames(d) = c("State","MSA","Wn")

# completely pooled
av = mean(d$Wn)

## unpooled for states
m1 = lmer(Wn ~ 1 + (1|State),data=d)
cf = coef(m1)$State
se = se.coef(m1)
x = cbind(cf,se$State)
colnames(x) = c("alpha","se")
x$State = rownames(x)
rownames(x) = NULL

p = ggplot(x, aes(x=State, y=alpha)) 
p = p + geom_point(size = 2) + xlab("")
p = p + geom_errorbar(aes(ymin=alpha-se, ymax=alpha+se), width=.1) 
p = p + geom_hline(aes(yintercept=av), linetype=2)
p = p + theme_bw(base_size=20) + coord_flip()
p = p + ggtitle("Null model for 2010 Wn")
p = p + ylab("alpha (gal/p/day)")
p

# Plot.ly plot ----
install.packages("devtools")
library("devtools")
install_github("ropensci/plotly")
library(plotly)

py = plotly(user="scottcworland", key="7tbehcvpcd")
response = py$ggplotly()

# regional plot----

# boxplot of normalized water use
WnR = cbind(WUMSA[,3],Wn)
colnames(WnR)[1] = "Region"
WnR2 = melt(WnR, id="Region")
colnames(WnR2)[2:3] = c("year","Wn")

p = ggplot(WnR2)  
p = p + geom_boxplot(aes(Region,Wn,fill=Region,color=year), alpha=0.8)
p = p + scale_fill_brewer(palette="Dark2")
p = p + scale_color_manual(values=c("black","black","black","black","black","black","black"))
p = p + xlab("Years (1985-2010)") + ylab("Wn (gal/p/day)") + guides(color=F) + guides(fill=F)
p = p + ggtitle("Normalized Public Supply withdrawal for 315 MSAs") + theme_bw(base_size = 20)
p
  

# Regional map
all_states = map_data("state")
regions = read.delim("Regions.txt", header=T)
load("MSA_ST_FIPS_REGION.rda")
colnames(all_states)[5] = "STATE_FULL"
all_states = merge(all_states, regions, "STATE_FULL")
all_states = all_states[order(all_states$order),];

# find approximate centroids and of regions and number of MSAs
cen = aggregate(all_states[,2:3], list(all_states$Region), mean)
cen$N = data.frame(table(MSA_Region$Region))[c(1:4,6:8),2]
colnames(cen)[1] = "Region"

# Map of regions
m = ggplot() + coord_fixed(1.3)
m = m + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill = Region),
                      color="black",size=0.5 );
m = m + scale_fill_brewer(palette="Dark2") + guides(fill=F)
m = m + geom_text(data=cen, aes(long, lat, label=N), color= "black", size=10, fontface="bold")
m = m + geom_text(aes(-115,27, label="# = Number of MSAs in Region"), size=8)
m = m + theme(panel.background = element_rect(fill = "transparent",color=NA),
                  plot.background = element_rect(fill = "transparent",color=NA),
                  axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  plot.title = element_text(lineheight=1, face="bold"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.margin=unit(c(0,0,-1,-1),"lines"))


grid.arrange(p,m,ncol=1)

# Percent water use map ----

## Import MSA shapefiles
CBSA <- readOGR("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\MSA\\MSA",
                layer="cb_2013_us_cbsa_500k");
CBSA2 <- spTransform(CBSA, CRS("+proj=longlat + datum=WGS84"));
MSA <- CBSA2[CBSA2@data$LSAD == "M1",];
MSA.NAME <- fortify(CBSA2, region="NAME");
colnames(MSA.NAME)[7] <- "NAME";
MSA.NAME <- merge(MSA.NAME,CBSA2@data,by="NAME");
MSA_all <- subset(MSA.NAME, LSAD =="M1" & long > -130 & lat > 20)
MSA_del <- merge(MSA_all,PerDel,by="GEOID",all.x=F);
MSA_del <- MSA_del[order(MSA_del$order),];

## percent diff from 1985, 1990-2010
m90 = ggplot()
m90 = m90 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),
                       color="black", fill="grey75",size=0.5 );
m90 = m90 + geom_polygon(data=MSA_del,aes(x = long,y = lat,group = group,fill=perdel),
                       alpha = 1, color="grey50");
m90 = m90 + scale_fill_gradientn(colours = rev(brewer.pal(n=9,name = 'PRGn')))
m90 = m90 + labs(fill="%PSDOdel")
m90 = m90 + theme(panel.background = element_rect(fill = "transparent",color=NA),
                plot.background = element_rect(fill = "transparent",color=NA),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                plot.title = element_text(lineheight=1, face="bold"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin=unit(c(0,0,-1,-1),"lines"))


m95 = ggplot()
m95 = m95 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),
                       color="black", fill="grey75",size=0.5 );
m95 = m95 + geom_polygon(data=MSA_dw,aes(x = long,y = lat,group = group,fill=dw1995),
                       alpha = 1, color="grey50");
m95 = m95 + scale_fill_gradientn(colours = rev(brewer.pal(n=9,name = 'PRGn')))
m95 = m95 + labs(fill="% diff 90-95")
m95 = m95 + theme(panel.background = element_rect(fill = "transparent",color=NA),
                plot.background = element_rect(fill = "transparent",color=NA),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                plot.title = element_text(lineheight=1, face="bold"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin=unit(c(0,0,-1,-1),"lines"))


m00 = ggplot()
m00 = m00 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),
                       color="black", fill="grey75",size=0.5 );
m00 = m00 + geom_polygon(data=MSA_dw,aes(x = long,y = lat,group = group,fill=dw2000),
                       alpha = 1, color="grey50");
m00 = m00 + scale_fill_gradientn(colours = rev(brewer.pal(n=9,name = 'PRGn')))
m00 = m00 + labs(fill="% diff 95-00")
m00 = m00 + theme(panel.background = element_rect(fill = "transparent",color=NA),
                plot.background = element_rect(fill = "transparent",color=NA),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                plot.title = element_text(lineheight=1, face="bold"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin=unit(c(0,0,-1,-1),"lines"))


m05 = ggplot()
m05 = m05 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),
                        color="black", fill="grey75",size=0.5 );
m05 = m05 + geom_polygon(data=MSA_dw,aes(x = long,y = lat,group = group,fill=dw2005),
                        alpha = 1, color="grey50");
m05 = m05 + scale_fill_gradientn(colours = rev(brewer.pal(n=9,name = 'PRGn')))
m05 = m05 + labs(fill="% diff 00-05")
m05 = m05 + theme(panel.background = element_rect(fill = "transparent",color=NA),
              plot.background = element_rect(fill = "transparent",color=NA),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(lineheight=1, face="bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin=unit(c(0,0,-1,-1),"lines"))


m10 = ggplot()
m10 = m10 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),
                       color="black", fill="grey75",size=0.5 );
m10 = m10 + geom_polygon(data=MSA_dw,aes(x = long,y = lat,group = group,fill=dw2010),
                       alpha = 1, color="grey50");
m10 = m10 + scale_fill_gradientn(colours = rev(brewer.pal(n=9,name = 'PRGn')))
m10 = m10 + labs(fill="% diff 05-10")
m10 = m10 + theme(panel.background = element_rect(fill = "transparent",color=NA),
                plot.background = element_rect(fill = "transparent",color=NA),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                plot.title = element_text(lineheight=1, face="bold"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin=unit(c(0,0,-1,-1),"lines"))

grid.arrange(m90,m95,m00,m05,m10,ncol=3)

# county water use map ----
require(grid)
counties <- readOGR("C:\\Users\\scworlan\\Documents\\Water Conservation\\R_conservation\\MSA\\county_shapefiles",
                    layer="cb_2013_us_county_500k");
counties2 <- spTransform(counties, CRS("+proj=longlat + datum=WGS84"));
county.fips <- fortify(counties2, region="GEOID");
colnames(county.fips)[7] <- "GEOID";
counties3 <- merge(county.fips,counties2@data,by="GEOID");
cnty.poly <- subset(counties3, long > -130 & long < 0 & lat > 20 & lat <50);
colnames(cnty.poly)[1] <- "cntyFIPS";
rm(counties,counties2,county.fips,counties3) # remove old county files for space

state.data <- map_data("state")

## Just counties within MSAs
normWU2010 = data.frame(WUMSAcnty$PSWFrTo2010/WUMSAcnty$POP2010*1000)
normWU2010[normWU2010>1000] = 1000
colnames(normWU2010) = "norm_WU"
normWU2010$cntyFIPS = WUMSAcnty$cntyFIPS

cnty.WU <- merge(cnty.poly,normWU2010,by="cntyFIPS")
cnty.WU <- cnty.WU[order(cnty.WU$order),]

m5 = ggplot(data=cnty.WU) + coord_fixed(1.3)
m5 = m5 + geom_polygon(data=state.data,aes(long,lat, group=group), color="white", fill="grey",size=0.5)
m5 = m5 + geom_polygon(aes(long,lat, group=group, fill=norm_WU), color=NA)
m5 = m5 + geom_polygon(data=MSA_all,aes(long,lat, group=group),fill=NA,color="grey33",size=0.5)
m5 = m5 + scale_fill_gradientn(colours = (brewer.pal(n=9,name = 'YlGnBu')))
m5 = m5 + ggtitle("2010 Fresh Water Puplic Supply Withdrawal/2010 County Pop for MSAs")
m5 = m5 + labs(fill="gal/p/d")
m5 = m5 + theme(panel.background = element_rect(fill = "transparent",color=NA),
                plot.background = element_rect(fill = "transparent",color=NA),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 20),
                plot.title = element_text(lineheight=1, size=20, face="bold"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin=unit(c(0,0,-1,-1),"lines"))
m5

## every county in lower 48
norm2WU2010 = data.frame(WUcnty$PSWFrTo2010/WUcnty$POP2010*1000)
norm2WU2010[norm2WU2010>1000] = 1000
colnames(norm2WU2010) = "norm_WU"
norm2WU2010$cntyFIPS = WUcnty$cntyFIPS

cnty.WU2 <- merge(cnty.poly,norm2WU2010,by="cntyFIPS")
cnty.WU2 <- cnty.WU2[order(cnty.WU2$order),]

m6 = ggplot(data=cnty.WU2) + coord_fixed(1.3)
m6 = m6 + geom_polygon(aes(long,lat, group=group, fill=norm_WU), color=NA)
m6 = m6 + geom_polygon(data=state.data,aes(long,lat, group=group), color="grey33", fill=NA,size=0.5)
#m6 = m6 + geom_polygon(data=MSA_all,aes(long,lat, group=group),fill=NA,color="grey33",size=0.5)
#m6 = m6 + theme_bw(base_size=20) 
m6 = m6 + scale_fill_gradientn(colours = (brewer.pal(n=9,name = 'YlGnBu')))
m6 = m6 + ggtitle("2010 Fresh Water Puplic Supply Withdrawal/2010 County Pop")
m6 = m6 + labs(fill="gal/p/d")
m6 = m6 + theme(panel.background = element_rect(fill = "transparent",color=NA),
                plot.background = element_rect(fill = "transparent",color=NA),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                legend.text = element_text(size = 16),
                legend.title = element_text(size = 20),
                plot.title = element_text(lineheight=1, size=20, face="bold"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin=unit(c(0,0,-1,-1),"lines"))
m6