
## load original data
load("USGSWU1985_2010cnty.rda")
d <- WUcnty

## subsample
pop = d[,c(1,2,7,12,17,22,27)]
ground = d[,c(1,3,8,13,18,23,28)]
surface = d[,c(1,4,9,14,19,24,29)]
total = d[,c(1,5,10,15,20,25,30)]

## rename for simpler indexing
names = c("cntyFIPS","1985","1990","1995","2000","2005","2010")
colnames(pop) = names
colnames(ground) = names
colnames(surface) = names
colnames(total) = names

pop2 = melt(pop, variable.name = "year", value.name = "pop")
ground2 = melt(ground, variable.name = "year", value.name = "ground")
surface2 = melt(surface, variable.name = "year", value.name = "surface")
total2 = melt(total, variable.name = "year", value.name = "total")

d2 = cbind(pop2,ground2[,3],surface2[,3],total2[,3])
colnames(d2)[4:6] = c("ground","surface","total")
d3 = melt(d2, id.vars = c("cntyFIPS", "year","pop"),variable.name = "type")
d3$wn = d3$value/d3$pop * 1000

ggplot(d3) + geom_boxplot(aes(type, wn , fill = year)) 

## normalize by population

summary = summary(ground)





## Smooth scatter ggplot
library(ggplot2)
Y85$col = densCols(log10(Y85$p),log10(Y85$t), colramp = colorRampPalette(rev(rainbow(10, end = 4/6))))

p <- ggplot(Y85) + geom_point(aes(log10(p), log10(t), col = col), size = 2) +
  scale_color_identity() + theme_bw(base_size=15) + ggtitle("1985") +
  xlab("log10(poulation)") + ylab("log10(total withdrawal)")
p