
library(lme4)
library(arm) 
library(ggplot2)
library(rjags)
library(ggmcmc)

setwd(getwd())

# Load data and prepare for models ----
load("MSA_Wn.rda")
load("USGSWU1985_2010MSA.rda")

data = read.csv("Wn_mlm_bayesian_prelim_data.csv")
MSA_cnty_Region$cntyFIPS <- sprintf("%05d", MSA_cnty_Region$cntyFIPS)

## Prepare data for 2010 null model
d = data.frame(WUMSA$State)
d$GEOID = WUMSA$GEOID
d$Wn=Wn$Wn2010
d = na.omit(d) #kingston NY has value > 1000
colnames(d) = c("State","MSA","Wn")
rownames(d) = NULL # d = MSA level wn values


# lmer ----
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


# Jags ----
# We need to initialize the parameters for the model.
# Initialize alpha_j and mu randomly from normal(0,1) distribution
# Initialize sigma randomly from a uniform distribution on [0,1]
n = length(d$Wn)
n.states = length(unique(d$State))

Wn.inits = function(chain){
  list (a=rnorm(n.states), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

# prepare a list of data to pass to JAGS model
Wn.data = list(n = n, n.states = n.states, y = d$Wn, State = d$State)

# Tell JAGS the names of the parameters it should report back to us
Wn.parameters = c("a", "mu.a", "sigma.y", "sigma.a")

# Compile the JAGS model, initializing the data and parameters
mlm.Wn.nopred.model = jags.model("Wn.multilevel.nopred.jags",
      data = Wn.data,
      inits = Wn.inits,
      n.chains = 3,
      n.adapt = 1000)

# After warming up, take 2000 random samples.
update(mlm.Wn.nopred.model,n.iter=2000) #burn-in

mlm.Wn.nopred = coda.samples(mlm.Wn.nopred.model,
         variable.names = Wn.parameters,
         n.iter = 2000)

# Here, we get the data back from JAGS and convert it to a useful form
post.nopred = as.matrix(mlm.Wn.nopred)
mean.a.nopred = rep(NA, n.states)
sd.a.nopred = rep(NA, n.states)

for (i in 1:n.states) {
  mean.a.nopred[i] = mean(post.nopred[ , paste('a[',i,']', sep='')])
  sd.a.nopred[i] = sd(post.nopred[ , paste('a[',i,']', sep='')])
}

x$alpha2 = mean.a.nopred
x$sd = sd.a.nopred

# Plot models
p = ggplot(data=x) 
p = p + geom_point(aes(x=State, y=alpha2), size = 3, color = "red", shape = 7)
#p = p + geom_errorbar(aes(x=State, y=alpha2, ymin=alpha2-sd, ymax=alpha2+sd), 
#                     width=.1, color ="red") 
p = p + geom_point(aes(x=State, y=alpha), size = 3) + xlab("")
p = p + geom_errorbar(aes(x=State, y=alpha, ymin=alpha-se, ymax=alpha+se), 
                      width=.1, alpha=1) 
p = p + geom_hline(aes(yintercept=av), linetype=2)#
p = p + theme_bw(base_size=20) + coord_flip()
p = p + ggtitle("Null model for 2010 Wn")
p = p + ylab("alpha (gal/p/day)")
p
