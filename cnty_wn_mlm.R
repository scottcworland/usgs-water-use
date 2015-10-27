
# Libraries, working directory, and data ----
## libraries
library(lme4)
library(arm) 
library(ggplot2)
library(rjags)
library(rstan)
library(ggmcmc)

## working directory
setwd(getwd())

## Load the data
cnty.data = read.csv("Wn_mlm_bayesian_prelim_data.csv")

## add leading zero to FIPs that need it
cnty.data$cntyFIPS <- sprintf("%05d", cnty.data$cntyFIPS)

# lmer ----
# completely pooled
av = mean(cnty.data$Wn)

## unpooled for states
m1 = lmer(y.Wn.cnty ~ 1 + (1|State),data=cnty.data)
cf = coef(m1)$State
se = se.coef(m1)
x = cbind(cf,se$State)
colnames(x) = c("alpha","se")
x$State = rownames(x)
rownames(x) = NULL


# MLM model in Jags ----

## prepare a list of data to pass to JAGS model
n = nrow(cnty.data)
n.states = length(unique(cnty.data$State))

Wn.data = list(n = n, 
               n.states = n.states, 
               y = cnty.data$y.Wn.cnty, 
               State = cnty.data$State)

## We need to initialize the parameters for the model.
## Initialize alpha_j and mu randomly from normal(0,1) distribution
## Initialize sigma randomly from a uniform distribution on [0,1]
Wn.inits = function(chain){
  list (a=rnorm(n.states), mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}

# Tell JAGS the names of the parameters it should report back to us
Wn.parameters = c("a", "mu.a", "sigma.y", "sigma.a")

# Compile the JAGS model, initializing the data and parameters
mlm.Wn.nopred.model = jags.model("Wn.multilevel.nopred.jags",
                                 data = Wn.data,
                                 inits = Wn.inits,
                                 n.chains = 3,
                                 n.adapt = 1000)

# After warming up, take 2000 random samples.
update(mlm.Wn.nopred.model, n.iter=2000) #burn-in

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
p = p + geom_errorbar(aes(x=State, y=alpha2, ymin=alpha2-sd, ymax=alpha2+sd), 
                    width=.1, color ="red") 
p = p + geom_point(aes(x=State, y=alpha), size = 3) + xlab("")
p = p + geom_errorbar(aes(x=State, y=alpha, ymin=alpha-se, ymax=alpha+se), 
                      width=.1, alpha=1) 
p = p + geom_hline(aes(yintercept=av), linetype=2)#
p = p + theme_bw(base_size=20) + coord_flip()
p = p + ggtitle("Null model for 2010 Wn")
p = p + ylab("alpha (gal/p/day)")
p











