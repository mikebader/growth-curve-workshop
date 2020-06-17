#### ANALYSIS EXAMPLE: BAYESIAN RANDOM INTERCEPTS & SLOPES ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              and random slopes for 150 largest metros using a fully Bayesian
##              method.
##              Based on example code by Julian Faraway:
##              http://www.maths.bath.ac.uk/~jjf23/stan/longitudinal.html
## Author: Michael Bader

library(ggplot2)
library(reshape2)
library(rstan)

## Set up the data
lmod <- lm(lnvalue_ti ~ month, data=zillow.long)
x <- model.matrix(lmod)
zillowdat <- list(Nobs=nrow(zillow.long),
                  Npreds = ncol(x),
                  Ngroups = length(unique(zillow.long$RegionID)),
                  y = zillow.long$lnvalue_ti,
                  x = x,
                  timevar = zillow.long$month,
                  group = rep(1:150, each=13)
            )

## Model
## (in Stan code)
longitudinal_stancode <- "
data {
     int<lower=0> Nobs;
     int<lower=0> Npreds;
     int<lower=0> Ngroups;
     vector[Nobs] y;
     matrix[Nobs,Npreds] x;
     vector[Nobs] timevar;
     int group[Nobs];
}
parameters {
     vector[Npreds] beta;
     real<lower=0> sigmaint;
     real<lower=0> sigmaslope;
     real<lower=0> sigmaeps;

     vector[Ngroups] etaint;
     vector[Ngroups] etaslope;
}
transformed parameters {
     vector[Ngroups] ranint;
     vector[Ngroups] ranslope;
     vector[Nobs] yhat;

     ranint  = sigmaint * etaint;
     ranslope = sigmaslope * etaslope;

     for (i in 1:Nobs)
          yhat[i] = x[i]*beta+ranint[group[i]]+ranslope[group[i]]*timevar[i];

     }
model {
     etaint ~ normal(0, 1);
     etaslope ~ normal(0, 1);
     y ~ normal(yhat, sigmaeps);
}
"

## Fit the model
## Note: this takes a while to run
rstan_options(auto_write = TRUE)
options(mc.cores=parallel::detectCores())
sm <- stan_model(model_code=longitudinal_stancode, verbose = FALSE)
system.time(fit <- sampling(sm, data=zillowdat, thin=10))

## Check the parameter diagnostics
pname <- "sigmaeps"
muc <- rstan::extract(fit, pars=pname,  permuted=FALSE, inc_warmup=FALSE)
mdf <- melt(muc)
ggplot(mdf,aes(x=iterations,y=value,color=chains)) + geom_line() + ylab(mdf$parameters[1])

pname <- "sigmaint"
muc <- rstan::extract(fit, pars=pname,  permuted=FALSE, inc_warmup=FALSE)
mdf <- melt(muc)
ggplot(mdf,aes(x=iterations,y=value,color=chains)) + geom_line() + ylab(mdf$parameters[1])

pname <- "sigmaslope"
muc <- rstan::extract(fit, pars=pname,  permuted=FALSE, inc_warmup=FALSE)
mdf <- melt(muc)
ggplot(mdf,aes(x=iterations,y=value,color=chains)) + geom_line() + ylab(mdf$parameters[1])

## Main parameters
print(fit,par=c("beta","sigmaint","sigmaslope","sigmaeps"))

## Fixed parameters
ref <- melt(rstan::extract(fit, pars="beta"))
colnames(ref)[2:3] <- c("parameter","lnvalue_ti")
ref$parameter <- factor(colnames(x)[ref$parameter])
ggplot(ref, aes(x=lnvalue_ti))+geom_density()+geom_vline(xintercept=0)+facet_wrap(~parameter,scales="free")

## Random parameters
postsig <- rstan::extract(fit, pars=c("sigmaint","sigmaslope","sigmaeps"))
ref <- melt(postsig)
colnames(ref)[2:3] <- c("logincome","parameter")
ggplot(data=ref,aes(x=logincome))+geom_density()+facet_wrap(~parameter,scales="free")
