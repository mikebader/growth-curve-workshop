#### ANALYSIS EXAMPLE: LINEAR TREND ACROSS TWO METROS ####
## Description: This file analyzes data over past year in NYC and
##              Philadelphia metros and estimates that trend
## Author: Michael Bader

source("_functions.R")
library(tidyverse) # Adds functions to improve data management & visualization
library(lubridate) # Adds functions to manage time variables
library(huxtable)  # To present pretty regression tables

##### ANALYSIS ####
## GATHER DATA
load("../data/zillow_long.RData")

## DESCRIBE THE DATA
d.ana <- zillow.long %>%
    filter(grepl("New York|Washington", RegionName)) %>%
    mutate(
        city = factor(if_else(grepl("New York", RegionName), "nyc", "was")),
        date = ymd(paste0(yyyymm, "01"))
    )
g.base <- ggplot(d.ana, aes(x=date, y=lnvalue_t, color=city)) +
    geom_point(size=1) +
    labs(
        y="Logged home value/sq. ft.",
        x="Month",
        color="Metro"
    )
g.base

## ANALYZE THE DATA
m.ana <- lm(lnvalue_t ~ month,data=d.ana)
summary(m.ana)


## Notice that the intercept does not fall anywhere near the observed data. 
## The data have time starting in March 1996 
## Let's rescale the variable so that zero is at the start of our period,
## then re-analyze the data
d.ana <- d.ana %>%
    mutate(
        monthc = month - min(month)
    )
m.ana <- lm(lnvalue_t ~ monthc, data=d.ana) ## Notice small change here
summary(m.ana)
m.ana.coef <- m.ana$coefficients

## Plot the residuals by metro
d.ana$lnvalue_t_hat <- predict(m.ana)
d.ana$e_t <- d.ana$lnvalue_t - d.ana$lnvalue_t_hat
d.ana[,c("city","e_t")]
ggplot(d.ana, aes(x=e_t)) +
    geom_histogram(bins = 200) +
    facet_grid(rows = vars(city))

## Estimate individual models for each metro
m.nyc <- lm(lnvalue_t ~ monthc, data=d.ana[d.ana$city=="nyc",])
m.was <- lm(lnvalue_t ~ monthc, data=d.ana[d.ana$city=="was",])
huxreg(m.nyc, m.was)

g.fit <- g.base +
    geom_smooth(method="lm", se=FALSE, size=.5, alpha=.75) +
    geom_smooth(aes(col=NULL), method="lm", se=FALSE)
g.fit
