#### ANALYSIS EXAMPLE: RANDOM SLOPES ACROSS TWO METROS ####
## Description: This file analyzes data over past year in San Diego, CA
##              and Myrtle Beach, SC and estimates trends
## Author: Michael Bader

source("_functions.R")
library(tidyverse) # Adds functions to improve data management & visualization
library(lubridate)

##### ANALYSIS ####
## GATHER DATA
load("../data/zillow_long.RData")

## DESCRIBE THE DATA
d.ana <- zillow.long %>%
    filter(grepl("San Diego|Myrtle Beach", RegionName)) %>%
    mutate(
        city = factor(if_else(grepl("San Diego", RegionName), "sd", "mb")),
        date = ymd(paste0(yyyymm, "01"))
    ) %>%
    filter(date < "2019-04-01")
    
g.base <- ggplot(d.ana, aes(x=date, y=lnvalue_t, color=city)) +
    geom_point(size=1) +
    labs(
        y="Logged home value/sq. ft.",
        x="Month",
        color="Metro"
    )
g.base

## ANALYZE THE DATA
d.ana <- d.ana %>%
    mutate(
        monthc = month - min(month)
    )
m.ana <- lm(lnvalue_t ~ monthc, data=d.ana) 
summary(m.ana)
m.ana.coef <- m.ana$coefficients

## Plot the residuals by metro
d.ana$lnvalue_t_hat <- predict(m.ana)
d.ana$e_t <- d.ana$lnvalue_t - d.ana$lnvalue_t_hat
ggplot(d.ana, aes(x=e_t)) +
    geom_histogram(bins = 200) +
    facet_grid(rows = vars(city))

## Estimate individual models for each metro
m.mb <- lm(lnvalue_t ~ monthc, data=d.ana[d.ana$city=="mb",])
m.sd <- lm(lnvalue_t ~ monthc, data=d.ana[d.ana$city=="sd",])
huxreg(m.mb, m.sd)

g.fit <- g.base +
    geom_smooth(method="lm", se=FALSE, size=.5, alpha=.75) +
    geom_smooth(aes(col=NULL), method="lm", se=FALSE)
g.fit
