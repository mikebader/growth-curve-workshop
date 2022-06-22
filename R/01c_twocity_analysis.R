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
## Notice below that we rescale the months in the datec variable so that the 
## first month is indexed at 0.
d.ana <- zillow.long %>%
    filter(grepl("New York|Washington", RegionName)) %>%
    mutate(
        city = factor(if_else(grepl("New York", RegionName), "nyc", "was")),
        date = ymd(paste0(yyyymm, "01")),
        datec = interval(min(date), date) %/% months(1) ## Rescale months
    )
g.base <- ggplot(d.ana, aes(x=date, y=lnvalue_t, color=city)) +
    geom_point(size=1) +
    scale_y_continuous(limits = c(6.15, 6.675)) +
    labs(
        y="Logged home value/sq. ft.",
        x="Month",
        color="Metro"
    )
g.base

## ANALYZE THE DATA
m.ana <- lm(lnvalue_t ~ datec, data=d.ana)
summary(m.ana)
m.ana.coef <- m.ana$coefficients

## Plot the residuals by metro
d.ana$lnvalue_t_hat <- predict(m.ana)
d.ana$e_t <- d.ana$lnvalue_t - d.ana$lnvalue_t_hat
d.ana[,c("city","e_t")]
ggplot(d.ana, aes(x=e_t, fill = city)) +
    geom_histogram(bins = 200) +
    geom_vline(xintercept = 0, size = 1, color = "#cccccc") +
    scale_y_continuous(breaks = seq(0, 10, 2)) 

## Estimate individual models for each metro
m.nyc <- lm(lnvalue_t ~ datec, data=d.ana[d.ana$city=="nyc",])
m.was <- lm(lnvalue_t ~ datec, data=d.ana[d.ana$city=="was",])
huxreg(list(NYC = m.nyc, WAS = m.was), statistics = c(N = "nobs"))

g.fit <- g.base +
    geom_smooth(method="lm", se=FALSE, size=.5, alpha=.75) +
    geom_smooth(aes(col=NULL), method="lm", se=FALSE)
g.fit
