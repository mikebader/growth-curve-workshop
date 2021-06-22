#### ANALYSIS EXAMPLE: LINEAR TREND ####
## Description: This file estimates a single linear trend of NYC metro housing
##              values over past 12 months
## Author: Michael Bader

rm(list=ls())
source("_functions.R")
library(ggplot2)
library(tidyverse)
current_year <- 2020

## GATHER DATA
## Relabel variables to indicate that they represent housing values
load("../data/zillow.RData")
X.idx <- grep("X",names(zillow))
names(zillow)[X.idx]<-sub("X","val.",sub("\\.","",names(zillow)[X.idx]))

## Create list of variables to keep data from March of two years ago
## to March of current year
keep_vars <- c(1:2,
    grep(paste0("val.", current_year-2, "03"), names(zillow)):
    grep(paste0("val.", current_year, "03"), names(zillow))        
)

nyc <- zillow[zillow$RegionName=="New York, NY", keep_vars]
nyc

## Uh oh! We need to make the data "long" in order to analyze it
nyc.long <- nyc %>%
    pivot_longer(
        cols         = starts_with('val'),
        names_to     = "yyyymm",
        names_prefix = "val\\.",
        values_to    = "value_t"
    ) 

nyc.long <- nyc.long %>%
    mutate(
        month = 0:(n() - 1),
        y_t = log(value_t)
        
    )

## DESCRIBE DATA
time_plt <- ggplot(nyc.long, aes(x=month,y=y_t)) +
    geom_point() +
    scale_x_continuous(breaks=seq(0,24,1),labels=rep(month.abb,3)[4:28])
time_plt

## ANALYZE DATA
m.nyc <- lm(y_t ~ month,data=nyc.long)
summary(m.nyc)

nyc.long <- nyc.long %>%
    mutate(
        yhat_t = predict(m.nyc),
        e_t = y_t - yhat_t
    )

## INTERPRET DATA
(
    pred_plt <- time_plt + 
        geom_line(data=nyc.long, aes(y=yhat_t), color="orange")
)
sapply(list(mean=mean(nyc.long$e_t),sd=sd(nyc.long$e_t)),round,5)

