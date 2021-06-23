#### ANALYSIS EXAMPLE: TIME-INVARIANT PREDICTORS ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              and random slopes for 150 largest metros with logged 
##              population used to refine estimates of intercepts and slopes
## Author: Michael Bader

source('_functions.R')
library(tidyverse)
library(lme4)

## Load Zillow and metro-level data and merge to Census
load("../data/metros.Rdata")
load("../data/zillow_long.Rdata")
zillow_acs <- zillow.long %>%
    mutate(
        month = month - min(month),
        RegionID = factor(as.character(RegionID))
    ) %>%
    left_join(metros, by="RegionID")
print(filter(zillow_acs, month==0), n=20) #Check merge

## PREPARE DATA
zillow_acs <- zillow_acs %>%
    arrange(RegionName, month) %>%
    mutate(
        lnvalue_ti = log(value_t),
        Geo_REGION = factor(Geo_REGION,
                            labels = c("Northeast","Midwest","South","West")),
        RegionID   = factor(as.character(RegionID)),
        lnpop_i    = log(SE_T001_001), # Logged population 
        clnpop_i   = lnpop_i - mean(lnpop_i, na.rm = TRUE) # Center variable
    )

## DESCRIBE DATA
## Let's take a look at the first couple of metros with the four key 
## variables
d.ana <- select(zillow_acs, RegionID, month, lnvalue_ti, clnpop_i)
print(d.ana, n = 50)

## Plot individual trajectories
quintiles <- group_by(zillow_acs, RegionID) %>% slice(1L) %>%
    select(clnpop_i) %>% pull() %>% 
    quantile(probs = seq(0, 1, .2), na.rm=TRUE)
d.ana <- d.ana %>% mutate(clnpop5 = cut(clnpop_i, quintiles))
g.desc <- ggplot(d.ana, aes(x=month,y=lnvalue_ti,group=RegionID, color=clnpop5)) +
    geom_line() +
    scale_color_brewer(palette="Blues") +
    labs(
        title = "Trend in price per sq. ft. by metropolitan area",
        subtitle = "Colors represent quintiles of population",
        color = "Quintile cutpoints"
    )
g.desc

## ANALYZE THE DATA
## Intercept-only: Influence of (logged) population on intercept only
m.int <- lmer(lnvalue_ti ~ clnpop_i + month + (1 + month | RegionID), 
              data=d.ana)
## Slope-only: Influence of (logged) population on slope only
m.slp <- lmer(lnvalue_ti ~ clnpop_i:month + month + (1 + month | RegionID), 
              data=d.ana)
## Intercept & slope: Influence of (logged) population on both intercept & slope only
m.ctr <- lmer(lnvalue_ti ~ clnpop_i * month + (1 + month | RegionID), 
              data=d.ana)
huxtable::huxreg(
    "Intercept-only" = m.int, 
    "Slope-only" = m.slp, 
    "Intercept & slope" = m.ctr,
    number_format = "%.5f",
    stars = NULL,
    statistics = c("N" = "nobs", "LL" = "logLik", "AIC" = "AIC"),
    coefs=c("(Intercept)", "clnpop_i", "month", "clnpop_i:month")
)

## Plot estimated values of parameters over original data
g.centered <- g.desc +
    geom_abline(
        intercept = fixef(m.ctr)["(Intercept)"],
        slope = fixef(m.ctr)["month"],
        size = 2, color = "darkorange"        
    )
g.centered

## PLOT MODEL WITH EFFECT OF POPULATION ON BOTH INTERCEPT & SLOPE
## Plot predicted influence of logged population on change in housing values
gammahat_00 <- fixef(m.ctr)["(Intercept)"]
gammahat_01 <- fixef(m.ctr)["clnpop_i"]
gammahat_10 <- fixef(m.ctr)["month"]
gammahat_11 <- fixef(m.ctr)["clnpop_i:month"]

m <- 0:25
nms <- c("Mean", "+1 sigma", "-1 sigma")
nd <- tibble(
    month = rep(m, 3),
    i = ordered(rep(nms, each = length(m)), levels=nms),
    clnpop_i = rep(c(0, 1, -1), each = length(m)),
    yhat = predict(m.ctr, newdata = tibble(month, clnpop_i), re.form = ~0)
)

modeled_year_end <- 2020
g.pres_base <- ggplot(nd, aes(x=month, y=yhat, color=i)) +
    labs(
        title = paste("Predicted logged values of price/sq.ft. of metro areas",
        "March", modeled_year_end-2, "to March ", modeled_year_end),
        subtitle = "Predictions based on population effect on intercept and slope",
        caption = "Note: Based on Zillow monthly median price data",
        x = "Month",
        color = "Metro Population"
    )

g.pres <- g.pres_base +
    geom_line() +
    labs(y = "Logged $/sq.ft.")
g.pres


## But its difficult to think in terms of logged price/sqft, let's make 
## the plot of price/sqft
g.presdollar <- g.pres_base +
    scale_y_continuous(limits = c(exp(5), exp(6))) +
    geom_line(aes(y=exp(yhat))) +
    labs(y = "$/sq.ft.") 
g.presdollar 
