#### ANALYSIS EXAMPLE: RANDOM INTERCEPTS ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              but constant slope for 150 largest metros
## Author: Michael Bader

source("_functions.R")
library(tidyverse)
library(lubridate)
library(lme4)
library(huxtable)

## GATHER THE DATA
load('../Data/zillow_long.RData')

## Subtract minimum value of month to set intercept to t=0
zillow.long <- zillow.long %>%
    mutate(
        month = month - min(month),
        date = ymd(paste0(yyyymm, "01")),
        lnvalue_ti = lnvalue_t, ## Helps us remember that value is per month/per metro
        )

## DESCRIBE THE DATA
g.ana <- ggplot(data=zillow.long,aes(x=month,y=lnvalue_ti,group=RegionID)) +
    geom_line() 
g.ana

d.int <- zillow.long[zillow.long$month==0,]
m.int <- paste(zillow.long[1,][["month.abbr"]], zillow.long[1,][["year"]])
qplot(d.int$lnvalue_ti, bins=15) +
      labs(
          x=paste("Logged median home value/sq. ft. in", m.int)
      )

## ANALYZE THE DATA
m.ana <- lmer(lnvalue_ti ~ month + (1 | RegionID),data=zillow.long)
summary(m.ana)

## INTERPRET THE DATA
g.pred <- g.ana +
    geom_abline(
        aes(intercept=m.ana@beta[1], slope = m.ana@beta[2]),
        color = "orange",
        size = 1.2
    )
g.pred

## REPORTING RESULTS
## The code below shows one way that you might want to report the predicted
## parameters of your model in a research paper or report

## Basic report (gets the job done, but is a little ugly)
## (This will cause R to issue warnings!)
h.ana <- huxreg(m.ana)
h.ana

## More advanced report (pretty, but more difficult)
## (This will cause R to issue a warning!)
h.ana <- huxreg(m.ana, 
                coefs = c("Intercept (gamma_00)" = "(Intercept)", 
                          "Month (beta_1)" = "month",
                          "tau_0i" = "sd__(Intercept)", 
                          "sigma_it" = "sd__Observation"),
                statistics = c("N"="nobs", "Log likelihood" = "logLik", 
                               "AIC" = "AIC")
)
h.ana %>%
    filter(model1 != "(NA)") %>% #Removes SE for variance components
    set_top_border(which(h.ana$names=="tau_0i"), 2, 1) %>%
    set_bottom_border(which(h.ana$names=="tau_0i")+1, 2, 1)


## Ignore Below (used for writing values to my notes)
m.ana.fe <- m.ana@beta
f <- file("../../lecture/_0103-analysis-estimates.tex")
writeLines(c(
    paste0("\\newcommand{\\intercept}{",round(m.ana.fe[1],3),"}"),
    paste0("\\newcommand{\\interceptexp}{", round(exp(m.ana.fe[1])), "}"),
    paste0("\\newcommand{\\slope}{",round(m.ana.fe[2],4),"}"),
    paste0("\\newcommand{\\varlevone}{",round(summary(m.ana)$sigma,3),"}"),
    paste0("\\newcommand{\\varlevtwo}{",round(summary(m.ana)$varcor$RegionID[1],3),"}")
    ),f)
close(f)
