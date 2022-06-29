#### SIMULATION EXAMPLE: TIME VARYING COVARIATES ON GROWTH MODEL ####
## Description: This file simulates a dataset with time varying covariates,
##              including both a level of change and altering the rate of change
## Author: Michael Bader

source('_functions.R')
library(tidyverse)
library(huxtable)
library(MASS)
library(lme4)
theme_set(theme_minimal() + theme(legend.position = "top"))

## SCENARIO
## In this scenario we predict that having heart surgery increases depression
## immediately post-surgery (interruption from the trend) and alters the rate
## of depression over time after having surgery. We will assume we follow 150
## patients over 5 years and that approximately 25% of patients have heart
## surgery. The outcome depression scale is measured on a scale of 1-10.

## CONJURE POPULATION
N   <- 150 
N_t <- 5

## Create individual-level data containing whether patient had heart surgery
## and, if so, when
P_surgery <- 0.25
d_person <- tibble(
    i = rep(1:N),
    has_surgery = as.logical(rbinom(N, 1, P_surgery)),
    t_surgery = if_else(
        has_surgery,
        map_int(1:N, ~ sample(0:(N_t - 1), 1)),
        NA_integer_
    ),
    N_t = N_t
) 

## Create a person-year dataset containing variales for:
##    a) whether a patient ever had heart surgery
##    b) how long it's been since they had heart surgery
d_persyr <- d_person %>%
    uncount(N_t) %>%
    group_by(i) %>%
    mutate(
        t = 0:(n() - 1),
        had_surgery = if_else(has_surgery & t >= t_surgery, 1, 0),
        timesince = if_else(has_surgery & t > t_surgery, t - t_surgery, 0L)
    ) %>%
    ungroup()

## Now we can simulate depression (our outcome variable) based on a model we 
## specify
gamma_00 <- 5       ## Conditional mean depression when t=0 for those w/o surgery 
gamma_10 <- -1/8    ## Annual change in depression
gamma_20 <- 0.5     ## Immediate jump in depression due to heart surgery
gamma_30 <- 3/16    ## Annual difference in change for those who had heart
                    ## from those who did not surgery

tau_00 <- (.5)^2    ## Variance around conditional mean intercept
tau_11 <- (1/16)^2  ## Variance around conditional mean slope
tau_01 <- 0         ## Correlation between person-specific intercept and slope
sigma  <- 1/16      ## Standard deviation of error off of patient's trend

## Draw random distribution of person-specific intercept and slope errors
## In this example, tau_00 and tau_11 are uncorrelated so we can draw each
## distribution of person-specific errors indepedently without using
## multivariate normal distribution
beta_0 = gamma_00 + rnorm(N, 0, sqrt(tau_00))
beta_1 = gamma_10 + rnorm(N, 0, sqrt(tau_11))

## If you did want to model correlations between the patient-specific intercept
## and slope parameters, you could use the following code:
# Tau <- matrix(c(tau_00, tau_01, tau_01, tau_11), nrow=2)
# Rho <- MASS::mvrnorm(N, mu = c(0, 0), Sigma = Tau)
# beta_0 = gamma_00 + Rho[, 1]
# beta_1 = gamma_10 + Rho[, 2]

## Make the data with fixed and random components
d_persyr <- d_persyr %>% 
    mutate(
        depression = rep(beta_0, each = N_t) + rep(beta_1, each = N_t) * t +
                     gamma_20 * had_surgery + gamma_30 * timesince +
                     rnorm(n(), 0, sigma)
    )

## Plot simulated data
## Color lines by whether patient ever had surgery
ggplot(d_persyr, aes(x = t, y = depression, group = i, color = has_surgery)) +
    geom_line(size = .25) +
    labs(
        title = "Simulation of heart surgery on depression",
        subtitle = "Simulation of 150 individual trajectories over five years",
        color = "Ever had heart surgery"
    )

## Calculate mean depression in years when patients had surgeries and in years
## when they did not (the difference should equal gamma_20)
d_persyr %>%
    group_by(had_surgery) %>%
    summarize(mean = mean(depression))

## ANALYZE THE SIMULATED DATA
## Estimate model parameters of simulated data
m <- lmer(depression ~  t + had_surgery + timesince + 
                       (1 + t | i),
          data = d_persyr)
summary(m)

## PRESENT RESULTS
## Save information about model variance components and groups
vc <- VarCorr(m)
m_out <- tidy_override(m,
                       glance = list(
                           tau_00 = vc$i[1,1],
                           tau_11 = vc$i[2,2],
                           tau_01 = vc$i[1,2],
                           N_grps = as.integer(length(levels(m@flist[[1]])))
                       ),
                       extend = TRUE
)

## Write results to table for output
coef_names <- c(
    "Time" = "t", "Had surgery" = "had_surgery", 
    "Time since surgery" = "timesince"
)
ranef_names <- c("tau\\_00" = "tau_00", "tau\\_11" = "tau_11",
                 "tau\\_01" = "tau_01", "sigma\\^{}2" = "sigma")
stat_names <- c(ranef_names, 
                "Log likelihood" = "logLik", "AIC" = "AIC", "BIC" = "BIC",
                "N (Patient-periods)" = "nobs", "N (Patient)" = "N_grps")
huxreg(list("Model" = m_out),
       stars = NULL, 
       statistics = stat_names,
       coefs = coef_names[1:3]) %>%
    set_bottom_border((nrow(.) - c(2, 5)), 2, 0.5) %>%
    set_number_format(nrow(.), 2, "%.0f") %>%
    set_caption(paste(
        "Parameter estimates and standard errors (in parentheses) of random",
        "intercepts and slopes model of simulated data measuring influence",
        "of heart surgery on depression"
    )) %>%
    set_escape_contents(everywhere, 1, FALSE) %>%
    set_tb_padding(everywhere, everywhere, 0) 

## Describe three types of cases and create data for those three types
case1 <- tribble(   # Never had surgery
    ~i, ~t, ~had_surgery, ~timesince,
    "No surgery", 0, 0, 0,
    "No surgery", 1, 0, 0,
    "No surgery", 2, 0, 0,
    "No surgery", 3, 0, 0,
    "No surgery", 4, 0, 0
)

case2 <- tribble(   # Had surgery at baseline
    ~i, ~t, ~had_surgery, ~timesince,
    "Surgery (t=0)", 0, 1, 0,
    "Surgery (t=0)", 1, 1, 1,
    "Surgery (t=0)", 2, 1, 2,
    "Surgery (t=0)", 3, 1, 3,
    "Surgery (t=0)", 4, 1, 4
)

case3 <- tribble(    # Had surgery in third year (i.e., t = 2)
    ~i, ~t, ~had_surgery, ~timesince,
    "Surgery (t=2)", 0, 0, 0,
    "Surgery (t=2)", 1, 0, 0,
    "Surgery (t=2)", 2, 1, 0,
    "Surgery (t=2)", 3, 1, 1,
    "Surgery (t=2)", 4, 1, 2
)

## Create predictions from fixed effects of each of these three cases
d_pr <- bind_rows(case1, case2, case3) %>%
    mutate(
        depression_hat = predict(m, newdata = ., re.form = ~0)
    )

## Make a plot showing the trajectories of these three example cases
ggplot(d_pr, aes(x = t, y = depression_hat, color = i)) +
    geom_line(size = 1) +
    labs(
        title = "Association between depression and heart surgery",
        subtitle = paste("Three example trajectories based on analysis of", 
                         "simulated data (N=150)"),
        y = "Predicted depression",
        x = "Time",
        color = "Example trajectory"
    )
