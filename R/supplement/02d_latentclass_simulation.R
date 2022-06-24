#### SIMULATION EXAMPLE: LATENT CLASS ANALYSIS ####
## Description: This file simulates a dataset of organized and unorganized
##              people based on simulation of their number of unread
##              emails, number of sheets on their desks, and number of
##              tabs open on their browsers
## Author: Michael Bader

source('_functions.R')
set.seed(560)
library(tidyverse)
library(MASS)
library(poLCA)

# Create the number of people of each type (org=organized, dis=disorganized)
N.org <- 250
N.dis <- 750
N <- N.org + N.dis

# Simulate population of organized people
mu.org    <- c(75,60,8)
Sigma.org <- diag(c(100, 100, 4))
org.pop   <- round(mvrnorm(N.org, mu=mu.org, Sigma=Sigma.org))

# Simulate population of disorganized people
mu.dis    <- c(100,100,10)
Sigma.dis <- Sigma.org
dis.pop   <- round(mvrnorm(N.dis,mu.dis,Sigma.dis))

# Combine the two populations into a single populations and make a dataset
#   Recall that, if we were getting the data to analyze we wouldn't know who was in the
#   organized group versus the disorganized group. We just know that because we simulated
#   our data
cnames <- c('emails', 'sheets', 'tabs')
lcpop <- rbind(org.pop,dis.pop) %>%
    magrittr::set_colnames(cnames) %>%
    as_tibble() %>%
    mutate(
        class = factor(c(rep("a",N.org),rep("b",N.dis)))
    )
sample_n(lcpop, 20)

# Examine the correlation matrix of the full sample
#   Notice how our variables are correlated with one another
cor(lcpop[,1:3])

# Examine the correlation matrices of the samples of organized and disorganized people
#   Notice how the variables *within* each class are not correlated with one another
cor(lcpop[lcpop$class=="a",1:3])
cor(lcpop[lcpop$class=="b",1:3])

# # Now we create categories for our data (we categorize each variable into five categories)
makecats <- function(.) {
    cut(., breaks = 5, include.lowest = TRUE, ordered_result = TRUE) %>%
        as.integer()
}
lcpop <- lcpop %>%
    mutate(across(all_of(cnames), .fns = list(cat = ~makecats(.))))

# Conduct a latent class analysis on our entire sample, starting with 2 classes
lca2 <- poLCA(cbind(emails_cat, sheets_cat, tabs_cat) ~ 1, 
              maxiter=50000, nclass=2, nrep=10, data=lcpop)
lcpop$predclass2 <- lca2$predclass

# Now let's try three classes to see if the model fits better
lca3 <- poLCA(cbind(emails_cat, sheets_cat, tabs_cat) ~ 1, 
              maxiter=50000, nclass=3, nrep=10, data=lcpop)

# Test whether the BIC from our 3-class model is lower than the BIC from our
# 2-class model (it shouldn't be since we know there are two classes!)
lca3$bic < lca2$bic
# (Phew!)

# As a fun excercise, you can calculate the BIC yourself based on the components:
BIC <- -2 * lca2$llik + lca2$npar * log(lca2$Nobs)

# We can then get the predicted class membership for each observation and the
# probability of class membership across the two classes
smp <- tibble(
    C = factor(lca2$predclass),
    PC = as.data.frame(round(lca2$posterior, 5))
) %>%
    unnest_wider(PC) %>%
    rename(PC1 = 2, PC2 = 3)
print(sample_n(smp, 20), n=20)


# Plot the distribution of variables used to detect latent class
lcpop_long <- bind_cols(lcpop, smp) %>%
    pivot_longer(
        ends_with("cat")
    )
ggplot(lcpop_long, aes(x = value, fill = C)) +
    geom_bar(data = filter(lcpop_long, C == 1), alpha = .5) +
    geom_bar(data = filter(lcpop_long, C == 2), alpha = .5) +
    facet_wrap(~name) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )

# Since we know the true class since we created our data, we can examine
# how well the predicted class fit the actual class
table(lcpop[,c('class', 'predclass2')])
