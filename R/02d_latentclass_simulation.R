#### SIMULATION EXAMPLE: LATENT CLASS ANALYSIS ####
## Description: This file simulates a dataset of organized and unorganized
##              people based on simulation of their number of unread
##              emails, number of sheets on their desks, and number of
##              tabs open on their browsers
## Author: Michael Bader

rm(list=ls())
source('_functions.R')
library(MASS)
library(poLCA)

# Create the number of people of each type (org=organized, dis=disorganized)
N.org <- 250
N.dis <- 750
N <- N.org + N.dis

# Simulate population of organized people
mu.org <- c(75,60,8)
Sigma.org <- matrix(c(100,0,0,0,100,0,0,0,4),3,3)
org.pop <- round(mvrnorm(N.org,mu=mu.org,Sigma=Sigma.org))

# Simulate population of disorganized people
mu.dis <- c(100,100,10)
Sigma.dis <- Sigma.org
dis.pop <- round(mvrnorm(N.dis,mu.dis,Sigma.dis))

# Combine the two populations into a single populations and make a dataset
#   Recall that, if we were getting the data to analyze we wouldn't know who was in the
#   organized group versus the disorganized group. We just know that because we simulated
#   our data
lcpop <- data.frame(rbind(org.pop,dis.pop))
names(lcpop) <- c('emails','sheets','tabs')
lcpop$class <- factor(c(rep("a",N.org),rep("b",N.dis)))
lcpop[sample(1:N, 20, replace=FALSE),]

# Examine the correlation matrix of the full sample
#   Notice how our variables are highly correlated with one another
cor(lcpop[,1:3])

# Examine the correlation matrices of the samples of organized and disorganized people
#   Notice how the variables *within* each class are not correlated with one another
cor(lcpop[lcpop$class=="a",1:3])
cor(lcpop[lcpop$class=="b",1:3])

# # Now we create categories for our data (we categorize each variable into four categories)
lcpop[,5:7] <- sapply(lcpop[,1:3], function(x){
    x <- cut(x, breaks=5, include.lowest=TRUE, ordered_result = TRUE)
    as.numeric(x)
})
names(lcpop)[5:7] <- paste0(names(lcpop[,1:3]), "cat")

# Conduct a latent class analysis on our entire sample, starting with 2 classes
lca2 <- poLCA(cbind(emailscat, sheetscat, tabscat) ~ 1, 
              maxiter=50000, nclass=2, nrep=10, data=lcpop)
lcpop$predclass2 <- lca2$predclass
table(lcpop[,c('class', 'predclass2')])

# Now let's try three classes to see if the model fits better
lca3 <- poLCA(cbind(emailscat, sheetscat, tabscat) ~ 1, 
              maxiter=50000, nclass=3, nrep=10, data=lcpop)

# Test whether the BIC from our 3-class model is lower than the BIC from our
# 2-class model (it shouldn't be since we know there are two classes!)
lca3$bic < lca2$bic
# (Phew!)
# As a fun excercise, you can calculate the BIC yourself based on the components:
BIC <- -2*lca2$llik+lca2$npar*log(lca2$Nobs)

# Examine how well our data matched what we know to be the truth by creating a variable
# containing the predicted class (C) and posterier predicted probability of class membership
# in the two classes (probC)
sample$C <- lca2$predclass
sample[,c('prob1','prob2')] <- round(lca2$posterior,5)
sample[1:100,]
