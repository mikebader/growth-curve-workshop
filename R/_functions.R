#### CONVENIENCE FUNCTIONS ####
## Convenience function to print the mean and standard deviation
mean.sd <- function(data,x_i,e_i) {
    mat <- sapply(data[,c(x_i,e_i)],function(x){round(c(mean(x),sd(x)),3)})
    rownames(mat) <- c('mean','sd')
    return(mat)
}

## Set seed to ensure reproducibility of simulations
set.seed(-1864853682)

## Set plot theme
library(ggplot2)
theme_set(theme_minimal())
