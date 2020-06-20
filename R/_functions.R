#### KEEP STANDARD R PACKAGES ####
standard.pkgs <- c("stats","graphics","grDevices","utils","datasets","methods","base")
loaded.pkgs <- (.packages())[!((.packages())%in%standard.pkgs)]
if(length(loaded.pkgs)>0){
    sapply(paste0("package:",loaded.pkgs),detach,unload=TRUE,character.only=TRUE)
}
(.packages())

#### CONVENIENCE FUNCTIONS ####
## Convenience function to print the mean and standard deviation
mean.sd <- function(data,x_i,e_i) {
    mat <- sapply(data[,c(x_i,e_i)],function(x){round(c(mean(x),sd(x)),3)})
    rownames(mat) <- c('mean','sd')
    return(mat)
}

## Convenience function to identify months in the past year
get.past_year <- function(d) {
    varcol <- d[,'month']
    c((max(varcol)-24):max(varcol))
}

## Set seed to ensure reproducibility of simulations
set.seed(-1864853682)

