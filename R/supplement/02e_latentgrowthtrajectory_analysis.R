#### ANALYSIS EXAMPLE: LATENT GROWTH TRAJECTORY ANALYSIS ####
## Description: This file analyzes trajectories of home values by
##              classifying metropolitan areas into different classes
##              based on similarity of slopes and intercepts
## Author: Michael Bader

rm(list=ls())
library('lcmm')
load('../data/zillow_long.Rdata')

month_max <- max(zillow.long$month)
zillow.long <- zillow.long[zillow.long$month>=(month_max-12),]
zillow.long$lnvalue_ti <- log(zillow.long$value_t)
zillow.long$month <- zillow.long$month - min(zillow.long$month)

m1.hlme <-hlme(lnvalue_ti~month,
               subject='RegionID',mixture=~month,
               ng=3,idiag=TRUE,data=zillow.long)
summary(m1.hlme)
m1.hlme$BIC

pprob <- round(m1.hlme$pprob,5)
zillow.long <- merge(zillow.long,pprob,by="RegionID")
zillow.long$class <- factor(zillow.long$class)

ggplot(data=zillow.long,aes(x=month,y=lnvalue_ti,group=RegionID,color=class)) +
    geom_line()

m2.hlme <- hlme(lnvalue_ti~month,
                subject='RegionID',mixture=~month,
                random=~1,
                ng=3,idiag=TRUE,data=zillow.long)
summary(m2.hlme)
pprob_m2 <- round(m2.hlme$pprob,5)
names(pprob_m2)[2:ncol(pprob_m2)] <- paste0(names(pprob_m2)[2:ncol(pprob_m2)],"_m2")
zillow.long <- merge(zillow.long,pprob_m2,by="RegionID")
zillow.long$class_m2 <- factor(zillow.long$class_m2)
ggplot(data=zillow.long,aes(x=month,y=lnvalue_ti,group=RegionID,color=class)) +
    geom_line()

m2vars <- grep("RegionName|m2",names(zillow.long))
zillow.long[zillow.long$month==0,m2vars]
