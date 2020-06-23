#### ANALYSIS EXAMPLE: TIME-INVARIANT PREDICTORS ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              and random slopes for 150 largest metros with Census region
##              used to refine estimates of intercepts and slopes
## Author: Michael Bader

rm(list=ls())
source('_functions.R')
library(ggplot2)
library(lme4)

## GATHER DATA
## Download and format Zillow's crosswalk dataset
xwlk_url <- 'http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv'
xwlk <- read.csv(xwlk_url,header=TRUE)[,c('CBSAName','MetroRegionID_Zillow', 'CBSACode')]
xwlk <- xwlk[!duplicated(xwlk),]
names(xwlk) <- c("ZillowName","RegionID","Geo_CBSA")

## Download Census 2010 data (that includes geographic identifiers
## and total population)
census2010 <- read.delim("../data/R11740339_SL320.txt",encoding="UTF-8")
census2010 <- census2010[,c("Geo_NAME","Geo_CBSA","Geo_REGION","SE_T001_001")]

## Select part of metro with largest population
census2010 <- census2010[order(census2010$SE_T001_001),]
census2010$i <- 1:nrow(census2010)
vals <- tapply(census2010$i,census2010$Geo_NAME,max)
metros <- census2010[census2010$i %in% vals,]

metros <- merge(xwlk,metros,by="Geo_CBSA")

## Load Zillow data and merge to Census
load("../data/zillow_long.Rdata")
zillow_acs <- merge(zillow.long,metros,by="RegionID",all.x=TRUE)
head(zillow_acs[zillow_acs$month==0,
                   c("RegionName","Geo_NAME")],20) #Check merge

## PREPARE DATA
zillow_acs <- zillow_acs[order(zillow_acs$RegionName,zillow_acs$month),]
zillow_acs <- zillow_acs[zillow_acs$month>=(max(zillow_acs$month)-24),]
zillow_acs$month <- zillow_acs$month - min(zillow_acs$month)
zillow_acs$lnvalue_ti <- log(zillow_acs$value_t)
zillow_acs$Geo_REGION <- factor(zillow_acs$Geo_REGION,
                                labels=c("Northeast","Midwest","South","West"))
zillow_acs$RegionName <- factor(zillow_acs$RegionName)

## DESCRIBE DATA
g.desc <- ggplot(zillow_acs[],aes(x=month,y=lnvalue_ti,
                        group=RegionName,color=Geo_REGION)) +
            geom_line()
g.desc

samp <- sample(levels(zillow_acs$RegionName),75)
g.samp <- ggplot(zillow_acs[zillow_acs$RegionName%in%samp,],
                 aes(x=month,y=lnvalue_ti,
                     group=RegionName,color=Geo_REGION)) +
            geom_line()
g.samp

## ANALYZE THE DATA
m.ana <- lmer(lnvalue_ti ~ month * SizeRank + Geo_REGION + 
                  (1 + month | RegionID), data=zillow_acs)
summary(m.ana)
m.ana.fe <- fixef(m.ana)
m.ana.re <- ranef(m.ana)$RegionID

## PLOT RESULTS BY REGION
## Get predicted values for each region
mock <- data.frame(
    Geo_REGION = rep(levels(zillow_acs$Geo_REGION),each=25),
    SizeRank = 75,
    month = rep(0:24,4)
)
mock$lnvaluehat_ti <- predict(m.ana,newdata=mock,re.form=NA)

## Plot the predicted logged value/sqft, the scale at which the model was 
## estimated
g.link <- ggplot(data=mock,aes(x=month,y=lnvaluehat_ti,
                     group=Geo_REGION,color=Geo_REGION)) +
                geom_line()
g.link

## Plot the predicted value/sqft (in dollars)
g.resp <- ggplot(data=mock,aes(x=month,y=exp(lnvaluehat_ti),
                               group=Geo_REGION,color=Geo_REGION)) +
    geom_line(size=1.5) 
g.resp

## Now we can make the plot look pretty for publications, etc.
month_labels <- mapply(
    paste, rep(month.abb,3)[seq(3,27,3)], c(rep(c(2018,2019), each=4), 2020)
)
g.region <- g.resp +
    scale_x_continuous(
        breaks=seq(0,24,3), 
        labels=month_labels) +
    labs(
        title="Predicted change in price by region",
        subtitle="March 2018-Mar 2020",
        y="Predicted price per sq. ft.", x="Month",
        color="Region"
        ) +
    theme(legend.position = "bottom")
g.region

## PLOT PREDICTED VALUES BY SIZE OF METRO AREA
mock <- data.frame(
    Geo_REGION = "Northeast",
    SizeRank = c(25, 75, 125),
    month = rep(0:24,3)
)
mock$lnvaluehat_ti <- predict(m.ana,newdata=mock,re.form=NA)
mock$SizeRank <- factor(mock$SizeRank)

## Plot values in logged price/sqft
g.link <- ggplot(data=mock,aes(x=month,y=lnvaluehat_ti,
                               group=Geo_REGION,color=Geo_REGION)) +
    geom_line()
g.link

## Plot values in price/sqft
g.resp <- ggplot(data=mock,aes(x=month,y=exp(lnvaluehat_ti),
                               group=SizeRank,color=SizeRank)) +
    geom_line()
g.resp

## Make pretty
g.pres <- g.resp +
    scale_color_manual(values=c("#0000dd", "#0000aa", "#000066")) +
    scale_x_continuous(
        breaks=seq(0,24,3), 
        labels=month_labels) +
    scale_y_continuous(limits=c(100,265)) +
    labs(
        title="Predicted change in price by population rank of metro area",
        subtitle="March 2018-Mar 2020",
        y="Predicted price per sq. ft.", x="Month",
        color="Rank"
    ) +
    theme(legend.position = "bottom")
g.pres
