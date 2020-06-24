#### ANALYSIS EXAMPLE: TIME-INVARIANT PREDICTORS (CATEGORICAL) ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              and random slopes for 150 largest metros with Census 
##              region used to refine estimates of intercepts and slopes
## Author: Michael Bader

## GATHER DATA
## Uses same data as time invariant predictor analysis
source("02b_timeinvariantpredictor_analysis.R")
d.ana <- zillow_acs[, c("RegionID", "month", "lnvalue_ti", "Geo_REGION")]
d.ana$RegionID <- factor(d.ana$RegionID)

## DESCRIBE DATA
head(d.ana, 50)

## Plot individual trajectories
g.desc <- ggplot(d.ana, aes(x=month,y=lnvalue_ti,
                            group=RegionID, color=Geo_REGION)) +
    geom_line() 
g.desc

samp <- sample(levels(d.ana$RegionID),75)
g.samp <- ggplot(d.ana[d.ana$RegionID%in%samp,],
                 aes(x=month,y=lnvalue_ti,
                     group=RegionID, color=Geo_REGION)) +
    geom_line() 
g.samp

## ANALYZE THE DATA
m.ana <- lmer(lnvalue_ti ~ Geo_REGION * month + (1 + month | RegionID), 
              data=d.ana)
summary(m.ana)
huxtable::huxreg(m.ana)


## PLOT RESULTS BY REGION
## Get predicted values for each region
mock <- data.frame(
    Geo_REGION = rep(levels(d.ana$Geo_REGION),each=25),
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
