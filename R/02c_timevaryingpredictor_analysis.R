rm(list=ls())
bls_url <- "https://www.bls.gov/web/metro/ssamatab1.txt"

bls <- read.fwf(bls_url,c(16,7,12,64,8,6,10,18,14,14))
bls <- bls[-1:-5,]
names(bls) <- c("laus","stfips","fips","name","year","month",
                "civ_labor_force","employment","unemployment",
                "unemp_rate")
numvars <- c("stfips","fips","year","month","unemp_rate")
bls[,numvars] <- apply(bls[,numvars],2,function(x) as.numeric(as.character(x)))
charvars <- c("laus","name")
bls[,charvars] <- apply(bls[,charvars],2,function(x) trimws(as.character(x)))
bls <- bls[,c("name",numvars)]
bls$uniqid <- paste0(
                sprintf("%05.0f",bls$fips),
                sprintf("%04.0f",bls$year),
                sprintf("%02.0f",bls$month)
                )


load("data/zillow_long.Rdata")
xwlk_url <- 'http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv'
xwlk <- read.csv(xwlk_url,header=TRUE)[,c('CBSAName','MetroRegionID_Zillow', 'CBSACode')]
xwlk <- xwlk[!duplicated(xwlk),]
names(xwlk) <- c("RegionName","RegionID","fips")

zillow.long$month.num <- sapply(zillow.long$month.abbr,
                                function(x) which(month.abb==x))

zillow.bls <- merge(zillow.long,xwlk[,2:3],by="RegionID")
zillow.bls$fips <- as.numeric(as.character(zillow.bls$fips))
zillow.bls$uniqid <- paste0(
                        sprintf("%05.0f",zillow.bls$fips),
                        sprintf("%04.0f",zillow.bls$year),
                        sprintf("%02.0f",zillow.bls$month.num)
                    )

## NOTE: These data are missing values for New England metropolitan areas
zillow.bls <- merge(zillow.bls,bls[,c("uniqid","unemp_rate")],by="uniqid",all.x=TRUE)

zillow.bls$lnvalue_ti <- log(zillow.bls$value_t)
zillow.bls <- zillow.bls[zillow.bls$month>=(max(zillow.bls$month)-12),]
zillow.bls$month <- zillow.bls$month - min(zillow.bls$month)

## DESCRIBE THE DATA
cor(zillow.bls[,c("lnvalue_ti","unemp_rate")],use="complete.obs")

## ANALYZE THE DATA
m.ana <- lmer(lnvalue_ti~month + unemp_rate + (1|RegionID),data=zillow.bls)
summary(m.ana)

## INTERPRET THE RESULTS
mean_unemp <- mean(zillow.bls[zillow.bls$month==0,"unemp_rate"],na.rm=TRUE)
mock <- data.frame(
    RegionID <- factor(rep(1:2,each=13),
                       labels=c("mean unemp.","1 pct. decline")),
    month <- rep(0:12,2),
    unemp_rate <- c(rep(mean_unemp,13),mean_unemp*0:12*(-1/12))
)
mock$lnvalue_ti_hat <- predict(m.ana,newdata=mock,re.form=NA)

ggplot(data=mock,aes(x=month,y=lnvalue_ti_hat,
                     group=RegionID,color=RegionID)) +
    geom_line()

## MODEL WITH RANDOM INTERCEPTS & SLOPES
m.ana2 <- lmer(lnvalue_ti~month + unemp_rate + (1+month|RegionID),data=zillow.bls)
summary(m.ana2)

mock$lnvalue_ti_hat_2 <- predict(m.ana2,newdata=mock,re.form=NA)
ggplot(data=mock,aes(x=month,y=lnvalue_ti_hat_2,
                     group=RegionID,color=RegionID)) +
    geom_line()

## CENTER UNEMPLOYMENT
zillow.bls$unemp_rate_c <- zillow.bls$unemp_rate - mean_unemp
m.ana3 <- lmer(lnvalue_ti~month + unemp_rate_c + (1+month|RegionID),data=zillow.bls)
summary(m.ana3)
