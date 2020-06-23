#### INITIALIZATION FILE FOR GROWTH CURVE MODELING CLASS ####
## Description: This file downloads and creates datasets for use in workshop
## Author: Michael Bader

wd <- '/Users/bader/work/Teaching/Workshops/GrowthCurveModeling/teaching-growth-curve-workshop/R/' ## Paste the path to the class directory you downloaded in the quotes
init <- function(wd) {
    rm(list=ls())

    ## Set path to working directory below
    if(wd==''){
        stop("Must define working directory path in _init.R",call.=FALSE)
    }
    setwd(wd)
    if(!dir.exists("../data")){
        dir.create("../data")
    }

    ## GATHER ZILLOW DATA
    data.url <- 'http://files.zillowstatic.com/research/public/Metro/'
    file.name <- 'Metro_MedianValuePerSqft_AllHomes.csv'
    price.sq.ft.url <- paste0(data.url,file.name)

    ## Load data and keep the largest 150 metros (starts at 2 because entire U.S. is
    ## listed in first row)
    zillow <- read.csv(price.sq.ft.url,header=TRUE)[2:151,]

    ## Save Zillow data object
    save(zillow,file="../data/zillow.RData")

    #### RESHAPE ZILLOW DATA
    ## Relabel variables to indicate that they represent housing values
    X.idx <- grep("X",names(zillow))
    names(zillow)[X.idx]<-sub("X","val.",sub("\\.","",names(zillow)[X.idx]))

    ## Reshape Zillow data frame into metro-month dataset
    zillow.long <- reshape(data=zillow,
                           direction="long",
                           varying=grep("val",names(zillow)),
                           v.names = "value_t",
                           timevar = "month",
                           idvar = "RegionID"
    )
    zillow.long <- zillow.long[order(zillow.long$RegionID),]
    zillow.long$month <- zillow.long$month - 1

    ## Add month/year variables
    month.abbrs <- tail(rep(month.abb,40),-3)[1:(max(zillow.long$month)+1)]
    N <- length(unique(zillow.long$RegionID))
    zillow.long$month.abbr <- rep(factor(month.abbrs,levels=month.abb),N)

    years <- c(rep(1996,9),rep(c(1997:2027),each=12))[1:length(month.abbrs)]
    zillow.long$year <- rep(years,N)

    ## Save long-format Zillow data
    save(zillow.long,file="../data/zillow_long.RData")
    
    ## GATHER AND FORMAT BLS MONTHLY UNEMPLOYMENT DATA
    ## Get data on the unemployment rate by metropolitan area from the 
    ## Bureau of Labor Statistics
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
    write.csv(bls, '../data/bls.csv')
}
init(wd)



