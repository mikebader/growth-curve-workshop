#### INITIALIZATION FILE FOR GROWTH CURVE MODELING CLASS ####
## Description: This file downloads and creates datasets for use in workshop
## Author: Michael Bader

wd <- '' ## Paste the path to the directory 'R' in the quotes
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
    save(zillow.long,file="data/zillow_long.RData")
}
init(wd)



