#### INITIALIZATION FILE FOR GROWTH CURVE MODELING CLASS ####
## Description: This file downloads and creates datasets for use in workshop
## Author: Michael Bader

wd <- '/Users/bader/work/teaching/workshops/modeling-change/materials/R' ## Paste the path to the class directory you downloaded in the quotes
init <- function(wd) {
    library(tidyverse)

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
    zillow.long <- zillow %>%
        pivot_longer(
            cols         = starts_with('val'),
            names_to     = "yyyymm",
            names_prefix = "val\\.",
            values_to    = "value_t"
        ) %>%
        group_by(RegionID) %>%
        mutate(
            month = 0:(n() - 1),
            lnvalue_t = log(value_t),
            year = substr(yyyymm, 1, 4),
            month.abbr = month.abb[as.numeric(substr(yyyymm, 5, 6))],
        ) %>%
        ungroup()

    ## Save data from March 2018 to March 2020
    zillow.long.all <- zillow.long
    mnum <- zillow.long %>%
        filter(yyyymm=='202003') %>%
        slice(1) %>%
        pull(month)
    zillow.long <- zillow.long %>%
        filter(month >= (mnum - 25))

    ## Save long-format Zillow data
    save(zillow.long, zillow.long.all, file="../data/zillow_long.RData")

    ## GATHER METRO POPULATION DATA
    ## GATHER DATA
    ## Download and format Zillow's crosswalk dataset
    xwlk_url <- 'http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv'
    xwlk <- read.csv(xwlk_url,header=TRUE)[,c('CBSAName','MetroRegionID_Zillow', 'CBSACode')]
    xwlk <- as_tibble(xwlk[!duplicated(xwlk),])
    names(xwlk) <- c("ZillowName","RegionID","Geo_CBSA")
    
    ## Download Census 2010 data (that includes geographic identifiers
    ## and total population)
    metros <- read_delim("../data/R11740339_SL320.txt", delim="\t") %>%
        select(Geo_NAME, Geo_CBSA, Geo_REGION, SE_T001_001) %>%
        arrange(-SE_T001_001) %>%
        group_by(Geo_NAME) %>%
        mutate(
            totpop = sum(SE_T001_001),
            Geo_CBSA = factor(as.character(Geo_CBSA))) %>%
        slice(1L) %>%
        ungroup() %>%
        left_join(xwlk, by = "Geo_CBSA")
    save(metros, file='../data/metros.Rdata')
    
    ## GATHER DC NEIGHBORHOOD COMPOSITION DATA
    fname <- "../data/nhgis0084_csv/nhgis0084_ts_geog2010_tract.csv"
    if(!file.exists(fname)){
        fname = "https://raw.githubusercontent.com/mikebader/growth-curve-workshop/master/Data/nhgis0084_csv/nhgis0084_ts_geog2010_tract.csv"
    }

    trts <- read_csv(fname) 

    dcpop <- filter(trts, STATEA=="11") %>%
        group_by(TRACTA) %>%
        mutate(
            year = DATAYEAR,
            t = (year - 1990) / 10,
            nhd = TRACTA,
            i = cur_group_id(),
            totpop = CL8AA,
            pnhw = 100 * CY8AA / totpop
        ) %>%
        ungroup() %>%
        arrange(i, t) %>%
        select(nhd, year, i, t, pnhw)
    save(dcpop, file="../data/dc-nhw-population.Rdata")


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
