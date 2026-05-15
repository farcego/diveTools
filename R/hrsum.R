##' Function for loading high resolution time-depth datasets
##'
##' This function reads and adapts HR datasets from the IMOS website.
##' Depth may be zero-offset corrected,but should be used with caution (i.e., not to be used here with datasets containing multiple seals) 
##' @title function for loading HR data 
##' @param patch the file to be uploaded (with it's patch)
##' @param origin ths source of the data (ie IMOS, direct tags etc)
##' @param zocing logical, wheter depths must be corrected and dive numbers to be generated or not.
##' @param list the data set is a list of seals
##' @param type character. Determines the source of the data and the processing steps
##' @return a data frame containing time and depth columns
##' @author Fer Arce
hrsum <- function(patch,origin, zocing=FALSE,dives=FALSE, list=FALSE,
                  type=c('IMOS','MACCA')){
    if (type[1]=='IMOS'){
        da <- read.csv(patch, header=TRUE, sep = '\t')
        names(da) <- c('Date','Depth')
        da$Date <- strptime(da$Date,format='%Y/%m/%d %H:%M:%S', tz='GMT')
        da$Date <- as.POSIXct(da$Date)
    }
    if (zocing) da <- Zocing(da)
    return(da)
}
