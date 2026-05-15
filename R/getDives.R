## formerly know as GetSeals
##' .. content for \description{} (no empty lines) ..
##'
##' This function make some renaming, against conventions from SMRU files. I'll have to eventually fix it
##' @title GetDives
##' @param deployment Name of the deployment (.e. ct79)
##' @param split Logical. If truw, data will be transformed in a list of individual seals
##' @param preclean Logical. If TRUE, some dives not matching certain metrics will be deleted
##' @param folder 
##' @param filename Name of the file where dive data is stored
##' @param keep  Logical. default FALSE. if TRUE, it will keep all columns of the file
##' @return 
##' @author Fer Arce
GetDives <- function(deployment, split=TRUE, preclean = TRUE,
                     folder = '~/phd/data/IMOS/', filename = 'dive', format = 'csv', keep = FALSE){
    '%out%' <- Negate('%in%')
    if (format != 'csv') stop('format not implement') 
    file <- paste(folder,deployment,'/',deployment,filename,'.',format, sep = '')
    focas <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
    ##if(deployment %in%c('ct133', 'ct135', 'ct140')) focas$ref <- focas$REF
    if('ref' %out% names(focas))
        focas$ref <- focas$REF
    if('lat' %out% names(focas))
        focas$lat <- focas$LAT
    if('lon' %out% names(focas))
        focas$lon <- focas$LON
    if(!keep)
        focas <- focas[,c('ref','DE_DATE','SURF_DUR','DIVE_DUR','MAX_DEP',
                          'D1','D2','D3','D4','T1','T2','T3','T4','lat','lon')]
    focas$Date <- as.POSIXct(strptime(focas$DE_DATE, format='%d/%m/%y %H:%M:%S', tz='GMT'))
    focas$day <- as.POSIXct(substr(focas$DE_DATE,1,8), tz='GMT')
    focas <- focas[order(focas$Date), ]
    focas$t1 <- focas$T1
    focas$t2 <- focas$T2
    focas$t3 <- focas$T3
    focas$t4 <- focas$T4
    focas$T1 <- (focas$t1*focas$DIVE_DUR)/100
    focas$T2 <- (focas$t2*focas$DIVE_DUR)/100
    focas$T3 <- (focas$t3*focas$DIVE_DUR)/100
    focas$T4 <- (focas$t4*focas$DIVE_DUR)/100
    if (preclean) focas <- diveC(focas)
    if (split) focas <- split(focas, focas$ref)
    ## inid <- sapply(focas, nrow)
    ## focaS <- lapply(focas, diveRem)
    ## endd <- sapply(focaS, nrow);rm(focaS)
    return(focas)
}
