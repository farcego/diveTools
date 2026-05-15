##' Function to correct the drifts on the depth measurements of the tags. i have to reimplement it in a way that it goes though a given data frame colukmn.
##' looks that tehre was a typo and it made a lot of stupid dives
##'
##' Esentially is a wrapper along a Spoon's pieze of coding art. It does not only correct the depth, but also generates the sequence of dives
##' @title Zocing function
##' @param Data High resolution time depth profiles
##' @param thres Threshold depth to be considered as surface
##' @param overwrite Logical, whether the actual Depth column should be overwritten or not
##' @return A Data frame with two extra columns added: the corrected depth and the dive number
##' @author Simon Wotherspoon
##' @author Fer Arce
Zocing <- function(Data,thres = 10, overwrite=TRUE, Cut){
    ti <- "6 hours" ## time interval to cut the file
    ct <- cut(Data$Date, ti) ## runs the zocing function for all time intervals.
    ## Unlist reformats the tappply output
    zd <- unlist(tapply(Data$Depth, ct, zoc))
    ## sets surface intervals within the "surface noise" value to 0
    sn <- thres
    names(zd) <- NULL
    nzd <- zd
    nzd[zd < sn] <- 0
    ## ......and adds these data to the dataframe
    l <- length(nzd)-length(Data$Depth)
    if (l==0) Data$cordep <- nzd
    if (l!=0) Data$cordep <- c(nzd,0)
    ## assigns numbers to each dive based on a user defined
    ## "surface noise" (sn) value
    ##Gives each surface or dive section a consecutive number
    ##Relaces NA in depth with 0
    nzd[is.na(nzd)] <- 0
    Data$cordep[is.na(Data$cordep)] <- 0
    if (overwrite) {
        Data$Depth <- Data$cordep
        Data$cordep <- NULL
    }
    if(!missing(Cut)) Data$Depth[Data$Depth <= Cut] <- 0
    return(Data)
}
