##' Function for calculate the drift on depth
##'
##' zoc generates an histogram from the upper part of the dives (ascending-surface-descending).
##' Based on the modal value obtained from the histogram, it gets the drift and returns a vector with the corrected depths
##' This function is not supposed to be used by the end user. Rather, it will be called from function Zocing
##' @title Zoc
##' @param x vector containing depth values to be corrected
##' @param zval depth threshold (lower)
##' @param blim depth threshold (upper, can be negative, ie, being 0 the surface, the seal can be out of the water)
##' @return a vector from which the drift value has been substracted
##' @author Simon Whoterspoon
##' @author Fer Arce
zoc <- function(x, zval = 50, blim = -20){
    h <- hist(x[x < zval & x > blim], right=FALSE, breaks = seq(blim, zval, 1), plot=FALSE)
                                        #abline(v=h$breaks[which.max(h$counts)], col="red")
                                        #print(h$breaks[which.max(h$counts)])
    ##scan("", 1)
    return(x - h$breaks[which.max(h$counts)])
}
