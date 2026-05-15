##' Function for numbering dives from the high resolution time depth
##' profiles
##'
##' DiveNum generates a column with a sequential dive index to work
##' with time-depth at dive level. While it allos to explicitly choose
##' of name of the new generated variable, it is recommended to keep
##' the pre-defined name of the function for further processing
##' withing the package functions.
##' @title DiveNum
##' @param Data a Data set containing at least a column with depth
##'     profiles
##' @param column name of the column containing the depth stamps
##' @param dive desired name of the column to contain the dive index
##' @return the same data.frame used as imput with an extra column
##'     containing the generated dive index
##' @author fer
DiveNum <- function(Data, column='Depth', dive='dive'){
    ##num <- ceiling(cumsum(abs(c(0, diff(Data$Depth == 0))))/2)
    num <- ceiling(cumsum(abs(c(0, diff(Data[,column] == 0))))/2)
    ##Data$dive <- num
    Data[,dive] <- num
    return(Data)
}
