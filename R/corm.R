###############################################################################################
##### Correlation Matrix
###############################################################################################

#' @name corm
#' @aliases corm
#' @title Correlation matrix and correlation ranking of a data.frame
#' @description Calculating the descriptive statistics of a data.frame and exporting in a data.frame
#' @usage corm(x,n)
#' @param x :a data.frame
#' @param n :number of decimal points
#' @examples #corm(sp1500,3) for correlation matrix of sp1500


corm <- function(x,n){
  typeofvar <- sapply(x,class)
  ha <- names(typeofvar[typeofvar == "numeric" | typeofvar == "integer" | typeofvar == "double"])
  x <- x[,c(ha)]

  cormatirx <-data.frame(round(cor(x, use = "na.or.complete"), n))
  corrank <- data.frame(sort(cor(x,use = "na.or.complete")[,1],decreasing = T))
  return(cormatirx)
  return(corrank)
}
