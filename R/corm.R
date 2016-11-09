###############################################################################################
##### Correlation Matrix
###############################################################################################

#' @name corm
#' @aliases corm
#' @title Correlation matrix and correlation ranking of a data.frame
#' @description Calculating the descriptive statistics of a data.frame and exporting in a data.frame
#' @usage corm(x)
#' @param x :a data.frame
#' @examples corm(sp1500) for correlation matrix of sp1500


corm <- function(x){
  typeofvar <- sapply(x,class)
  ha <- names(typeofvar[typeofvar == "numeric" | typeofvar == "integer"])
  x <- x[,c(ha)]

  cormatirx <-data.frame(cor(x, use = "na.or.complete"))
  corrank <- data.frame(sort(cor(x,use = "na.or.complete")[,1],decreasing = T))
  return(cormatirx)
  return(corrank)
}
