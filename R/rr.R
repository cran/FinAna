###############################################################################################
##### Calculating rate of return of a vector
###############################################################################################

#' @name rr
#' @aliases rr
#' @title Calculating rate of return of a vector
#' @description Calculating the rate of return of a vector for further analysis, including calculating beta of companies, plotting to see the trend of the stock for technical analysis
#' @usage rr(x)
#' @param x :a vector of company prices
#' @examples rr(aapl)


rr <- function(x){
  x <- as.data.frame(na.omit(x))
  if(dim(x)[1]>5){
  ratereturn <- (lag(as.ts(x), k = 1)/as.ts(x))-1
  }
  return(as.data.frame(ratereturn))
}
