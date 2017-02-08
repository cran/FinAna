###############################################################################################
##### Calculating rate of return of a vector
###############################################################################################

#' @name rr
#' @aliases rr
#' @title Calculating rate of return of a vector
#' @description Calculating the rate of return of a vector for further analysis, including calculating beta of companies, plotting to see the trend of the stock for technical analysis
#' @usage rr(x,n)
#' @param x :a vector of company prices
#' @param n : number of lags
#' @examples #rr(aapl,1)


rr <- function(x, n){

  ratereturn <- c(as.numeric("NA"),(lag(as.ts(x), k = n)/as.ts(x))-1)

  return(ratereturn)
}
