###############################################################################################
##### kurtosis
###############################################################################################

#' @name kur
#' @aliases kur
#' @title Calculating kurtosis for numeric data
#' @description Kurtosis
#' @usage kur(x)
#' @param x :a numeric variable
#' @examples kur(return) for skewness of variable return


kur <- function(x){
  x <- na.omit(x)
  me <- mean(x,na.rm = T)
  med <- median(x, na.rm = T)
  std <- sd(x,na.rm = T)
  kurtosis <- mean((x - me)^4)/(mean((x-me)^2)^2)
  return(kurtosis)
}
