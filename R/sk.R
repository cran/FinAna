###############################################################################################
##### Skewness
###############################################################################################

#' @name sk
#' @aliases sk
#' @title Calculating skewness for numeric data
#' @description Calculating Pearson's skewness in three types: mode, median, and mean
#' @usage sk(x, type = 3)
#' @param x :a numeric variable
#' @param type :type = 1 for mode skewness; type = 2 for median skewness; type = 3 for mean skewness
#' @examples #sk(return) for skewness of variable return


sk <- function(x, type=3){
  me <- mean(x,na.rm = T)
  med <- median(x, na.rm = T)
  mo <- get.mode(x)
  std <- sd(x,na.rm = T)
  if(type == 1){
    skewness <- (me-mo)/std
  }else if(type == 2){
    skewness <- 3*(me-med)/std
  }else{
    above <- mean((x - me)^3, na.rm = T)
    below <- ((sum((x-me)^2, na.rm = T)/(length(x)-1))^(3/2))
    skewness <- above/below
  }
  return(skewness)
}
