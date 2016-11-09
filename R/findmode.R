###############################################################################################
##### Mode for numeric variables
###############################################################################################

#' @name findmode
#' @aliases findmode
#' @title Calculating mode for numeric data
#' @description Calculating mode for numeric data
#' @usage findmode(x)
#' @param x :a numeric variable(vector)
#' @examples getmode(return) for mode of variable return

findmode <- function(x){
    x <- na.omit(x)
    uniquex <- unique(x)
    uniquex[which.max(tabulate(match(x, uniquex)))]
  }
