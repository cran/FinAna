###############################################################################################
##### Descriptive statistics
###############################################################################################

#' @name desc
#' @aliases desc
#' @title Descriptice statistics of a data.frame
#' @description Calculating the descriptive statistics of a data.frame and exporting in a data.frame
#' @usage desc(x,n)
#' @param x :a data.frame
#' @param n :number of decimal points
#' @examples #desc(sp1500,3) for descriptive statistics of sp1500

desc <- function(x,n){

  typeofvar <- sapply(x,class)
  ha <- names(typeofvar[typeofvar == "numeric" | typeofvar == "integer"])
  x <- x[,c(ha)]

  des <- as.data.frame(matrix(nrow = dim(x)[2], ncol = 11))
  names(des) <- c("name","obs","max","min","mean","median","mode","var","std","skew","kurt")
  des[,1] <- names(x)
  des[,2] <- round(apply(x,2, length), n)
  des[,3] <- round(apply(x,2, max, na.rm = T), n)
  des[,4] <- round(apply(x,2, min, na.rm = T), n)
  des[,5] <- round(apply(x,2,mean, na.rm = T), n)
  des[,6] <- round(apply(x,2,median, na.rm = T), n)
  des[,7] <- round(apply(x,2,get.mode), n)
  des[,8] <- round(apply(x,2,var, na.rm = T), n)
  des[,9] <- round(apply(x,2,sd, na.rm = T), n)
  des[,10] <- round(apply(x,2,sk), n)
  des[,11] <- round(apply(x,2,kur), n)
  return(des)
}
