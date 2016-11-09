###############################################################################################
##### Descriptive statistics
###############################################################################################

#' @name desc
#' @aliases desc
#' @title Descriptice statistics of a data.frame
#' @description Calculating the descriptive statistics of a data.frame and exporting in a data.frame
#' @usage desc(x)
#' @param x :a data.frame
#' @examples desc(sp1500) for descriptive statistics of sp1500

desc <- function(x){

  typeofvar <- sapply(x,class)
  ha <- names(typeofvar[typeofvar == "numeric" | typeofvar == "integer"])
  x <- x[,c(ha)]

  des <- as.data.frame(matrix(nrow = dim(x)[2], ncol = 9))
  names(des) <- c("name","obs","mean","median","mode","var","std","skew","kurt")
  des[,1] <- names(x)
  des[,2] <- apply(x,2, length)
  des[,3] <- apply(x,2,mean, na.rm = T)
  des[,4] <- apply(x,2,median, na.rm = T)
  des[,5] <- apply(x,2,findmode)
  des[,6] <- apply(x,2,var, na.rm = T)
  des[,7] <- apply(x,2,sd, na.rm = T)
  des[,8] <- apply(x,2,sk)
  des[,9] <- apply(x,2,kur)
  return(des)
}
