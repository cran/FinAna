###############################################################################################
##### Beta for companies
###############################################################################################

#' @name betaf
#' @aliases betaf
#' @title Calculating beta for a company or a select of companies
#' @description Calculating beta using common method or linear regression(OLS)
#' @usage betaf(x,y,method)
#' @param x :a vector or a data.frame of rate of return of companies
#' @param y :name of the independent variable
#' @param method :method of calculation; method = 1 for a common expression of beta(see detail); method = 2 using linear regression to estimate the beta
#' @examples #betaf(appl,sp500)


betaf <- function(x,y,method){

  dependent <- x[,y]
  typeofvar <- sapply(x,class)
  ha <- typeofvar[typeofvar == "numeric" & !names(typeofvar) == y]
  x <- x[,c(names(ha))]

  if(method == 1){

    n <- dim(x)[2]
    varr <- var(dependent,na.rm = T)
    betare <- as.data.frame(matrix(nrow = n, ncol = 2))
    betare[,1] <- names(x)
    for (i in 1:n){
      covdf <- as.data.frame(cbind(x[,i],dependent))
      covdf <- as.data.frame(na.omit(covdf))
      covv <- cov(covdf[1], covdf[2], use = "na.or.complete")
      betare[i,2] <- covv/varr
    }
  }else{
    n <- dim(x)[2]
    betare <- as.data.frame(matrix(nrow = n, ncol = 2))
    betare[,1] <- names(x)

    for(i in 1:n){
      rrco <- x[,i]
      rdf <- as.data.frame(cbind(rrco, dependent))

      lmFit <- lm(dependent~rrco, rdf, na.omit = na.action)
      betare[i,2] <- lmFit$coefficients[2]
    }

  }
  return(betare)
}
