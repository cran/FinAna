##################################################################################################
### Annuity Future Value
##################################################################################################

#' @name annu.fv
#' @aliases annu.fv
#' @title Calculate future value of annuity
#' @description Calculate future value of an ordinary annuity or an annuity due.
#' @usage annu.fv(pmt,i,n,type = 0)
#' @param pmt :the equal amount of payment of each period
#' @param i :interest rate accoding to the period
#' @param n :number of periods
#' @param type :type = 0 for ordinary annuity, type = 1 for annuity due
#' @examples #annu.fv(100,0.0248,10,0)


annu.fv <- function(pmt,i,n,type = 0){

  if(type == 1){

    fv <- pmt*((((1+i)^n)-1)/i)*(1+i)
  }else{

    fv <- pmt*((((1+i)^n)-1)/i)
  }

  return(fv)
}
