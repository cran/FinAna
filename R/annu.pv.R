##################################################################################################
### Annuity Present Value
##################################################################################################

#' @name annu.pv
#' @aliases annu.pv
#' @title Calculate present value of annuity
#' @description Calculate present value of an ordinary annuity or an annuity due.
#' @usage annu.pv(pmt,i,n,type = 0)
#' @param pmt :the equal amount of payment of each period
#' @param i :interest rate accoding to the period
#' @param n :number of periods
#' @param type :type = 0 for ordinary annuity, type = 1 for annuity due
#' @examples #annu.pv(100,0.0248,10,0)


annu.pv <- function(pmt,i,n,type = 0){

  if(type == 1){

    pv <- pmt*((1-((1+i)^-n))/i)*(1+i)
  }else{

    pv <- pmt*((1-((1+i)^-n))/i)
  }

  return(pv)
}
