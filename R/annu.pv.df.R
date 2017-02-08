##################################################################################################
### Deffered Annuity Present Value
##################################################################################################

#' @name annu.pv.df
#' @aliases annu.pv.df
#' @title Calculate present value of annuity
#' @description Calculate present value of an ordinary annuity or an annuity due.
#' @usage annu.pv.df(pmt,i,n,k)
#' @param pmt :the equal amount of payment of each period
#' @param i :interest rate accoding to the period
#' @param n :number of periods
#' @param k :number of periods deffered
#' @examples #annu.pv(100,0.0248,10,4,0)


annu.pv.df <- function(pmt,i,n,k){

    pv <- pmt*((1-((1+i)^-n))/i)*((1+i)^-k)
    round(pv,2)

  return(pv)
}
