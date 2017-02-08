##################################################################################################
### Plain Vanilla Bond
##################################################################################################

#' @name bond.price
#' @aliases bond.price
#' @title Calculate the plain vanilla bond price
#' @description Calculate the plain vanilla bond price
#' @usage bond.price(par,c,n,yield,m)
#' @param par :the face value of the bond
#' @param c :the annual coupon rate of the bond
#' @param n :number of years
#' @param yield :the annual yield to maturity of a bond
#' @param m :couponding period in a year
#' @examples #bond.price(1000,0.03,10,0.0248,2)


bond.price <- function(par,c,n,yield,m){

  pmt <- c*par/m
  p <- pmt*((1-((1+yield/m)^(-m*n)))/(yield/2))+(par/((1+yield/m)^(m*n)))

  return(p)

}
