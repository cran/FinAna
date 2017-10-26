##################################################################################################
### Ploting Function
##################################################################################################

## function to create histogram and scatter plots for data.frame:
## c = 0 when there is no dummy varibale in the data.frame
## c = 1 when there is dummy varibale in the data.frame
#' @name ploth
#' @aliases ploth
#' @title Plot histograms for a data.frame
#' @description Plotting histograms for a data.frame. Also the function will name the graphs and number the graphs.
#' @usage ploth(x,c,l)
#' @param x :a dataframe
#' @param c :is there dummy variable in the data.frame; c = 0 when there is none; c = 1 when there is
#' @param l : number of labeling starts at (default = 1)
#' @examples #ploth(sp500,0,20) for histograms of sp500 which does not has dummy variables


ploth <- function(x,c,l = 1){
  typeofvar <- sapply(x,class)
  ha <- typeofvar[typeofvar == "numeric"]
  if(c == 1){
    dumornot <- typeofvar[typeofvar == "integer"]
    dummyvar <- x[,c(names(dumornot))]
    dummyvar <- as.data.frame(dummyvar[!unique(dummyvar) == 0 & !unique(dummyvar) == 1])
    if(dim(dummyvar)[1] > 0){
    ha <- c(dumornot,ha)}
    }
  x <- x[,c(names(ha))]
  var <- names(x)
  n <- length(var)
    for(i in 1:n){
      hist(x[,i], main = paste("Fig.", paste(l+i-1, paste("Histogram of",var[i]))), xlab = var[i])
    }
}
