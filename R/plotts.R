##################################################################################################
### Ploting Function Time series
##################################################################################################

## function to create histogram and scatter plots for data.frame:
## c = 0 when there is no dummy varibale in the data.frame
## c = 1 when there is dummy varibale in the data.frame
#' @name plotts
#' @aliases plotts
#' @title Plot time series plots for a data.frame
#' @description Plotting time series plots for a data.frame, with name the graphs and number the graphs.
#' @usage plotts(x,c)
#' @param x :a dataframe
#' @param c :is there dummy variable in the data.frame; c = 0 when there is none; c = 1 when there is
#' @examples #plotts(sp500,0)

plotts <- function(x,c){
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
    ts.plot(x[,i], main = paste("Fig.", paste(i, paste("Time Series Plot of",var[i]))), ylab = var[i], xlab = "")
  }
}
