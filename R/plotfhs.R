##################################################################################################
### Ploting Function
##################################################################################################

## function to create histogram and scatter plots for data.frame:
## a = 0 for both
## a = 1 for histogram
## a = 2 for scatter plots
## dependent is the column name of the dependent variable
## c = 0 when there is no dummy varibale in the data.frame
## c = 1 when there is dummy varibale in the data.frame
#' @name ploths
#' @aliases ploths
#' @title Plot histograms and scatter plots for a data.frame
#' @description Plotting histograms or scatter plots of your choice for a data.frame. Also the function will name the graphs and number them.The purpose of the function is to save time when plotting graphs for a regression analysis or other usage. The function can plot, name and number the graphs at one step.
#' @usage ploths(x,a,independent,c)
#' @param x :a dataframe
#' @param a :the type of graph you want; a = 1 for histograms; a = 2 for scatter plots; a = 0 for both
#' @param dependent :the independent variable for scatterplots
#' @param c :is there dummy variable in the dataframe; c = 0 when there is none; c = 1 when there is
#' @examples ploths(sp500,0,"price",0)


ploths <- function(x,a,dependent,c){

  dependent <- x[,dependent]
  typeofvar <- sapply(x,class)
  ha <- typeofvar[typeofvar == "numeric" & !names(typeofvar) == dependent]
  if(c == 1){
  dumornot <- typeofvar[typeofvar == "integer"]
  dummyvar <- x[,c(names(dumornot))]
  dummyvar <- dummyvar[!unique(dummyvar) == 0 & !unique(dummyvar) == 1]
  ha <- c(ha,dummyvar)
  }
  x <- x[,c(names(ha))]
  var <- names(x)
  n <- length(var)

  if (a == 1){
    hist(dependent, main = paste("Fig.", paste(1, paste("Histogram of",dependent))), xlab = dependent)
    for(i in 1:n){
      hist(x[,i], main = paste("Fig.", paste(i+1, paste("Histogram of",var[i]))), xlab = var[i])
    }
  }else if(a == 2){
    for(i in 1:n){
      plot(x[,i], dependent, main = paste("Fig.", paste(i, paste("Scatterplot of",var[i]))), xlab = var[i], ylab = dependent)
    }
  }else{
    hist(dependent, main = paste("Fig.", paste(1, paste("Histogram of",dependent))), xlab = dependent)
    for(i in 1:n){
      hist(x[,i], main = paste("Fig.", paste(i+1, paste("Histogram of",var[i]))), xlab = var[i])}
    for(i in 1:n){
      plot(x[,i], dependent, main = paste("Fig.", paste(i+n+1, paste("Scatterplot of",var[i]))), xlab = var[i], ylab = dependent)
      }
  }
}
