##################################################################################################
### Ploting Function scatter smooth
##################################################################################################


plotsm <- function(x,dependent,c){
  x <- na.omit(x)
  yl <- dependent
  dependent <- x[,dependent]
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
  x <- as.data.frame(na.omit(x))
  for(i in 1:n){
    scatter.smooth(x[,i],dependent, main = paste("Fig.", paste(i, paste("Scatter Plot of",var[i]))), xlab = var[i],ylab = yl)
  }
}
