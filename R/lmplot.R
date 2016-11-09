###############################################################################################
##### Regression Diagnostics Graphs
###############################################################################################

#' @name lmplot
#' @aliases lmplot
#' @title Diagnostics Graphs for a linear regression
#' @description Plotting diagnostic graphs for a fitted object: histogram of the residuals, scatter plot of the fitted values and residuals, scatter plot of fitted values and actual values, time series plot of residuals, and time series plot of actual and fitted values.
#' @usage lmplot(x)
#' @param x :a linear fitted object
#' @param y :the independent variable
#' @examples lmplot(lm)

lmplot <- function(x,y){
  regdf <- as.data.frame(na.omit(cbind(x,y)))
  lmFit <- lm(y~x,regdf)

  hist(lmFit$residuals, probability = T, xlab = "Residuals", ylab = "Frequency", main = "Fig.1 Histogram of Residuals");lines(density(lmFit$residuals))
  plot(lmFit$fitted.values, lmFit$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals v. Fitted Values")
  plot(lmFit$fitted.values, regdf$y, xlab = "Fitted Values", ylab = "Actual", main = "Fitted v. Actual")
  ts.plot(lmFit$residuals, ylab = "Residuals", main = "TS plot of Residuals"); abline(h = 0)
  plot(regdf$y, type = "l",lty = 1, main = "TS plot of Actual and Fitted Values");lines(lmFit$fitted.values, lty = 2)
}
