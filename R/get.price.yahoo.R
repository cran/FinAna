###############################################################################################
##### Download financial data from yahoo
###############################################################################################


get.price.yahoo <- function (tkr, bg = "first", ed = "today", f = "d"){
  num <- length(tkr)
  pricelist <- list()
  if (bg == "first" & ed == "today") {
    for (i in 1:num) {
      p <- read.csv(paste0("http://ichart.finance.yahoo.com/table.csv?s=", tkr[i]))
      p <- p[order(p$Date),]
      names(p) <- c("date", "open", "high", "low", "close", "volume", "adjusted")
      p$obs <- c(1:length(p[,1]))
      pricelist[[tkr[i]]] <- p
    }
  }else{
    bgmth <- as.character(as.numeric(substring(bg,6,7))-1)
    bgday <- substring(bg,9,10)
    bgyear <- substring(bg,1,4)
    if (ed != "today") {
      edmth <- as.character(as.numeric(substring(ed,6,7))-1)
      edday <- substring(ed,9,10)
      edyear <- substring(ed,1,4)
    }else{
      ed <- as.character(substring(Sys.time(),1,10))
      edmth <- substring(ed,6,7)
      edday <- substring(ed,9,10)
      edyear <- substring(ed,1,4)
    }
    for (i in 1:num) {
      p <- read.csv(paste0("http://ichart.finance.yahoo.com/table.csv?s=", tkr[i], "&a=", bgmth, "&b=", bgday, "&c=", bgyear, "&d=", edmth, "&e=", edday, "&f=", edyear, "&g=", f))
      p <- p[order(p$Date),]
      names(p) <- c("date", "open", "high", "low", "close", "volume", "adjusted")
      p$obs <- c(1:length(p[,1]))
      pricelist[[tkr[i]]] <- p
    }
  }
  return(pricelist)
}
