###############################################################################################
##### Download financial data from google
###############################################################################################


get.price.google <- function (tkr, bg = "2001-01-01", ed = "today"){
  n <- length(tkr)
  pricelist <- list()

  bgmth <- month.abb[as.numeric(substring(bg, 6,7))]
  bgday <- substring(bg,9,10)
  bgyear <- substring(bg,1,4)
  if (ed != "today") {
    edmth <- month.abb[as.numeric(substring(ed, 6,7))]
    edday <- substring(ed,9,10)
    edyear <- substring(ed,1,4)
  }
  else {
    ed <- as.character(Sys.Date())
    edmth <- month.abb[as.numeric(substring(ed, 6,7))]
    edday <- substring(ed,9,10)
    edyear <- substring(ed,1,4)
  }
  for (i in 1:n) {
    p <- read.csv(paste0("https://www.google.com/finance/historical?q=", tkr[i], "&output=csv", "&startdate=", bgmth, "+", bgday, "+", bgyear, "&enddate=", edmth, "+", edday, "+", edyear))
    names(p) <- c("date", "open", "high", "low", "close", "volume")
    p$obs <- 1 : length(p$open)
    p <- p[sort(p$obs, decreasing = T),]
    p <- p[,1:6]
    p$obs <- 1 : length(p$open)
    }
    pricelist[[tkr[i]]] = p
  return(pricelist)
}
