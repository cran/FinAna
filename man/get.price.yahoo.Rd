
\name{get.price.yahoo}
\alias{get.price.yahoo}
\title{Download financial data from Yahoo finance}
\usage{
get.price.yahoo(tkr, bg = "first",ed = "today", f = "d")
}
\arguments{
\item{tkr}{:company ticker, e.g. "BABA","AMZN"}

\item{bg}{:beginning date, e.g. "first","2000-02-29"}

\item{ed}{:ending date, e.g. "today", "2016-11-10"}

\item{f}{:frequency, e.g. "d" for daily,"w" for weekly,"m" for monthly}
}
\description{
Download stock prices for one company or a list of companies from Yahoo finance. The function can download daily, weekly and monthly data. And furthur application of rate of return function and beta function in the package for more analysis.
}
\examples{
#get.price.yahoo("GOOG")
#get.price.yahoo("GOOG", bg = "first",ed = "today", f = "d")
# the two above are the same
#
# tkr <- c("AAPL", "IBM","YHOO")
# pricelist <- get.price.yahoo(tkr, bg = "first",ed = "today", f = "m")
# aapl <- pricelist[1] # convert to single data.frame
# ibm <- pricelist[2] # convert to single data.frame
# yhoo <- pricelist[3] # convert to single data.frame
}

