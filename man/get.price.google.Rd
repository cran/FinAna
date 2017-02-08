
\name{get.price.google}
\alias{get.price.google}
\title{Download financial data from google finance}
\usage{
get.price.google(tkr, bg = "2001-01-01",ed = "today")
}
\arguments{
\item{tkr}{:company ticker, e.g. "BABA","AMZN"}

\item{bg}{:beginning date, e.g."2000-02-29"}

\item{ed}{:ending date, e.g. "today", "2016-11-10"}

}
\description{
Download stock prices for one company or a list of companies from google finance. And furthur application of rate of return function and beta function in the package for more analysis.
}
\examples{
#get.price.google("GOOG")
#get.price.google("GOOG", bg = "2001-01-01",ed = "today")
# the two above are the same
#
# tkr <- c("AAPL", "IBM","YHOO")
# pricelist <- get.price.google(tkr, bg = "2001-01-01",ed = "today")
# aapl <- pricelist[1] # convert to single data.frame
# ibm <- pricelist[2] # convert to single data.frame
# yhoo <- pricelist[3] # convert to single data.frame
}

