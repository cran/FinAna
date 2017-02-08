###############################################################################################
##### Mode for numeric variables
###############################################################################################

get.mode <- function(x){
    x <- na.omit(x)
    uniquex <- unique(x)
    uniquex[which.max(tabulate(match(x, uniquex)))]
  }
