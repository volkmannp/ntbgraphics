## February 2020
## Tcf4 data demo analysis for function "getexpdata" and "ploteachexp"

library(ntbgraphics)

## get and modify data
data.animal.joined <- getexpdata("/Users/paul/Documents/Einführung R/Wahlfach")

## loop through and plot list of experiments
myexp <- c(as.list(colnames(data.animal.joined[, -(1:2)])))
lapply(myexp, ntbgraphics::ploteachexp)
