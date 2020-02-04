## February 2020
## Tcf4 data demo analysis for function "getexpdata" and "ploteachexp"
## Plotting 4-arm experiments (by loop)

## load libraries (and functions)
library(ntbgraphics)

## get and modify data
data.animal.joined <- getexpdata(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))

## loop through and plot list of experiments
myexp <- c(as.list(colnames(data.animal.joined[, -(1:2)])))
lapply(myexp, ploteachexp, paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"))
lapply(myexp, ploteachexp, "/Users/paul/Documents/Github_Repo/ntbgraphics")
