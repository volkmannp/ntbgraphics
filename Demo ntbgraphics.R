## February 2020
## Tcf4 data demo analysis for function "getexpdata" and "ploteachexp"

library(devtools)
library(ntbgraphics)
devtools::load_all()

## get and modify data
data.animal.joined <- getexpdata(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))

## loop through and plot list of experiments
myexp <- c(as.list(colnames(data.animal.joined[, -(1:2)])))
lapply(myexp, ntbgraphics::ploteachexp, paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
