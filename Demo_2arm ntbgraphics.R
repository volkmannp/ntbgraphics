## February 2020
## Tcf4 data demo analysis for function "getexpdata_2arm" and "ploteachexp_2arm"
## Plotting single experiments

## load libraries (and functions)
library(devtools)
library(ntbgraphics)
devtools::load_all()

## get and modify data
data.animal.joined <- getexpdata_2arm(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))

##  plot single experiments
ploteachexp_2arm("Meanspeed", paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
