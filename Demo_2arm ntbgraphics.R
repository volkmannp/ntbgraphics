## February 2020
## Tcf4 data demo analysis for function "getexpdata" and "ploteachexp"

library(devtools)
library(ntbgraphics)
devtools::load_all()


## get and modify data
data.animal.joined <- getexpdata_2arm(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
## loop through and plot list of experiments
ploteachexp_2arm("Meanspeed", paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
