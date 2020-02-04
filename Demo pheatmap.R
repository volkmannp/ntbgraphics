## February 2020
## Tcf4 data demo analysis for function "___(pheatmap)"
## Heatmap 4-arm experiment

## load libraries (and functions)
library(ntbgraphics)

## get and modify data
data.animal.joined <- getexpdata(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
data.animal.SerialLearn <- data.matrix(data.animal.joined)
