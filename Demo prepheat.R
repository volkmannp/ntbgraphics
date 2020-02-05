## February 2020
## Tcf4 data demo analysis for function "prepareheatmap"
## Mapping 4-arm experiment

## load libraries (and functions)
library(ntbgraphics)

## get and modify data
data.animal.joined <- getexpdata(paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"))
data.animal.matrix <- prepareheatmap(data.animal.joined)

## map experiments
pheatmap(data.animal.matrix)
