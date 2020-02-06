## February 2020
## Tcf4 data demo analysis for functions "prepareheatmap" and "heatmapexp"
## Mapping 4-arm experiment

## load libraries (and functions)
library(ntbgraphics)

## get and modify data
data.animal.joined <- getexpdata(paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"))
data.animal.matrix <- prepareheatmap(data.animal.joined)

## print out heatmap
pheatmapout <- heatmapexp(data.animal.matrix, system.file("extdata", package = "ntbgraphics", mustWork = T),
                          title = "Example Data Heatmap")
