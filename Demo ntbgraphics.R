### February 2020
### Paul Volkmann
### Tcf4 data demo analysis for functions of "ntbgraphics"

## clear workspace and load libraries (and functions)
rm(list = ls(all.names = TRUE))
library(ntbgraphics)

##
## Please note: Each function works independently of what the user might have run before;
##    - the function'ploteachexp' refers internally to 'getexpdata' without the user having to run it first;
##    - analogical, 'loopplotexp' refers internally to the former without the user having to run them first!
##    - surprisingly, the same holds true for 'heatmapexp'
##

## (getexpdata) get modified table with data
data.animal.joined <- getexpdata(directory = paste0(system.file("extdata/", package = "ntbgraphics",
                                                                mustWork = T),"/"), analysis = "4arm")

## (ploteachexp) plot a defined experiment
ploteachexp(expname = "Meanspeed",
            directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
            analysis = "4arm",
            orderplots = "tcf4",
            saveplotdir = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"))

## (loopplotexp) plot all experiments
data.animal.matrix <- loopplotexp(
            directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
            analysis = "4arm",
            orderplots = "tcf4",
            saveplotdir = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"))

## (heatmapexp) print out heatmap
data.animal.matrix <- heatmapexp(directory = paste0(system.file("extdata", package = "ntbgraphics",
                                                                mustWork = T),"/"),
                                 analysis = "4arm",
                                 title = "Example Data Heatmap")

#################
# saving all boxplots in one file (elegant way)
