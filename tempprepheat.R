## February 2020
## Tcf4 data demo analysis for function "___(pheatmap)"
## Heatmap 4-arm experiment

## load libraries (and functions)
library(ntbgraphics)

## get and modify data
data.animal.matrix <- getexpdata("/Users/paul/Documents/Einführung R/Wahlfach/") %>%
  column_to_rownames(., "RFID") %>%
  select(Meanspeed:SerialLearn) %>%
  data.matrix() %>%
  na.omit %>%
  scale()

pheatmap(data.animal.matrix)
