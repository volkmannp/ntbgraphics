### February 2020
### Paul Volkmann
### Tcf4 PCA and tSNE data analysis

library(ntbgraphics)
library(Rtsne)
library(ggfortify)
library(ggbiplot)

## get tables
data.animal.joined <- getexpdata(directory = "/Users/paul/Documents/Einführung R/Wahlfach/")
data.animal.matrix <- heatmapexp(directory = "/Users/paul/Documents/Einführung R/Wahlfach/",
                                 analysis = "4arm",
                                 title = "Example Data Heatmap")
data.animal.list <- data.animal.joined %>%
  na.omit %>%
  select(RFID:GT_Env) %>%
  data.frame()


### PCA
## perform PCA
pca_tcf4 <- prcomp(data.animal.matrix)

## prepare plotting
pca_tcf4_plot <- cbind(data.animal.list, pca_tcf4$x)

## plot PCA
ggbiplot(pca_tcf4, ellipse=TRUE, color = data.animal.list$GT_Env, groups=data.animal.list$GT_Env) +
  scale_color_manual(values = c("#00FF00", "#FF0000", "#FFFF00", "#0000FF")) +
  geom_point(size = 2) +
  ggtitle("PCA Tcf4 Ellipse")

ggplot(pca_tcf4_plot, aes(x = PC1, y = PC2, color= GT_Env)) +
  scale_color_manual(values = c("#00FF00", "#FF0000", "#FFFF00", "#0000FF")) +
  geom_point(size = 2) +
  ggtitle("PCA Tcf4")


### tSNE
## perform tSNE
tsne_tcf4 <- Rtsne(data.animal.matrix, check_duplicates=FALSE, pca=FALSE, perplexity=18, theta=0.5, dims=2)

## prepare plotting
d_tsne_tcf4 <- as.data.frame(tsne_tcf4$Y)
tsne_tcf4plot <- cbind(data.animal.list, d_tsne_tcf4)

## plot tSNE
ggplot(tsne_tcf4plot, aes(x=V1, y=V2, color=GT_Env)) +
  scale_color_manual(values = c("#00FF00", "#FF0000", "#FFFF00", "#0000FF")) +
  geom_point(size = 2) +
  ggtitle("t-SNE Tcf4")
