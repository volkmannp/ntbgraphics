data.animal.matrix <- heatmapexp(directory = "/Users/paul/Documents/Einführung R/Wahlfach/",
                                 analysis = "4arm",
                                 title = "Example Data Heatmap")

tsne_tcf4 <- Rtsne(data.animal.matrix, check_duplicates=FALSE, pca=TRUE, perplexity=18, theta=0.5, dims=2)

d_tsne_tcf4 = as.data.frame(tsne_tcf4$Y)

ggplot(d_tsne_tcf4, aes(x=V1, y=V2)) +
  geom_point(size=0.75) +
  ggtitle("t-SNE Tcf4")
