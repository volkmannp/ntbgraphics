pcatsneexp <- function(directory, analysis = c("4arm", "2arm_tg", "2arm_ko"), perplex, theta = 0.5,
                       pastetitle = "PCA", pastetitle2 = "tSNE") {
  
  ### get tables
  data.animal.joined <- getexpdata(directory)
  data.animal.matrix <- data.animal.joined %>%
    column_to_rownames(., "RFID") %>%
    select(Meanspeed:SerialLearn) %>%
    data.matrix(.) %>%
    na.omit %>%
    scale()
  data.animal.list <- data.animal.joined %>%
    na.omit %>%
    `if`(analysis == "4arm", select(., RFID, GT_Env, Meanspeed:SerialLearn),.) %>%
    `if`(analysis == "2arm_tg", select(., RFID, Genotype, Meanspeed:SerialLearn),.) %>%
    `if`(analysis == "2arm_ko", select(., RFID, Genotype, Meanspeed:SerialLearn),.) %>%
    data.frame(.) %>%
    `if`(analysis == "4arm",
         mutate(., GT_Env = factor(GT_Env, levels = c("wt_hc", "wt_sd", "tg_hc", "tg_sd"))),.) %>%
    `if`(analysis == "2arm_tg",mutate(., Genotype = factor(Genotype)),.) %>%
    `if`(analysis == "2arm_ko",mutate(., Genotype = factor(Genotype)),.)
  
  
  ### PCA
  ## perform PCA
  pca_analysis <- prcomp(data.animal.matrix)
  
  ## prepare plotting
  pca_analysis_plot <- cbind(data.animal.list, pca_analysis$x)
  if (analysis == "4arm") {
    rownames(pca_analysis$x) <- data.animal.list$GT_Env
  }
  
  
  ## plot PCA
  pca_plot <- ggplot(pca_analysis_plot, aes(x = PC1, y = PC2, color= data.animal.list$GT_Env)) +
    theme_bw() +
    theme_bw() +
    # customize title position and size
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 16)) +
    # all elements blank
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),
          # customize axes
          axis.line.y = element_line(colour = "black", size=0.6),
          axis.line.x = element_line(colour = "black", size=0.6),
          # axis.ticks.x = element_line(colour = "black", size=0.6),
          axis.text.x = element_text(angle=0, size=10),
          axis.text.y = element_text(angle=0, size=10),
          text = element_text(size=14),
          # customize legend
          legend.text = element_text(size=9),
          legend.title = element_text(size=12)) +
    # colors of points, size and title
    scale_color_manual(values = c("#b4b4b4", "#3c3c3c", "#84dcff", "#1e24fc")) +
    geom_point(size = 2) +
    labs(color = "Legend") + 
    ggtitle(pastetitle)
  print(pca_plot)
  
  pca_plot_ellipse <- ggbiplot(pca_analysis, ellipse=TRUE, color = data.animal.list$GT_Env,
                               groups= data.animal.list$GT_Env) +
    theme_bw() +
    # customize title position and size
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 16)) +
    # all elements blank
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),
          # customize axes
          axis.line.y = element_line(colour = "black", size=0.6),
          axis.line.x = element_line(colour = "black", size=0.6),
          # axis.ticks.x = element_line(colour = "black", size=0.6),
          axis.text.x = element_text(angle=0, size=10),
          axis.text.y = element_text(angle=0, size=10),
          text = element_text(size=14),
          # customize legend
          legend.text = element_text(size=9),
          legend.title = element_text(size=12)) +
    # colors of points, size and title
    scale_color_manual(values = c("#b4b4b4", "#3c3c3c", "#84dcff", "#1e24fc")) +
    labs(color = "Legend") + 
    ggtitle(paste(pastetitle, "Ellipse"))
  print(pca_plot_ellipse)
  
  
  ### tSNE
  ## perform tSNE
  tsne_analysis <- Rtsne(data.animal.matrix, check_duplicates = FALSE, pca = FALSE, 
                         perplexity = perplex,
                         theta = theta, dims=2)
  
  ## prepare plotting
  d_tsne_analysis <- as.data.frame(tsne_analysis$Y)
  tsne_analysisplot <- cbind(data.animal.list, d_tsne_analysis)
  
  ## plot tSNE
  tsne_plot <- ggplot(tsne_analysisplot, aes(x=V1, y=V2, color = data.animal.list$GT_Env,)) +
    theme_bw() +
    labs(fill = "Legend") +
    # customize title position and size
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 16)) +
    # all elements blank
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),
          # customize axes
          axis.line.y = element_line(colour = "black", size=0.6),
          axis.line.x = element_line(colour = "black", size=0.6),
          # axis.ticks.x = element_line(colour = "black", size=0.6),
          axis.text.x = element_text(angle=0, size=10),
          axis.text.y = element_text(angle=0, size=10),
          text = element_text(size=14),
          # customize legend
          legend.text = element_text(size=9),
          legend.title = element_text(size=12)) +
    # colors of points, size and title
    scale_color_manual(values = c("#b4b4b4", "#3c3c3c", "#84dcff", "#1e24fc")) +
    geom_point(size = 2) +
    labs(color = "Legend") + 
    ggtitle(pastetitle2)
  #tsne_plot <-  tsne_plot + scale_fill_discrete(name = "New Legend Title")
  print(tsne_plot)
  
  return(list(pca_analysis, tsne_analysis))
}


pcatsneexp(directory = "/Users/paul/Documents/Einführung R/Wahlfach", analysis = "2arm_tg", perplex = 18)
