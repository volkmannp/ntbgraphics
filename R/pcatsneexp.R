#' @title Perform and plot PCA and tSNE of NTB experiment dataset
#'
#' @author Paul Volkmann
#'
#' @name pcatsneexp
#'
#' @description A function that produces cluster plots for results of Principal Component Analysis and
#' t-distributed Stochastic Neighbor Embedding of a z-scored NTB dataset
#' and returns a list containing the results (see examples for easy way to access individually).
#' (Requires function 'getexpdata' internally.)
#' For right formatting of your files, please consider the "ReadMe for ntbgraphics".
#'
#' @param 'directory': file directory of Behavior and Animal List files
#' @param 'analysis': specifying the kind of experiment performed - 4-arm or 2-arm
#' with either transgenic or knock-out animals as group of interest
#' (or choosing the kind of analysis preferred)
#' (default: "4arm")
#' @param 'perplex':  define perplexity parameter (should not be bigger than >[nrow(matrix] - 1)/3<)
#' @param 'theta': define theta in the range of 0 to 1 (speed/accuracy trade-off;
#' increase for less accuracy, set to 0 for exact TSNE)
#' (default: 0.5)
#' @param 'pastetitle': customize title of PCA plots; title for PCA will be "pastetitle",
#' title for PCA with ellipse will be "pastetitle Ellipse"
#' (default: "PCA")
#' @param 'pastetitle2': define title of tSNE plot
#' (default: "tSNE")
#'
#' @return three cluster plots and a list containing the results
#'
#' @export
#'
#' @example
#' pcatsneexp(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"), perplex = 18)
#'
#' results <- pcatsneexp(
#' directory = paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"),
#' analysis = "4arm",
#' perplex =  10,
#' theta = 0.8,
#' pastetitle = "new_testdata_pca_09-04-2044",
#' pastetitle2 = "new_testdata_tsne_09-04-2044")
#'
#' results_pca <- results[[1]]
#' results_tsne <- results[[2]]

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
    select(RFID:GT_Env) %>%
    data.frame(.) %>%
    `if`(analysis == "4arm",
         mutate(., GT_Env = factor(GT_Env, levels = c("wt_hc", "wt_sd", "tg_hc", "tg_sd"))),.) %>%
    `if`(analysis == "2arm_tg",mutate(., GT_Env = factor(GT_Env)),.) %>%
    `if`(analysis == "2arm_ko",mutate(., GT_Env = factor(GT_Env)),.)


  ### PCA
  ## perform PCA
  pca_analysis <- prcomp(data.animal.matrix)

  ## prepare plotting
  pca_analysis_plot <- cbind(data.animal.list, pca_analysis$x)
  rownames(pca_analysis$x) <- data.animal.list$GT_Env

  ## plot PCA
  pca_plot <- ggplot(pca_analysis_plot, aes(x = PC1, y = PC2, color= data.animal.list$GT_Env)) +
    scale_color_manual(values = c("#ABDDA4", "#2B83BA", "#FDAE61", "#D7191C")) +
    geom_point(size = 2) +
    ggtitle(pastetitle)
  print(pca_plot)

  pca_plot_ellipse <- ggbiplot(pca_analysis, ellipse=TRUE, color = data.animal.list$GT_Env,
                               groups= data.animal.list$GT_Env)+
    scale_color_manual(values = c("#ABDDA4", "#2B83BA", "#FDAE61", "#D7191C")) +
    ggtitle(paste(pastetitle, "Ellipse"))
  print(pca_plot_ellipse)


  ### tSNE
  ## perform tSNE
  tsne_analysis <- Rtsne(data.animal.matrix, check_duplicates = FALSE, pca = FALSE, perplexity = perplex,
                     theta = theta, dims=2)

  ## prepare plotting
  d_tsne_analysis <- as.data.frame(tsne_analysis$Y)
  tsne_analysisplot <- cbind(data.animal.list, d_tsne_analysis)

  ## plot tSNE
  tsne_plot <- ggplot(tsne_analysisplot, aes(x=V1, y=V2, color = data.animal.list$GT_Env,)) +
    scale_color_manual(values = c("#ABDDA4", "#2B83BA", "#FDAE61", "#D7191C")) +
    geom_point(size = 2) +
    ggtitle(pastetitle2)
  print(tsne_plot)

  return(list(pca_analysis, tsne_analysis))
}
