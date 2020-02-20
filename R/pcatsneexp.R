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
#' @param directory specifies file directory of 'Meta Behavior' and 'Animal List' files within quotation 
#' marks (mind correct spelling of both files and 'directory'!);
#' no default
#' @param analysis specifies the kind of experiment performed within quotation marks;
#' "2arm_ko","2arm_tg", "2arm_sd", "2arm_treat",
#' "4arm_sd_ko", "4arm_sd_tg", "4arm_treat_ko", "4arm_treat_tg"
#' (tg for transgenic, ko for knockout;
#' 4arm_sd_x assumes a stress paradigm with social defeat (sd) and housing or handling control (hc) as 
#' control;
#' 4arm_treat_x assumes a treatment paradigm with treated (treat) and untreated (untreat) animals;
#' 2arm_x assumes wildtype controls (wt) for tg and ko, housing or handling controls (hc) for sd and
#' untreated controls (untreat) for treated animals;
#' ('analysis' defines the kind of experiment performed, respectively the kind of analysis preferred - 
#' you can easily perform 2arm analysis for 4arm experiments looking only at the groups of interest, 
#' but not the other way around);
#' default: "2arm_ko"
#' @param ordercolumns defines the order paradigm of experiment column appearance in internal table within 
#' quotation marks: "ntb", "rdoc", "manual";
#' order of experiments may be customized manually with "manual" (-> use 'ordercolumns_manual' for exact 
#' appearance; there, you may also choose to exclude experiments - this may be the only useful application 
#' of this parameter in this functions' context);
#' default: "ntb"
#' @param ordercolumns_manual customizes order of appearance and especially appearance itself of experiment 
#' columns in internal table and final analysis (experiments that are not listed will not be included);
#' only if 'ordercolumns' = "manual";
#' user has to provide a vector containing characters within quotation marks (e.g. by using 
#' c("Meanspeed", "SerialLearn")) with all experiments he wants to include into the final analysis;
#' no need for specification if 'ordercolumns' is not "manual"
#' default: FALSE
#' @param exclude.animals excluding animals from analysis by RFID;
#' user has to provide a vector containing characters within quotation marks (e.g. by using 
#' c("900200000067229", "900200000065167")) with all animals he wants to exclude from the final analysis;
#' if FALSE is provided, no animal will be excluded;
#' default: FALSE
#' @param orderlevelcond defines order of factor levels of conditions within quotation marks:
#' "other", "gtblock", "etblock", "2rev";
#' definition of order of groups in legend:
#' "other" for alphabetical order in case of 4arm; also for default order of 2arm experiments
#' (which lists the 'control' first, then the 'condition');
#' "gtblock" for order wt_x, wt_y, tg_x, tg_y;
#' "etblock" for order x_hc, y_hc, x_sd, y_sd;
#' "2rev" for inverse order of 2arm default only, meaning listing the 'condition' first, then the 'control';
#' default: "other"
#' @param return.matrix,mean boolean that specifies if matrix and following analysis should only consist of 
#' the mean of each group for each experiment; grouping follows specification of groups to be analyzed as 
#' defined by 'analysis';
#' default: FALSE
#' @param directional boolean that specifies if directionality paradigm following RDoC concept should be
#' applied; if TRUE, columns 'Rotations', 'FreezeBase', 'Timeimmobile', 'Baseline', 'Activity', 'Choices' and
#' 'Meanspeed' are multiplied by -1; only useful if 'absoluteval' is FALSE;
#' default: FALSE
#' @param absoluteval boolean that specifies if only absolute values of z-scored matrix should be given and
#' analyzed;
#' default: FALSE
#' @param perplex defines perplexity parameter (should not be bigger than >[nrow(matrix] - 1)/3<);
#' default: 1
#' @param theta defines theta in the range of 0 to 1 (speed/accuracy trade-off;
#' increase for less accuracy, set to 0 for exact TSNE);
#' default: 0.5
#' @param ellipse_pca boolean that defines if conficdence interval ellipses should be displayed in standard
#' PCA plot;
#' default: FALSE
#' @param ellipse_tsne boolean that defines if conficdence interval ellipses should be displayed in tSNE plot;
#' default: FALSE
#' @param ellconf defines confidence level for ellipses in all plots (where applicable) between 0 and 1:
#' default: 0.75
#' @param ellalpha defines transparency of ellipses in all plots (where applicable); takes values between 0 
#' for high transparency/low intensity and 1 for low transparency/high intensity of filling:
#' default: 0.2
#' @param pastetitle customizes title of PCA plots within quotation marks; title for PCA will be "pastetitle",
#' title for PCA with ellipse will be "pastetitle Ellipse";
#' default: "PCA"
#' @param pastetitle2 defines title of tSNE plot within quotation marks;
#' default: "tSNE"
#' @param saveplotdir file directory where to save plots within quotation marks;
#' you may set to FALSE if you do not want to save plot to PDF;
#' default: location of Behavior and Animal List files as specified in 'directory'
#' 
#' @return three cluster plots (two for PCA, one for tSNE) and a list containing the results
#'
#' @export
#'
#' @examples pcatsneexp(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
#'
#' @examples results <- pcatsneexp(directory = paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"),
#'                                     analysis = "4arm",
#'                                     exclude.animals = c("900200000068816"),
#'                                     orderlevelcond = "gtblock",
#'                                     directional = TRUE,
#'                                     perplex =  1,
#'                                     theta = 0.8,
#'                                     ellipse_tsne = TRUE,
#'                                     ellconf = 0.95,
#'                                     ellalpha = 0.10,
#'                                     pastetitle = "new_testdata_pca_09-04-2044",
#'                                     pastetitle2 = "new_testdata_tsne_09-04-2044",
#'                                     saveplotdir = FALSE)
#'          results_pca <- results[[1]]
#'          results_tsne <- results[[2]]


pcatsneexp <- function(directory, 
                       analysis = c("2arm_ko","2arm_tg", "2arm_sd", "2arm_treat",
                                    "4arm_sd_ko", "4arm_sd_tg", "4arm_treat_ko", "4arm_treat_tg"),
                       ordercolumns = c("ntb", "rdoc", "manual"),
                       ordercolumns_manual,
                       exclude.animals = FALSE,
                       orderlevelcond = c("other", "gtblock", "etblock", "2rev"),
                       return.matrix.mean = FALSE,
                       directional = FALSE,
                       absoluteval = FALSE,
                       perplex = 1, 
                       theta = 0.5,
                       ellipse_pca = FALSE,
                       ellipse_tsne = FALSE,
                       ellconf = 0.75,
                       ellalpha = 0.2,
                       pastetitle = "PCA", 
                       pastetitle2 = "tSNE",
                       saveplotdir = directory) {
  
  ### get data
  ## get matrix
  data.animal.matrix <- getexpdata(directory, 
                                   analysis,
                                   ordercolumns,
                                   ordercolumns_manual,
                                   exclude.animals,
                                   orderlevelcond,
                                   acceptable.nas = 0,
                                   return.matrix = TRUE,
                                   return.matrix.mean,
                                   healthy_norm = FALSE,
                                   naomit = TRUE,
                                   directional,
                                   absoluteval)
  ## get animal list for assignments
  data.animal.list <- getexpdata(directory, 
                                 analysis,
                                 ordercolumns,
                                 ordercolumns_manual,
                                 exclude.animals,
                                 orderlevelcond,
                                 acceptable.nas = 0,
                                 return.matrix = F) %>%
    na.omit() %>%
    select(RFID:Condition) %>%
    data.frame(.)
  # rename columns with abbreviations
  if("Meanspeed" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Meanspeed"), c("Mnsp"))
  }
  if("Rotations" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Rotations"), c("Rot"))
  }
  if("Center" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Center"), c("Ctr"))
  }
  if("Alternations" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Alternations"), c("Alt"))
  }
  if("Choices" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Choices"), c("Chc"))
  }
  if("Activity" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Activity"), c("Act"))
  }
  if("Nocturnal" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Nocturnal"), c("Noc"))
  }
  if("PlacePref" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("PlacePref"), c("PlP"))
  }
  if("SerialLearn" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("SerialLearn"), c("SrL"))
  }
  if("ReversalLearn" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("ReversalLearn"), c("RvL"))
  }
  if("SucPref" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("SucPref"), c("ScP"))
  }
  if("Baseline" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Baseline"), c("ppiBs"))
  }
  if("inhibition70" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("inhibition70"), c("in70"))
  }
  if("inhibition75" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("inhibition75"), c("in75"))
  }
  if("inhibition80" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("inhibition80"), c("in80"))
  }
  if("Timeimmobile" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Timeimmobile"), c("Tim"))
  }
  if("FreezeBase" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("FreezeBase"), c("FrBs"))
  }
  if("Context" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Context"), c("Con"))
  }
  if("Cue" %in% colnames(data.animal.matrix)) {
    data.animal.matrix <-  renameCol(data.animal.matrix, c("Cue"), c("Cue"))
  }
  
  
  ### PCA
  ## perform PCA
  pca_analysis <- prcomp(data.animal.matrix)
  ## prepare plotting
  pca_analysis_plot <- cbind(data.animal.list, pca_analysis$x)
  rownames(pca_analysis$x) <- data.animal.list$Condition
  
  ## PCA standard plot
  pca_plot <- ggplot(pca_analysis_plot, aes(x = PC1, y = PC2, color = data.animal.list$Condition,
                                            fill = data.animal.list$Condition)) +
    theme_bw() +
    theme_bw() +
    # customize title position and size
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 35)) +
    # all elements blank
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),
          # customize axes
          axis.line.y = element_line(colour = "black", size=1),
          axis.line.x = element_line(colour = "black", size=1),
          # axis.ticks.x = element_line(colour = "black", size=0.6),
          axis.text.x = element_text(angle=0, size=15),
          axis.text.y = element_text(angle=0, size=15),
          text = element_text(size=25),
          # customize legend
          legend.text = element_text(size=20),
          legend.title = element_text(size=25)) +
    `if`(ellipse_pca == TRUE, stat_ellipse(aes(x = PC1, y = PC2, color = data.animal.list$Condition),
                                           alpha = ellalpha,
                                           geom = "polygon",
                                           size = 0.85,
                                           type = "t", 
                                           level = ellconf,
                                           segments = 51,
                                           inherit.aes = TRUE,
                                           show.legend = FALSE)) +
    # colors of points
    `if`(grepl("4arm", analysis) && orderlevelcond == "gtblock", 
         scale_color_manual(values = c("#b4b4b4", "#3c3c3c", "#84dcff", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "etblock", 
         scale_color_manual(values = c("#b4b4b4", "#84dcff", "#3c3c3c", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "other", 
         scale_color_manual(values = c("#84dcff", "#1e24fc", "#b4b4b4", "#3c3c3c"))) +
    `if`(grepl("2arm", analysis), 
         scale_color_manual(values = c("#b4b4b4", "#1e24fc"))) +
    # colors of fillings
    `if`(grepl("4arm", analysis) && orderlevelcond == "gtblock", 
         scale_fill_manual(values = c("#b4b4b4", "#3c3c3c", "#84dcff", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "etblock", 
         scale_fill_manual(values = c("#b4b4b4", "#84dcff", "#3c3c3c", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "other", 
         scale_fill_manual(values = c("#84dcff", "#1e24fc", "#b4b4b4", "#3c3c3c"))) +
    `if`(grepl("2arm", analysis), 
         scale_fill_manual(values = c("#b4b4b4", "#1e24fc"))) +
    # point size
    geom_point(size = 4) +
    # customize legend title
    labs(color = "Legend") + 
    # hide legend for fillings
    guides(fill = FALSE) +
    # title
    ggtitle(pastetitle)
  print(pca_plot)
  
  ## PCA arrow plot
  pca_plot_arrow <- ggbiplot(pca_analysis, 
                             color = data.animal.list$Condition,
                             groups= data.animal.list$Condition,
                             varname.adjust = 3,
                             fill = data.animal.list$Condition) +
    theme_bw() +
    # customize title position and size
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 35)) +
    # all elements blank
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),
          # customize axes
          axis.line.y = element_line(colour = "black", size=1),
          axis.line.x = element_line(colour = "black", size=1),
          # axis.ticks.x = element_line(colour = "black", size=0.6),
          axis.text.x = element_text(angle=0, size=15),
          axis.text.y = element_text(angle=0, size=15),
          text = element_text(size=18),
          # customize legend
          legend.text = element_text(size=20),
          legend.title = element_text(size=25)) +
    stat_ellipse(aes(x = xvar, y = yvar, color = data.animal.list$Condition,
                     fill = data.animal.list$Condition),
                 alpha = ellalpha,
                 geom = "polygon",
                 size = 0.85,
                 type = "t",
                 level = ellconf,
                 segments = 51,
                 inherit.aes = FALSE,
                 show.legend = FALSE) +
    # colors of points
    `if`(grepl("4arm", analysis) && orderlevelcond == "gtblock", 
         scale_color_manual(values = c("#b4b4b4", "#3c3c3c", "#84dcff", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "etblock", 
         scale_color_manual(values = c("#b4b4b4", "#84dcff", "#3c3c3c", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "other", 
         scale_color_manual(values = c("#84dcff", "#1e24fc", "#b4b4b4", "#3c3c3c"))) +
    `if`(grepl("2arm", analysis), 
         scale_color_manual(values = c("#b4b4b4", "#1e24fc"))) +
    # colors of fillings
    `if`(grepl("4arm", analysis) && orderlevelcond == "gtblock", 
         scale_fill_manual(values = c("#b4b4b4", "#3c3c3c", "#84dcff", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "etblock", 
         scale_fill_manual(values = c("#b4b4b4", "#84dcff", "#3c3c3c", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "other", 
         scale_fill_manual(values = c("#84dcff", "#1e24fc", "#b4b4b4", "#3c3c3c"))) +
    `if`(grepl("2arm", analysis), 
         scale_fill_manual(values = c("#b4b4b4", "#1e24fc"))) +
    geom_point(aes(colour=data.animal.list$Condition), size = 4) +
    # point size and title
    labs(color = "Legend") + 
    ggtitle(paste(pastetitle, "Arrows"))
  print(pca_plot_arrow)  
  
  
  ### tSNE
  ## perform tSNE
  tsne_analysis <- Rtsne(data.animal.matrix, check_duplicates = FALSE, pca = FALSE, 
                         perplexity = perplex,
                         theta = theta, dims=2)
  ## prepare plotting
  d_tsne_analysis <- as.data.frame(tsne_analysis$Y)
  tsne_analysisplot <- cbind(data.animal.list, d_tsne_analysis)
  
  ## plot tSNE
  tsne_plot <- ggplot(tsne_analysisplot, aes(x=V1, y=V2, color = data.animal.list$Condition,
                                             fill = data.animal.list$Condition)) +
    theme_bw() +
    labs(fill = "Legend") +
    # customize title position and size
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 35)) +
    # all elements blank
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),
          # customize axes
          axis.line.y = element_line(colour = "black", size=1),
          axis.line.x = element_line(colour = "black", size=1),
          # axis.ticks.x = element_line(colour = "black", size=0.6),
          axis.text.x = element_text(angle=0, size=15),
          axis.text.y = element_text(angle=0, size=15),
          text = element_text(size=25),
          # customize legend
          legend.text = element_text(size=20),
          legend.title = element_text(size=25)) +
    `if`(ellipse_tsne == TRUE, stat_ellipse(aes(x = V1, y = V2, color = data.animal.list$Condition),
                                            alpha = ellalpha,
                                            geom = "polygon",
                                            size = 0.85,
                                            type = "t", 
                                            level = ellconf,
                                            segments = 51,
                                            inherit.aes = TRUE,
                                            show.legend = FALSE)) +
    # colors of points
    `if`(grepl("4arm", analysis) && orderlevelcond == "gtblock", 
         scale_color_manual(values = c("#b4b4b4", "#3c3c3c", "#84dcff", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "etblock", 
         scale_color_manual(values = c("#b4b4b4", "#84dcff", "#3c3c3c", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "other", 
         scale_color_manual(values = c("#84dcff", "#1e24fc", "#b4b4b4", "#3c3c3c"))) +
    `if`(grepl("2arm", analysis), 
         scale_color_manual(values = c("#b4b4b4", "#1e24fc"))) +
    # colors of fillings
    `if`(grepl("4arm", analysis) && orderlevelcond == "gtblock", 
         scale_fill_manual(values = c("#b4b4b4", "#3c3c3c", "#84dcff", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "etblock", 
         scale_fill_manual(values = c("#b4b4b4", "#84dcff", "#3c3c3c", "#1e24fc"))) +
    `if`(grepl("4arm", analysis) && orderlevelcond == "other", 
         scale_fill_manual(values = c("#84dcff", "#1e24fc", "#b4b4b4", "#3c3c3c"))) +
    `if`(grepl("2arm", analysis), 
         scale_fill_manual(values = c("#b4b4b4", "#1e24fc"))) +
    # point size and title
    geom_point(size = 4) +
    labs(color = "Legend") + 
    ggtitle(pastetitle2)
  #tsne_plot <-  tsne_plot + scale_fill_discrete(name = "New Legend Title")
  print(tsne_plot)
  
  if (saveplotdir != FALSE) {
    pdf(paste0(saveplotdir, "/PCA&tSNE.pdf"), width = 7, height = 5)
    print(pca_plot)
    print(pca_plot_arrow)
    print(tsne_plot)
    dev.off()
  }
  
  return(list(pca_analysis, tsne_analysis))
}