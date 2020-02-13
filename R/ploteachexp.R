#' @title Plot NTB experiments
#'
#' @author Paul Volkmann
#'
#' @name ploteachexp
#'
#' @description A function that takes experiments (columns) of an NTB dataset
#' and visualizes the data as customized boxplots. (Requires function 'getexpdata' internally.)
#' For right formatting of your files, please consider the "ReadMe for ntbgraphics".
#'
#' @param directory file directory of 'Meta Behavior' and 'Animal List' files
#' (mind correct spelling of both files and directory!)
#' (no default)
#' @param expname name of a column/experiment of the NTB dataset
#' @param analysis specifying the kind of experiment performed;
#' "4arm_sd_tg", "4arm_sd_ko", "4arm_treat_tg", "4arm_treat_ko",
#' "2arm_tg", "2arm_ko", "2arm_sd", "2arm_treat";
#' (tg for transgenic, ko for knockout;
#' 4arm_sd_x assumes a stress paradigm with social defeat (sd) and housing or handling control (hc) as 
#' control;
#' 4arm_treat_x assumes a treatment paradigm with treated (treat) and untreated (untreat) animals;
#' 2arm_x assumes wildtype controls (wt) for tg and ko, housing or handling controls (hc) for sd and
#' untreated controls (untreat) for treat
#' (analysis defines the kind of experiment performed, respectively the kind of analysis preferred - 
#' you can easily perform 2arm analysis for 4arm experiments, looking only at the groups of interest, 
#' but not the other way around)
#' (default: "2arm_ko")
#' @param exclude.animals excluding animals from analysis by RFID
#' user has to provide a vector containing characters (e.g. by using c("900200000067229", "900200000065167")) 
#' with all animals he wants to exclude from the final table;
#' if FALSE is provided, no animal will be excluded
#' (default= FALSE)
#' @param orderlevelcond defines order of boxplots within each experiment:
#' "other" for alphabetical order in case of 4arm and >'control', 'condition'< as order for 2arm experiments;
#' "gtblock" for order wt_x, wt_y, tg_x, tg_y;
#' "etblock" for order x_hc, y_hc, x_sd, y_sd;
#' note that no specification is possible for 2arm experiments
#' (default: "other")
#' @param saveplotdir file directory where to save plots;
#' you may set to FALSE if you do not want to save plot to PDF
#' (default: location of Behavior and Animal List files as specified in 'directory')
#'
#' @return boxplot saved as PDF
#'
#' @export
#'
#' @example
#' ploteachexp(expname = "Meanspeed",
#' directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"))
#' 
#' ploteachexp(expname = "Center",
#' directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
#' analysis = "4arm_sd_tg",
#' exclude.animals = c("900200000068816"),
#' orderlevelcond = "etblock",
#' saveplotdir = paste0(system.file("../plots", package = "ntbgraphics", mustWork = T),"/"))


ploteachexp <- function(directory, 
                        expname,
                        analysis = c("2arm_ko","2arm_tg", "2arm_sd", "2arm_treat",
                                     "4arm_sd_ko", "4arm_sd_tg", "4arm_treat_ko", "4arm_treat_tg"),
                        ordercolumns = c("ntb", "rdoc", "manual"),
                        exclude.animals = FALSE,
                        orderlevelcond = c("other", "gtblock", "etblock"),
                        saveplotdir = directory) {
  
  #getexpdata
  data.animal.joined <- getexpdata(directory, analysis, ordercolumns, exclude, orderlevelcond)

  # define axis limits
  ymin = min(data.animal.joined[[expname]], na.rm = TRUE)*0.25
  ymax = max(data.animal.joined[[expname]], na.rm = TRUE)*1.25


  # plotting
  outplot <- ggplot(data.animal.joined, aes_string(x="Condition", y=expname, fill="Condition"))
  # `if`(analysis == "2arm_ko",
  #      outplot <- ggplot(data.animal.joined, aes_string(x="Condition", y=expname, fill="Condition")),
  # `if`(analysis == "4arm",
  #      outplot <- ggplot(data.animal.joined, aes_string(x="Condition", y=expname, fill="Condition")))))


    # boxplot with transparent filling
    outplot <- outplot + geom_boxplot(alpha = 0.4) +

    # choose colors (order depends on GT first, than other factors)
    `if`(analysis == "4arm_sd_tg" && orderlevelcond == "gtblock", 
         scale_fill_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +
    `if`(analysis == "4arm_sd_tg" && orderlevelcond == "gtblock", 
         scale_color_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +
    `if`(analysis == "4arm_sd_ko" && orderlevelcond == "gtblock", 
         scale_fill_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +
    `if`(analysis == "4arm_sd_ko" && orderlevelcond == "gtblock", 
         scale_color_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +
    `if`(analysis == "4arm_treat_tg" && orderlevelcond == "gtblock", 
         scale_fill_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +
    `if`(analysis == "4arm_treat_tg" && orderlevelcond == "gtblock", 
         scale_color_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +
    `if`(analysis == "4arm_treat_ko" && orderlevelcond == "gtblock", 
         scale_fill_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +
    `if`(analysis == "4arm_treat_ko" && orderlevelcond == "gtblock", 
         scale_color_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +

    `if`(analysis == "4arm_sd_tg" && orderlevelcond == "etblock", 
         scale_fill_manual(values=c("#F7FCFD", "#238B45", "#CCECE6", "#00441B"))) +
    `if`(analysis == "4arm_sd_tg" && orderlevelcond == "etblock", 
         scale_color_manual(values=c("#F7FCFD", "#238B45", "#CCECE6", "#00441B"))) +
    `if`(analysis == "4arm_sd_ko" && orderlevelcond == "etblock", 
         scale_fill_manual(values=c("#F7FCFD", "#238B45", "#CCECE6", "#00441B"))) +
    `if`(analysis == "4arm_sd_ko" && orderlevelcond == "etblock", 
         scale_color_manual(values=c("#F7FCFD", "#238B45", "#CCECE6", "#00441B"))) +
    `if`(analysis == "4arm_treat_tg" && orderlevelcond == "etblock", 
         scale_fill_manual(values=c("#F7FCFD", "#238B45", "#CCECE6", "#00441B"))) +
    `if`(analysis == "4arm_treat_tg" && orderlevelcond == "etblock", 
         scale_color_manual(values=c("#F7FCFD", "#238B45", "#CCECE6", "#00441B"))) +
    `if`(analysis == "4arm_treat_ko" && orderlevelcond == "etblock", 
         scale_fill_manual(values=c("#F7FCFD", "#238B45", "#CCECE6", "#00441B"))) +
    `if`(analysis == "4arm_treat_ko" && orderlevelcond == "etblock", 
         scale_color_manual(values=c("#F7FCFD", "#238B45", "#CCECE6", "#00441B"))) +
      
    `if`(analysis == "2arm_tg", scale_fill_manual(values=c("#CCECE6", "#00441B"))) +
    `if`(analysis == "2arm_tg", scale_color_manual(values=c("#CCECE6", "#00441B"))) +
    `if`(analysis =="2arm_ko", scale_fill_manual(values=c("#CCECE6", "#00441B"))) +
    `if`(analysis == "2arm_ko", scale_color_manual(values=c("#CCECE6", "#00441B"))) +
    `if`(analysis == "2arm_sd", scale_fill_manual(values=c("#CCECE6", "#00441B"))) +
    `if`(analysis == "2arm_sd", scale_color_manual(values=c("#CCECE6", "#00441B"))) +
    `if`(analysis =="2arm_treat", scale_fill_manual(values=c("#CCECE6", "#00441B"))) +
    `if`(analysis == "2arm_treat", scale_color_manual(values=c("#CCECE6", "#00441B"))) +

    # add data points
    geom_point(pch = 21, stroke=0.93, position = position_jitterdodge()) +

    # title of axes
    ylab(paste0(expname, "Score")) +
    xlab("Condition") +


    # range of y-axis
    coord_cartesian(ylim = c(ymin, ymax)) +

    # asterisks for significance
    `if`(analysis == "4arm_sd_tg" || analysis == "4arm_sd_ko" ||
           analysis == "4arm_treat_tg" || analysis == "4arm_treat_ko", 
         geom_signif(test = "t.test",
                comparisons = list(c(1, 2),
                                   c(3, 4),
                                   c(2, 3),
                                   c(2, 4)),
                y=c(0.85*ymax, 0.85*ymax, 0.89*ymax, 0.95*ymax),
                map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05),
                textsize = 5,  tip_length = 0.005)) +
    `if`(analysis == "2arm_tg" || analysis == "2arm_ko"
         || analysis == "2arm_sd" || analysis == "2arm_treat"
         , geom_signif(test = "t.test",
                comparisons = list(c(1, 2)),
                y=0.89*ymax,
                map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05),
                textsize = 5,  tip_length = 0.005)) +

    # customize title
    ggtitle(expname) +

    # choose theme for layout (theme_choose)
    theme_bw() +

    # customize title position and size
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 16)) +

    # all elements blank
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),

          # customize axes
          axis.line.y = element_line(colour = "black", size=0.6),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(angle=0, size=10),
          axis.text.y = element_text(angle=0, size=10),
          text = element_text(size=14),

          # customize legend
          legend.text = element_text(size=9),
          legend.title = element_text(size=12))

  # save pdf
  `if`(!(saveplotdir =="FALSE"), ggsave(paste0(saveplotdir, expname, ".pdf")))
  # return plot
  return(outplot)
}
