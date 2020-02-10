#' @title Plot NTB experiments
#'
#' @author Paul Volkmann
#'
#' @name ploteachexp
#'
#' @description A function that takes experiments (columns) of an NTB dataset
#' and visualizes the data as customized boxplots. (Requires function 'getexpdata' internally.)
#'
#' @param 'expname': name of a column/experiment of the NTB dataset
#' @param 'directory': file directory of Behavior and Animal List files
#' @param 'analysis': specifying the kind of experiment performed - 4-arm by default or 2-arm
#' with either transgenic or knock-out animals as group of interest
#' (or choosing the kind of analysis preferred)
#' @param 'orderplots': gives user ability to specifiy order of plots
#' (by default and for "other", plot order will depend on alphabetical order of GT_Env objects;
#' for "tcf4", plot order will be wt_hc, wt_sd, tg_hc, tg_sd (only for 4-arm experiments))
#' @param 'saveplotdir': file directory where to save plots
#' (default at location of Behavior and Animal List files);
#' you may set to FALSE if you do not want to save plot to PDF.
#'
#' @return boxplot saved as PDF
#'
#' @export
#'
#' @example
#' ploteachexp(expname = "Meanspeed",
#' directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
#' analysis = "4arm",
#' orderplots = "tcf4",
#' saveplotdir = paste0(system.file("../plots", package = "ntbgraphics", mustWork = T),"/"))
#'
#' myexp <- c(as.list(colnames(data.animal.joined[, -(1:2)])))
#' map(myexp, ploteachexp, paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
#' analysis= "2arm_tg")


ploteachexp <- function(expname, directory, analysis = c("4arm", "2arm_tg", "2arm_ko"),
                        orderplots = c("other", "tcf4"), saveplotdir = directory) {

  #getexpdata
  data.animal.joined <- getexpdata(directory, analysis)

  # order plot appearance
  `if`(orderplots == "tcf4", data.animal.joined$GT_Env <- factor(data.animal.joined$GT_Env,
                                                                 levels = c("wt_hc", "wt_sd",
                                                                            "tg_hc", "tg_sd")))

  # define axis limits
  ymin = min(data.animal.joined[[expname]], na.rm = TRUE)*0.25
  ymax = max(data.animal.joined[[expname]], na.rm = TRUE)*1.25


  # plotting
  `if`(analysis == "2arm_tg",
       outplot <- ggplot(data.animal.joined, aes_string(x="Genotype", y=expname, fill="Genotype")),
  `if`(analysis == "2arm_ko",
       outplot <- ggplot(data.animal.joined, aes_string(x="Genotype", y=expname, fill="Genotype")),
  `if`(analysis == "4arm",
       outplot <- ggplot(data.animal.joined, aes_string(x="GT_Env", y=expname, fill="GT_Env")))))


    # boxplot with transparent filling
    outplot <- outplot + geom_boxplot(alpha = 0.4) +

    # choose colors
    `if`(analysis == "4arm", scale_fill_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +
    `if`(analysis == "4arm", scale_color_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B"))) +
    `if`(analysis == "2arm_tg", scale_fill_manual(values=c("#00441B", "#CCECE6"))) +
    `if`(analysis == "2arm_tg", scale_color_manual(values=c("#00441B", "#CCECE6"))) +
    `if`(analysis =="2arm_ko", scale_fill_manual(values=c("#00441B", "#CCECE6"))) +
    `if`(analysis == "2arm_ko", scale_color_manual(values=c("#00441B", "#CCECE6"))) +

    # add data points
    geom_point(pch = 21, stroke=0.93, position = position_jitterdodge()) +

    # title of axes
    ylab(paste0(expname, "Score")) +
    `if`(analysis == "4arm", xlab("Conditions")) +
    `if`(analysis == "2arm_tg", xlab("Genotype")) +
    `if`(analysis == "2arm_ko", xlab("Genotype")) +

    # range of y-axis
    coord_cartesian(ylim=c(ymin, ymax)) +

    # asterisks for significance
    `if`(analysis == "4arm", geom_signif(test = "t.test",
                comparisons = list(c(1, 2),
                                   c(3, 4),
                                   c(2, 3),
                                   c(2, 4)),
                y=c(0.85*ymax, 0.85*ymax, 0.89*ymax, 0.95*ymax),
                map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05),
                textsize = 5,  tip_length = 0.005)) +
    `if`(analysis == "2arm_tg", geom_signif(test = "t.test",
                comparisons = list(c(1, 2)),
                y=0.89*ymax,
                map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05),
                textsize = 5,  tip_length = 0.005)) +
    `if`(analysis == "2arm_ko", geom_signif(test = "t.test",
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
