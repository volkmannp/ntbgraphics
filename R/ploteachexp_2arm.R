#' @title Plot NTB experiments (2-arm)
#'
#' @author Paul Volkmann
#'
#' @name ploteachexp_2arm
#'
#' @description A function that takes experiments (columns) of a 2-arm NTB dataset
#' and visualizes the data as customized boxplots.
#'
#' @param 'expname': name of a column/experiment of an NTB dataset
#' @param 'directory': file directory where to save plots
#'
#' @import ggplot2
#' @importFrom ggplot2 ggplot
#' @importFrom ggsignif geom_signif
#'
#' @return boxplot saved as PDF
#'
#' @example
#' ploteachexp_2arm("Meanspeed", paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
#'
#' myexp <- c(as.list(colnames(data.animal.joined[, -(1:2)])))
#' lapply(myexp, ploteachexp_2arm, paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))


ploteachexp_2arm <- function(expname, directory) {

  # define axis limits
  ymin = min(data.animal.joined[[expname]], na.rm = TRUE)*0.25
  ymax = max(data.animal.joined[[expname]], na.rm = TRUE)*1.25

  # plotting
  ggplot(data.animal.joined, aes_string(x="Genotype", y=expname, fill="Genotype")) +

    # boxplot with transparent filling
    geom_boxplot(alpha = 0.4) +

    # choose colors
    scale_fill_manual(values=c("#CCECE6", "#00441B")) +
    scale_color_manual(values=c("#CCECE6", "#00441B")) +

    # add data points
    geom_point(pch = 21, stroke=0.93, position = position_jitterdodge()) +

    # title of axes
    ylab(paste0(expname, "Score")) +
    xlab("Genotype") +

    # range of y-axis
    coord_cartesian(ylim=c(ymin, ymax)) +

    # asterisks for significance
    geom_signif(test = "t.test",
                comparisons = list(c(1, 2)),
                y=0.89*ymax,
                map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05),
                textsize = 5,  tip_length = 0.005) +

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
  ggsave(paste0(directory, expname, "_2arm.pdf"))
}
