#' @title Plot every NTB experiment into one single PDF
#'
#' @author Paul Volkmann
#'
#' @name loopplotexp
#'
#' @description A function that takes an NTB dataset
#' and visualizes the data as customized boxplots.
#' (Requires functions 'ploteachexp' and 'getexpdata' internally.)
#' For right formatting of your files, please consider the "ReadMe for ntbgraphics".
#'
#' @param 'directory': file directory of Behavior and Animal List files
#' @param 'analysis': specifying the kind of experiment performed - 4-arm or 2-arm
#' with either transgenic or knock-out animals as group of interest
#' (or choosing the kind of analysis preferred)
#' (default: "4arm")
#' @param 'orderplots': gives user ability to specifiy order of plots
#' (for "other", plot order will depend on alphabetical order of GT_Env objects;
#' for "tcf4", plot order will be wt_hc, wt_sd, tg_hc, tg_sd (only for 4-arm experiments))
#' (default: "other")
#' @param 'saveplotdir': file directory where to save plots
#' (default: location of Behavior and Animal List files as specified in 'directory')
#'
#' @return all boxplots saved in PDF
#'
#' @export
#'
#' @example
#' loopplotexp(directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
#' analysis = "4arm",
#' orderplots = "tcf4")


loopplotexp <- function(directory, analysis = c("4arm", "2arm_tg", "2arm_ko"),
                      orderplots = c("other", "tcf4"), saveplotdir = directory) {

  ## get and modify data
  data.animal.joined <- getexpdata(directory, analysis)

  ## loop through and plot list of experiments
  myexp <- c(as.list(colnames(data.animal.joined[, -(1:2)])))
  allplots <- list()
  allplots <- map(myexp, ploteachexp, directory, analysis, orderplots, saveplotdir = FALSE)

  pdf(paste0(saveplotdir, "/All_experiments.pdf"))
  print(allplots)
  dev.off()
}
