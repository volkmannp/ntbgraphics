#' @title Plot NTB experiments
#'
#' @author Paul Volkmann
#'
#' @name loopplotexp
#'
#' @description A function that takes an NTB dataset
#' and visualizes the data as customized boxplots.
#' (Requires functions 'ploteachexp' and 'getexpdata' internally.)
#'
#' @param 'directory': file directory of Behavior and Animal List files
#' @param 'analysis': specifying the kind of experiment performed - 4-arm by default or 2-arm
#' with either transgenic or knock-out animals as group of interest
#' (or choosing the kind of analysis preferred)
#' @param 'orderplots': gives user ability to specifiy order of plots
#' (by default and for "other", plot order will depend on alphabetical order of GT_Env objects;
#' for "tcf4", plot order will be wt_hc, wt_sd, tg_hc, tg_sd (only for 4-arm experiments))
#' @param 'saveplotdir': file directory where to save plots
#' (default at location of Behavior and Animal List files)
#'
#' @return boxplot saved as PDF
#'
#' @export
#'
#' @example
#' loopplotexp(directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
#' analysis = "4arm",
#' orderplots = "tcf4",
#' saveplotdir = paste0(system.file("../plots", package = "ntbgraphics", mustWork = T),"/"))


loopplotexp <- function(directory, analysis = c("4arm", "2arm_tg", "2arm_ko"),
                      orderplots = c("other", "tcf4"), saveplotdir = directory) {

  ## get and modify data
  data.animal.joined <- getexpdata(directory, analysis)

  ## loop through and plot list of experiments
  myexp <- c(as.list(colnames(data.animal.joined[, -(1:2)])))
  map(myexp, ploteachexp, directory, analysis, orderplots, saveplotdir)
}
