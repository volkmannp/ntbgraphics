#' @title Plot all NTB experiments into one single PDF
#'
#' @author Paul Volkmann
#'
#' @name loopplotexp
#'
#' @description A function that takes an NTB dataset and visualizes the data as customized boxplots, saving
#' it into one PDF file.
#' Requires functions 'ploteachexp' and 'getexpdata' internally.
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
#' @param ordercolumns defines the order paradigm of experiment (column) appearance in internal table and
#' final PDF file within quotation marks: "ntb", "rdoc", "manual";
#' order of experiments may be chronological with "ntb", follow RDoC clustering with "rdoc" or be customized
#' manually with "manual" (-> use 'ordercolumns_manual' for exact appearance; there, you may also choose to 
#' exclude experiments);
#' default: "ntb"
#' @param ordercolumns_manual customizes order of appearance and appearance itself of experiment columns 
#' in internal table and final PDF file (experiments that are not listed will not be included);
#' only if 'ordercolumns' = "manual";
#' user has to provide a vector containing characters within quotation marks (e.g. by using 
#' c("Meanspeed", "SerialLearn")) with all experiments he wants to include into the experiment selection with
#' desired order;
#' no need for specification if 'ordercolumns' is not "manual"
#' default: FALSE
#' @param exclude.animals excluding animals from analysis by RFID;
#' user has to provide a vector containing characters within quotation marks (e.g. by using 
#' c("900200000067229", "900200000065167")) with all animals he wants to exclude from the final plotting;
#' if FALSE is provided, no animal will be excluded;
#' default: FALSE
#' @param orderlevelcond defines order of boxplots in plot within quotation marks:
#' "other", "gtblock", "etblock", "2rev";
#' "other" for alphabetical order in case of 4arm; also for default order of 2arm experiments
#' (which displays the 'control' first, then the 'condition');
#' "gtblock" for order wt_x, wt_y, tg_x, tg_y;
#' "etblock" for order x_hc, y_hc, x_sd, y_sd;
#' "2rev" for inverse order of 2arm default only, meaning displaying the 'condition' first, then the 
#' 'control';
#' default: "other"
#' @param acceptable.nas defines the maximum number of NAs allowed within the same row;
#' if number of actual NAs within one row is bigger than the number provided, the row will be excluded from 
#' table and following plotting;
#' if the number of acceptable NAs should be unlimited, no value has to be provided;
#' default: "unlimited"
#' @param saveplotdir file directory where to save plots;
#' you may set to FALSE if you do not want to save plots to PDF;
#' default: location of Behavior and Animal List files as specified in 'directory'
#'
#' @return all boxplots saved in one PDF
#'
#' @export
#' 
#' @examples loopplotexp(paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"))
#'
#' @examples loopplotexp(directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
#'                      analysis = "4arm_sd_tg",
#'                      ordercolumns = "ntb",
#'                      exclude.animals = c("900200000068816"),
#'                      orderlevelcond = "etblock",
#'                      acceptable.nas = 2,
#'                      saveplotdir = paste0(system.file("../plots", package = "ntbgraphics", mustWork = T),"/"))
                      

loopplotexp <- function(directory, 
                        analysis = c("2arm_ko","2arm_tg", "2arm_sd", "2arm_treat",
                                     "4arm_sd_ko", "4arm_sd_tg", "4arm_treat_ko", "4arm_treat_tg"),
                        ordercolumns = c("ntb", "rdoc", "manual"),
                        ordercolumns_manual = FALSE,
                        exclude.animals = FALSE,
                        orderlevelcond = c("other", "gtblock", "etblock", "2rev"),
                        acceptable.nas = "unlimited",
                        saveplotdir = directory) {

  ## get and modify data
  data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, exclude.animals, 
                                   orderlevelcond, acceptable.nas, return.matrix = F)

  ## loop through and plot list of experiments
  myexp <- c(as.list(colnames(data.animal.joined[, -(1:2)])))
  allplots <- list()
  allplots <- map(myexp, ploteachexp, directory, analysis, ordercolumns, ordercolumns_manual, 
                  exclude.animals, orderlevelcond, acceptable.nas, saveplotdir = F)

  pdf(paste0(saveplotdir, "/All_experiments.pdf"))
  print(allplots)
  dev.off()
}
