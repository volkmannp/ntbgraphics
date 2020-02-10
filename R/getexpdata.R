#' @title Get data of NTB experiments
#'
#' @author Paul Volkmann
#'
#' @name getexpdata
#'
#' @description A function that imports an NTB dataset and prepares the data for plotting and analysis.
#' For right formatting of your files, please consider the "ReadMe for ntbgraphics".
#'
#' @param 'directory': file directory of Behavior and Animal List files
#' (mind correct spelling of both files and directory!)
#' @param 'analysis': specifying the kind of experiment performed - 4-arm by default or 2-arm
#' with either transgenic or knock-out animals as group of interest
#' (or choosing the kind of analysis preferred)
#'
#' @return prepared and joined dataframe of all animals and corresponding NTB experiments
#'
#' @export
#'
#' @example
#' getexpdata("./inst/extdata/", "2arm_tg")
#'
#' directory_test <- system.file("extdata", package = "ntbgraphics")
#' getexpdata(directory_test, "4arm")


getexpdata <- function(directory, analysis = c("4arm", "2arm_tg", "2arm_ko")) {

  # import data
  meta.data <-  readxl::read_excel(paste0(directory,"/Meta Behavior.xlsx"))
  animal.list <-  readxl::read_excel(paste0(directory, "/Animal List.xlsx"))

  data.animal.joined <-  animal.list %>%
    # exclude NAs in Genotype
    filter(Genotype!= 'NA') %>%
    # merge conditions in case of 4-arm
    `if`(analysis == "4arm", unite(., col="GT_Env", Genotype, Environmental, sep= "_", remove = FALSE), .) %>%
    # join animals and behavior data
    left_join(meta.data, by = c("RFID" = "Animal")) %>%
    # change values from chr to num
    mutate_at(vars(Meanspeed:SerialLearn),list(as.numeric)) %>%
    # select relevant columns
    `if`(analysis == "4arm", select(., RFID, GT_Env, Meanspeed:SerialLearn),.) %>%
    `if`(analysis == "2arm_tg", select(., RFID, Genotype, Meanspeed:SerialLearn),.) %>%
    `if`(analysis == "2arm_ko", select(., RFID, Genotype, Meanspeed:SerialLearn),.)

  #return amended dataframe
  return(data.animal.joined)
}
