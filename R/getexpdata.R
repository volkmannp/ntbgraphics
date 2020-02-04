#' @title Get data of NTB experiments
#'
#' @author Paul Volkmann
#'
#' @name getexpdata
#'
#' @description A function that imports a 4-arm GT_Env NTB dataset and prepares the data for plotting and analysis.
#'
#' @param 'directory': file directory of Behavior and Animal List files
#' (mind correct spelling of both files and directory!)
#'
#' @import dplyr
#' @import readxl
#' @import tidyr
#'
#' @return prepared and joined dataframe of all animals and corresponding NTB experiments
#'
#' @example
#' getexpdata("./inst/extdata/")
#'
#' directory <- system.file("extdata", package = "ntbgraphics")
#' getexpdata(directory)


getexpdata <- function(directory) {

  meta.data <-  read_excel(paste0(directory,"/Meta Behavior.xlsx"))
  animal.list <-  read_excel(paste0(directory, "/Animal List.xlsx"))

  data.animal.joined <-  animal.list %>%
    # exclude NAs in Genotype
    filter(Genotype!= 'NA') %>%
    # merge conditions
    unite(col="GT_Env", Genotype, Environmental, sep= "_", remove = FALSE) %>%
    # join animals and behavior data
    left_join(meta.data, by = c("RFID" = "Animal")) %>%
    # change values from chr to num
    mutate_at(vars(Meanspeed:SerialLearn),list(as.numeric)) %>%
    # select relevant columns
    select(RFID, GT_Env, Meanspeed:SerialLearn)

  #return amended dataframe
  return(data.animal.joined)
}
