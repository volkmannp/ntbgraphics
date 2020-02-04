#' @title Get data of NTB experiments (2-arm)
#'
#' @author Paul Volkmann
#'
#' @name getexpdata_2arm
#'
#' @description A function that imports a 2-arm GT NTB dataset and prepares the data for plotting and analysis.
#'
#' @param 'directory': file directory of Behavior and Animal List files
#' (mind correct spelling of both files and directory!)
#'
#' @return prepared and joined dataframe of all animals and corresponding NTB experiments
#'
#' @export
#'
#' @example
#' getexpdata("./inst/extdata/")
#'
#' directory <- system.file("extdata", package = "ntbgraphics")
#' getexpdata(directory)


getexpdata_2arm <- function(directory) {

  meta.data <-  readxl::read_excel(paste0(directory,"/Meta Behavior.xlsx"))
  animal.list <-  readxl::read_excel(paste0(directory, "/Animal List.xlsx"))

  data.animal.joined <-  animal.list %>%
    # exclude NAs in Genotype
    filter(Genotype!= 'NA') %>%
    # join animals and behavior data
    left_join(meta.data, by = c("RFID" = "Animal")) %>%
    # change values from chr to num
    mutate_at(vars(Meanspeed:SerialLearn),list(as.numeric)) %>%
    # select relevant columns
    select(RFID, Genotype, Meanspeed:SerialLearn)

  #return amended dataframe
  return(data.animal.joined)
}
