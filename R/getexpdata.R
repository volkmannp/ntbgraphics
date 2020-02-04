#' @title Get data of NTB experiments
#'
#' @author Paul Volkmann
#'
#' @name getexpdata
#'
#' @description A function that imports a 4-arm GT_Env NTB1.0 dataset and prepares the data for plotting and analysis.
#'
#' @param 'directory': file directory of Behavior and Animal List files
#' (mind correct spelling of both files and directory!)
#'
#' @return prepared and joined dataframe of all animals and corresponding NTB experiments
#'
#' @example
#' getexpdata("/inst/extdata/")
#'
#' directory <- system.file("extdata", package = "ntbgraphics")
#' getexpdata(directory)

getexpdata <- function(directory) {

  # example of checking inputs and returning messages
  # if (!exists(directory)) {
  #   warning("This is a warning")
  #   stop("Stop function")
  # }

  meta.data <-  read_excel(paste0(directory,"/Meta Behavior.xlsx"))
  animal.list <-  read_excel(paste0(directory, "/Animal List.xlsx"))



  data.animal.joined <-  suppressWarnings(animal.list %>%
    # exclude NAs in Genotype
    filter(Genotype!= 'NA') %>%
    # merge conditions
    unite(col="GT_Env", Genotype, Environmental, sep= "_", remove = FALSE) %>%
    # join animals and behavior data
    left_join(meta.data, by = c("RFID" = "Animal")) %>%
    # change values from chr to num
    dplyr::mutate_at(vars(Meanspeed:SerialLearn),(funs(as.numeric))) %>%
    # select relevant columns
    select(RFID, GT_Env, Meanspeed:SerialLearn))

  #get column/experiment names
  myexp <- c(as.list(colnames(data.animal.joined[, -(1:2)])))

  #return amended dataframe
  return(data.animal.joined)
}
