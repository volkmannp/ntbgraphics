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
#' @param 'analysis': specifying the kind of experiment performed - 4-arm or 2-arm
#' with either transgenic or knock-out animals or with social defeat looling at environmental condition
#' as group of interest
#' (respectively, choosing the kind of analysis preferred)
#' (default: "4arm")
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


getexpdata <- function(directory, 
                       analysis = c("4arm", "2arm_tg", "2arm_ko", "2arm_sd"),
                       ordercolumns = c("ntb", "rdoc", "manual"),
                       ordercolumns_manual,
                       exclude = FALSE) {
  
  # import data
  meta.data <-  readxl::read_excel(paste0(directory,"/Meta Behavior.xlsx"))
  animal.list <-  readxl::read_excel(paste0(directory, "/Animal List.xlsx"))
  
  data.animal.joined <-  animal.list %>%
    # exclude NAs in Genotype
    filter(Genotype!= 'NA') %>%
    # merge conditions in case of 4-arm
    `if`(analysis == "4arm", unite(., col="Condition", Genotype, Environmental, sep= "_", remove = FALSE), .) %>%
    `if`(analysis == "2arm_tg", dplyr::rename(., Condition = Genotype), .) %>%
    `if`(analysis == "2arm_ko", dplyr::rename(., Condition = Genotype), .) %>% 
    `if`(analysis == "2arm_sd", dplyr::rename(., Condition = Environmental), .) %>%
    # join animals and behavior data
    left_join(meta.data, by = c("RFID" = "Animal"))
  
  
  # define order of columns
  if (ordercolumns == "ntb") {
    col.names <- c("RFID", "Condition", # identifiers                                                                 
                   "Meanspeed", "Rotations", # open field
                   "Center",  "Alternations", "Choices", # y maze
                   "Activity", "Nocturnal", "PlacePref", "SerialLearn", "ReversalLearn", "SucPref", # ic
                   "Baseline", "inhibition70", "inhibition75", "inhibition80", # ppi
                   "Timeimmobile", # tail suspension
                   "FreezeBase", "Context", "Cue") # fear conditioning
  } else if (ordercolumns == "rdoc") {
    col.names <- c("RFID", "Condition", # identifiers
                   "Alternations", "ReversalLearn", "SerialLearn", "Cue", "Context", # cognition
                   "SucPref", "PlacePref", "Rotations", # positive valence
                   "Center", "FreezeBase", "Timeimmobile", "Baseline", # negative valence
                   "Activity", "Nocturnal", "Choices", "Meanspeed", # arousal and regulation
                   "inhibition70", "inhibition75", "inhibition80") # sensorimotor
  } else if (ordercolumns == "manual") {
    col.names <- c("RFID", "Condition", ordercolumns_manual)
  }
  
  # define number of column positions
  col.pos <- c(1:length(col.names))
  # create data frame with all possible column names and their ideal positions
  col.names.order.ideal <- data.frame(col.names, col.pos)
  # create data frame with actual column names
  col.names.order.actual <- data.frame(colnames(data.animal.joined))
  
  order.input <- col.names.order.actual %>% 
    # join the two created frames
    left_join(col.names.order.ideal, by=c("colnames.data.animal.joined."="col.names")) %>% 
    # loose all NAs, i.e. columns that do not exist in data.animal.joined
    na.omit() %>% 
    # sort by ideal positions
    arrange(., col.pos) %>% 
    # select your column names, now sorted
    select(., colnames.data.animal.joined.) %>% 
    # extract your column names as a vector
    pull(., colnames.data.animal.joined.)
  
  
  data.animal.joined <- data.animal.joined %>% 
    # select relevant columns and adjust order according to former preparation
    select(., all_of(order.input)) %>% 
    # change values from chr to num
    mutate_at(., vars(nth(order.input, 3):last(order.input)),list(as.numeric)) %>% 
    # delete selected animals
    `if`(exclude != FALSE, filter(., !RFID %in% exclude),.)
  
  #return amended dataframe
  return(data.animal.joined)
}


