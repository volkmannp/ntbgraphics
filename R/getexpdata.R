#' @title Get data of NTB experiments
#'
#' @author Paul Volkmann
#'
#' @name getexpdata
#'
#' @description A function that imports an NTB dataset and prepares the data for plotting and analysis.
#' For right formatting of your files, please consider the "ReadMe for ntbgraphics".
#'
#' @param directory file directory of 'Meta Behavior' and 'Animal List' files
#' (mind correct spelling of both files and directory!)
#' (no default)
#' @param analysis specifying the kind of experiment performed;
#' "4arm_sd_tg", "4arm_sd_ko", "4arm_treat_tg", "4arm_treat_ko",
#' "2arm_tg", "2arm_ko", "2arm_sd", "2arm_treat";
#' (tg for transgenic, ko for knockout;
#' 4arm_sd_x assumes a stress paradigm with social defeat (sd) and housing or handling control (hc) as 
#' control;
#' 4arm_treat_x assumes a treatment paradigm with treated (treat) and untreated (untreat) animals;
#' 2arm_x assumes wildtype controls (wt) for tg and ko, housing or handling controls (hc) for sd and
#' untreated controls (untreat) for treat
#' (analysis defines the kind of experiment performed, respectively the kind of analysis preferred - 
#' you can easily perform 2arm analysis for 4arm experiments, looking only at the groups of interest, 
#' but not the other way around)
#' (default: "2arm_ko")
#' @param ordercolumns defining the order of experiment column appearance in final table;
#' RFID and Condition are always listed first and need no specification;
#' order of experiments may be chronological with "ntb", follow RDoC clustering with "rdoc" or be customized
#' manually with "manual" (-> use ordercolumns_manual for exact appearance; there, you may also choose to 
#' exclude experiments)
#' (default: "ntb") 
#' @param ordercolumns_manual customizing order of appearance and appearance itself of experiment columns 
#' in final table (experiments that are not listed will not be included);
#' only if ordercolumns = "manual";
#' user has to provide a vector containing characters (e.g. by using c("Meanspeed", "SerialLearn")) 
#' with all experiments he wants to include into the final tabel
#' (no default; no need for specification if ordercolumns is not "manual")
#' @param exclude.animals excluding animals from analysis by RFID
#' user has to provide a vector containing characters (e.g. by using c("900200000067229", "900200000065167")) 
#' with all animals he wants to exclude from the final table;
#' if FALSE is provided, no animal will be excluded
#' (default= FALSE)
#' @param orderlevelcond defines order of factor levels of conditions 
#' (which might be important when it comes to plotting or displaying your data grouped by condition 
#' in a defined order):
#' "other" for alphabetical order in case of 4arm and >'control', 'condition'< as default order for 
#' 2arm experiments;
#' "gtblock" for order wt_x, wt_y, tg_x, tg_y;
#' "etblock" for order x_hc, y_hc, x_sd, y_sd;
#' note that no (further) specification is possible for 2arm experiments
#' (default: "other")
#'
#' @return prepared and joined dataframe of all animals and corresponding NTB experiments
#'
#' @export
#'
#' @example
#' getexpdata(directory = "./inst/extdata/", analysis = "4arm_sd_tg")
#'
#' directory_test <- system.file("extdata", package = "ntbgraphics")
#' getexpdata(directory = directory_test, analysis = "2arm_sd", ordercolumns = "manual", 
#' ordercolumns_manual = c("Meanspeed", "SerialLearn", "Center"), exclude.animals = c("900200000070142"))


getexpdata <- function(directory, 
                       analysis = c("2arm_ko","2arm_tg", "2arm_sd", "2arm_treat",
                                    "4arm_sd_ko", "4arm_sd_tg", "4arm_treat_ko", "4arm_treat_tg"),
                       ordercolumns = c("ntb", "rdoc", "manual"),
                       ordercolumns_manual,
                       exclude.animals = FALSE,
                       orderlevelcond = c("other", "gtblock", "etblock")) {
  
  options(warn = -1)
  
  ## import data
  meta.data <-  readxl::read_excel(paste0(directory,"/Meta Behavior.xlsx"))
  animal.list <-  readxl::read_excel(paste0(directory, "/Animal List.xlsx"))
  
  # modify tables
  data.animal.joined <-  animal.list %>%
    # exclude NAs in Genotype
    filter(Genotype!= 'NA') %>%
    # merge conditions in case of 4arm
    `if`(analysis == "4arm_sd_tg", unite(., col="Condition", Genotype, Environmental, sep= "_", 
                                      remove = FALSE), .) %>%
    `if`(analysis == "4arm_sd_ko", unite(., col="Condition", Genotype, Environmental, sep= "_", 
                                      remove = FALSE), .) %>%
    `if`(analysis == "4arm_treat_tg", unite(., col="Condition", Genotype, Treatment, sep= "_", 
                                      remove = FALSE), .) %>%
    `if`(analysis == "4arm_treat_ko", unite(., col="Condition", Genotype, Treatment, sep= "_", 
                                         remove = FALSE), .) %>%
    # rename column of interest in case of 2arm
    `if`(analysis == "2arm_tg", dplyr::rename(., Condition = Genotype), .) %>%
    `if`(analysis == "2arm_ko", dplyr::rename(., Condition = Genotype), .) %>% 
    `if`(analysis == "2arm_sd", dplyr::rename(., Condition = Environmental), .) %>%
    `if`(analysis == "2arm_treat", dplyr::rename(., Condition = Treatment), .) %>%
    # join animals and behavior data
    left_join(meta.data, by = c("RFID" = "Animal"))
  
  
  # define preferred order of columns
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
  
  ## prepare order setup
  # consider intersect(x, y)
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
    `if`(exclude.animals != FALSE, filter(., !RFID %in% exclude.animals),.)
  
  # order factor levels of conditions (e.g. for order of plot appearance)
  # stringr::str_detect(analysis, "^4arm_sd") &&
  
  `if`(analysis == "4arm_sd_tg" && orderlevelcond == "gtblock",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt_hc", "wt_sd", "tg_hc", "tg_sd")),.)
  `if`(analysis == "4arm_sd_ko" && orderlevelcond == "gtblock",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt_hc", "wt_sd", "ko_hc", "ko_sd")),.)
  `if`(analysis == "4arm_treat_tg" && orderlevelcond == "gtblock",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt_untreat", "wt_treat",
                                                         "tg_untreat", "tg_treat")),.)
  `if`(analysis == "4arm_treat_ko" && orderlevelcond == "gtblock",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt_untreat", "wt_treat",
                                                         "ko_untreat", "ko_treat")),.)
  `if`(analysis == "4arm_sd_tg" && orderlevelcond == "etblock",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt_hc", "tg_hc", "wt_sd", "tg_sd")),.)
  `if`(analysis == "4arm_sd_ko" && orderlevelcond == "etblock",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt_hc", "ko_hc", "wt_sd", "ko_sd")),.)
  `if`(analysis == "4arm_treat_tg" && orderlevelcond == "etblock",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt_untreat", "tg_untreat",
                                                         "wt_treat", "tg_treat")),.)
  `if`(analysis == "4arm_treat_ko" && orderlevelcond == "etblock",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt_untreat", "ko_untreat",
                                                         "wt_treat", "ko_treat")),.)

  `if`(analysis == "2arm_tg",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt", "tg")),.)
  `if`(analysis == "2arm_ko",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("wt", "ko")),.)
  `if`(analysis == "2arm_sd",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("hc", "sd")),.)
  `if`(analysis == "2arm_treat",
       data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                              levels = c("untreat", "treat")),.)
  
  #return amended dataframe
  return(data.animal.joined)
}
