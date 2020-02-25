#' @title Get data of NTB experiments in customized format
#'
#' @author Paul Volkmann
#'
#' @name getexpdata
#'
#' @description A function that imports an NTB dataset and prepares the data for plotting and analysis as 
#' dataframe or matrix.
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
#' @param ordercolumns defines the order paradigm of experiment column appearance in final table within 
#' quotation marks: "ntb", "rdoc", "manual";
#' RFID and Condition are always listed first and need no specification;
#' order of experiments may be chronological with "ntb", follow RDoC clustering with "rdoc" or be customized
#' manually with "manual" (-> use 'ordercolumns_manual' for exact appearance; there, you may also choose to 
#' exclude experiments);
#' default: "ntb"
#' @param ordercolumns_manual customizes order of appearance and appearance itself of experiment columns 
#' in final table (experiments that are not listed will not be included);
#' only if 'ordercolumns' = "manual";
#' user has to provide a vector containing characters within quotation marks (e.g. by using 
#' c("Meanspeed", "SerialLearn")) with all experiments he wants to include into the final tabel with desired
#' order;
#' no need for specification if 'ordercolumns' is not "manual"
#' default: FALSE
#' @param exclude.animals excluding animals from analysis by RFID;
#' user has to provide a vector containing characters within quotation marks (e.g. by using 
#' c("900200000067229", "900200000065167")) with all animals he wants to exclude from the final table;
#' if FALSE is provided, no animal will be excluded;
#' default: FALSE
#' @param orderlevelcond defines order of factor levels of conditions within quotation marks:
#' "other", "gtblock", "etblock", "2rev";
#' (might be important when it comes to plotting or displaying your data grouped by condition 
#' in a defined order):
#' "other" for alphabetical order in case of 4arm; also for default order of 2arm experiments
#' (which lists the 'control' first, then the 'condition');
#' "gtblock" for order wt_x, wt_y, tg_x, tg_y;
#' "etblock" for order x_hc, y_hc, x_sd, y_sd;
#' "2rev" for inverse order of 2arm default only, meaning listing the 'condition' first, then the 'control';
#' default: "other"
#' @param acceptable.nas defines the maximum number of NAs allowed within the same row;
#' if number of actual NAs within one row is bigger than the number provided, the row will be excluded from 
#' table and following analyses;
#' if the number of acceptable NAs should be unlimited, no value has to be provided;
#' default: "unlimited"
#' @param return.matrix boolean that defines if the standard dataframe or a z-scored matrix should be 
#' provided;
#' by default, getexpdata generates a dataframe containing raw joined animal and experiment information;
#' 'return.matrix' can further process the dataframe with customizable functions to return a z-scored matrix,
#' for e.g. heatmapping, pca and tsne;
#' default: FALSE
#' @param return.matrix,mean boolean that specifies if matrix should only contain the mean of each group
#' for each experiment; grouping follows specification of groups to be analyzed as defined by 'analysis';
#' only useful if 'return.matrix' is TRUE;
#' default: FALSE
#' @param healthy_norm boolean that specifies if mean matrix should be normalized to healthy controls by
#' subtracting all values by the healthy controls;
#' only if return.matrix and return.matrix.mean are TRUE; not possible for 2arm experiments;
#' default: FALSE
#' @param naomit boolean that specifies if each columns with any number of NAs bigger than 0 should be 
#' excluded; only applied and useful if 'return.matrix' is TRUE;
#' may appear redundant concerning earlier listed 'acceptable.nas', but gives user the opportunity, to save
#' settings within function with different needs for dataframe and (probably later needed) matrix;
#' default: FALSE
#' @param directional boolean that specifies if directionality paradigm following RDoC concept should be
#' applied; if TRUE, columns 'Rotations', 'FreezeBase', 'Timeimmobile', 'Baseline', 'Activity', 'Choices' and
#' 'Meanspeed' are multiplied by -1; 
#' additional option to set to "emptcf4" (within quotation marks!), if you selectively want to multiply 
#' columns 'Center', 'Choices' and 'Meanspeed';
#' only applied if 'return.matrix' is TRUE and only useful if 'absoluteval' is FALSE;
#' default: FALSE
#' @param absoluteval boolean that specifies if only absolute values of z-scored matrix should be given;
#' only applied and useful if 'return.matrix' is TRUE;
#' default: FALSE
#'
#' @return prepared and joined dataframe of all animals and corresponding NTB experiments 
#' or customized z-scored matrix
#'
#' @export
#' 
#' @examples getexpdata(directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"))
#'
#' @examples getexpdata(directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"), 
#'                    analysis = "2arm_sd", 
#'                    ordercolumns = "manual", 
#'                    ordercolumns_manual = c("Meanspeed", "SerialLearn", "Center"), 
#'                    exclude.animals = c("900200000070142"), 
#'                    orderlevelcond = "2rev", 
#'                    acceptable.nas = 3, 
#'                    return.matrix = TRUE, 
#'                    naomit = TRUE, 
#'                    directional = TRUE)


getexpdata <- function(directory, 
                       analysis = c("2arm_ko","2arm_tg", "2arm_sd", "2arm_treat",
                                    "4arm_sd_ko", "4arm_sd_tg", "4arm_treat_ko", "4arm_treat_tg"),
                       ordercolumns = c("ntb", "rdoc", "manual"),
                       ordercolumns_manual = FALSE,
                       exclude.animals = FALSE,
                       orderlevelcond = c("other", "gtblock", "etblock", "2rev"),
                       acceptable.nas = "unlimited",
                       return.matrix = FALSE,
                       return.matrix.mean = FALSE,
                       healthy_norm = FALSE,
                       naomit = FALSE,
                       directional = FALSE,
                       absoluteval = FALSE) {
  
  ### directional = "emptcf4" is hidden <<<<<<<<<<<<
  
  ### use switch() for more flexible level assignments and assert_that() for more complex error management
  
  # turn warnings off
  options(warn=-1)
  
  # check if directory is provided and if it exists
  if (missing(directory)) {
    stop("Please provide path to 'Meta Behavior' and 'Animal List' files!")
  } else if (dir.exists(directory) == FALSE) {
    stop(sprintf("The path `%s` does not exist!", directory))
  } 
  # check for data file
  if (file.exists(paste0(directory,"/Meta Behavior.xlsx")) == FALSE |
      file.exists(paste0(directory,"/Animal List.xlsx")) == FALSE) {
    stop(sprintf("Path `%s` does not contain one of or both input excel files!", directory))
  }
  
  # ensure that in case of no provided argument, first one of list is taken
  analysis <- analysis[1]
  ordercolumns <- ordercolumns[1]
  orderlevelcond <- orderlevelcond[1]
  
  # ensure that correct analysis is provided
  if(analysis == "2arm_ko") {
    print("Warning: You have chosen '2arm_ko' as type of analysis. Since this is the default setting, please make sure it matches the data provided. Furthermore, refer to the help page '?getexpdata' to check available options!")
  }
  
  ## import data
  suppressMessages(meta.data <-  readxl::read_excel(paste0(directory,"/Meta Behavior.xlsx")))
  suppressMessages(animal.list <-  readxl::read_excel(paste0(directory, "/Animal List.xlsx")))
  
  # ensure that Animal is a character - important for joining both tables
  meta.data <- meta.data %>% 
    mutate_at(., vars("Animal"),list(as.character))
  
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
    # ensure that RFID is a character - important for joining both tables
    mutate_at(., vars("RFID"),list(as.character)) %>% 
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
  if (analysis == "4arm_sd_tg" && orderlevelcond == "gtblock") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt_hc", "wt_sd", "tg_hc", "tg_sd"))
  }
  if (analysis == "4arm_sd_ko" && orderlevelcond == "gtblock") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt_hc", "wt_sd", "ko_hc", "ko_sd"))
  }
  if (analysis == "4arm_treat_tg" && orderlevelcond == "gtblock") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt_untreat", "wt_treat",
                                                      "tg_untreat", "tg_treat"))
  }
  if (analysis == "4arm_treat_ko" && orderlevelcond == "gtblock") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt_untreat", "wt_treat",
                                                      "ko_untreat", "ko_treat"))
  }
  if (analysis == "4arm_sd_tg" && orderlevelcond == "etblock") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt_hc", "tg_hc", "wt_sd", "tg_sd"))
  }
  if (analysis == "4arm_sd_ko" && orderlevelcond == "etblock") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt_hc", "ko_hc", "wt_sd", "ko_sd"))
  }
  if (analysis == "4arm_treat_tg" && orderlevelcond == "etblock") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt_untreat", "tg_untreat",
                                                      "wt_treat", "tg_treat"))
  }
  if (analysis == "4arm_treat_ko" && orderlevelcond == "etblock") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt_untreat", "ko_untreat",
                                                      "wt_treat", "ko_treat"))
  }
  
  if (analysis == "2arm_tg") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt", "tg"))
  }
  if (analysis == "2arm_ko") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("wt", "ko"))
  }
  if (analysis == "2arm_sd") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("hc", "sd"))
  }
  if (analysis == "2arm_treat") {
    data.animal.joined$Condition <- factor(data.animal.joined$Condition,
                                           levels = c("untreat", "treat"))
  }
  
  if (orderlevelcond == "2rev") {
    if (analysis == "2arm_tg") {
      data.animal.joined$Condition <- factor(data.animal.joined$Condition, levels = c("tg", "wt"))
    } else if (analysis == "2arm_ko") {
      data.animal.joined$Condition <- factor(data.animal.joined$Condition, levels = c("ko", "wt"))
    } else if (analysis == "2arm_sd") {
      data.animal.joined$Condition <- factor(data.animal.joined$Condition, levels = c("sd", "hc"))
    } else if(analysis == "2arm_treat") {
      data.animal.joined$Condition <- factor(data.animal.joined$Condition, levels = c("treat", "untreat"))
    }
  }
  
  # exclude columns containing certain amount of NAs (counts number of NAs per columns, saves information
  # in new column na_count, filters regarding value in this columns na_count, drops columns na_count)
  data.animal.joined$na_count <- rowSums(is.na(data.animal.joined))
  data.animal.joined <- data.animal.joined %>% 
    filter(na_count <= paste(acceptable.nas)) %>% 
    select(-na_count) %>% 
    # finally, arrange by condition
    arrange(.,Condition)
  
  # option for creating matrix with different possible parameters
  if(return.matrix == TRUE) {
    
    # standard matrix
    if (return.matrix.mean == FALSE) {
      # arrange by condition and transform column RFID to rownames
      data.animal.matrix <- data.animal.joined %>%
        column_to_rownames(., "RFID")
      
      # unselect condition and transfrom into matrix, z-scoring
      data.animal.matrix <- data.animal.matrix %>% 
        select(nth(colnames(data.animal.matrix), 2):last(colnames(data.animal.matrix))) %>%
        data.matrix() %>%
        `if`(naomit == TRUE, na.omit(.), .) %>%
        scale()
      
      # set NAs to zero
      data.animal.matrix[is.na(data.animal.matrix)] <- 0
    }
    
    # matrix containing means for every group only
    if (return.matrix.mean == TRUE) {
      length.col <- data.animal.joined %>% 
        colnames() %>% 
        length() %>% 
        as.numeric()
      
      data.animal.matrix <- aggregate(data.animal.joined[, 3:length.col],
                                      list(data.animal.joined$Condition), mean, na.rm = T)
      
      data.animal.matrix <- data.animal.matrix %>% 
        data.frame() %>% 
        column_to_rownames("Group.1") %>% 
        data.matrix() %>% 
        scale()
      
      # optionally subtract the wt_hc values from all other values
      if (healthy_norm == TRUE && analysis == "4arm_sd_ko") {
        data.animal.matrix <- sweep(data.animal.matrix, 2, data.animal.matrix["wt_hc",], "-")
      }
      if (healthy_norm == TRUE && analysis == "4arm_sd_tg") {
        data.animal.matrix <- sweep(data.animal.matrix, 2, data.animal.matrix["wt_hc",], "-")
      }
      if (healthy_norm == TRUE && analysis == "4arm_treat_ko") {
        data.animal.matrix <- sweep(data.animal.matrix, 2, data.animal.matrix["wt_untreat",], "-")
      }
      if (healthy_norm == TRUE && analysis == "4arm_treat_tg") {
        data.animal.matrix <- sweep(data.animal.matrix, 2, data.animal.matrix["wt_untreat",], "-")
      }
    }
    
    # inverse z-scoring according to directionality paradigm
    col.names.actual <- colnames(data.animal.matrix)
    
    if ('Rotations' %in% col.names.actual == TRUE && directional == TRUE) {
      data.animal.matrix[, "Rotations"] <- data.animal.matrix[, "Rotations"]*-1
    }
    if ('FreezeBase' %in% col.names.actual == TRUE && directional == TRUE) {
      data.animal.matrix[, "FreezeBase"] <- data.animal.matrix[, "FreezeBase"]*-1
    }
    if ('Timeimmobile' %in% col.names.actual == TRUE && directional == TRUE) {
      data.animal.matrix[, "Timeimmobile"] <- data.animal.matrix[, "Timeimmobile"]*-1
    }
    if ('Baseline' %in% col.names.actual == TRUE && directional == TRUE) {
      data.animal.matrix[, "Baseline"] <- data.animal.matrix[, "Baseline"]*-1
    }
    if ('Activity' %in% col.names.actual == TRUE && directional == TRUE) {
      data.animal.matrix[, "Activity"] <- data.animal.matrix[, "Activity"]*-1
    }
    if ('Choices' %in% col.names.actual == TRUE && directional == TRUE) {
      data.animal.matrix[, "Choices"] <- data.animal.matrix[, "Choices"]*-1
    }
    if ("Meanspeed" %in% col.names.actual == TRUE && directional == TRUE) {
      data.animal.matrix[, "Meanspeed"] <- data.animal.matrix[, "Meanspeed"]*-1
    }
    
    # inverse z-scoring according to empirical Tcf4 paradigm
    col.names.actual <- colnames(data.animal.matrix)
    
    if ('Center' %in% col.names.actual == TRUE && directional == "emptcf4") {
      data.animal.matrix[, "Center"] <- data.animal.matrix[, "Center"]*-1
    }
    if ('Choices' %in% col.names.actual == TRUE && directional == "emptcf4") {
      data.animal.matrix[, "Choices"] <- data.animal.matrix[, "Choices"]*-1
    }
    if ("Meanspeed" %in% col.names.actual == TRUE && directional == "emptcf4") {
      data.animal.matrix[, "Meanspeed"] <- data.animal.matrix[, "Meanspeed"]*-1
    }
    
    # optionally take absolute values
    if (absoluteval == TRUE) {
      data.animal.matrix <- abs(data.animal.matrix)
    }
    
  }
  
  # return amended dataframe
  if(return.matrix == FALSE) {
    return(data.animal.joined)
  }
  # return amended matrix
  if(return.matrix == TRUE) {
    return(data.animal.matrix)
  }
  # turn warnings back on
  options(warn=0)
}
