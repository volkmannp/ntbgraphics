heatmapexp <- function(directory, 
                       analysis = c("2arm_ko","2arm_tg", "2arm_sd", "2arm_treat",
                                    "4arm_sd_ko", "4arm_sd_tg", "4arm_treat_ko", "4arm_treat_tg"),
                       ordercolumns = c("ntb", "rdoc", "manual"),
                       ordercolumns_manual,
                       exclude.animals = FALSE,
                       orderlevelcond = c("other", "gtblock", "etblock"),
                       naomit = TRUE,
                       directional = FALSE,
                       absoluteval = FALSE,
                       clustercols = TRUE,
                       clusterrows = TRUE,
                       colorbrewname = "RdYlBu",
                       title = "Heatmap") {
  
  # get data and arrange by condition, select RFIDs as rownames
  data.animal.matrix <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                   exclude.animals, orderlevelcond) %>%
    arrange(.,Condition) %>% 
    column_to_rownames(., "RFID")
  
  # unselect condition and transfrom into matrix, z-scoring
  data.animal.matrix <- data.animal.matrix %>% 
    select(nth(colnames(data.animal.matrix), 2):last(colnames(data.animal.matrix))) %>%
    data.matrix() %>%
    `if`(naomit == TRUE, na.omit(.), .) %>%
    scale()
  
  data.animal.matrix[is.na(data.animal.matrix)] <- 0
  
  # inverse z-scoring accordingly to directionality paradigm
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
  
  # optional take absolute values
  if (absoluteval == TRUE) {
    data.animal.matrix <- abs(data.animal.matrix)
  }
  
  # prepare annotation table and colors by analysis type
  if (analysis == "2arm_tg") {
    annotation <- list(Condition=(c(
      wt = "#CCECE6",
      tg = "#238B45")))
    data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                     exclude.animals, orderlevelcond) %>%
      select(., RFID, Condition) %>%
      column_to_rownames(., "RFID")
  } else if (analysis == "2arm_ko") {
    annotation <- list(Condition=(c(
      wt = "#CCECE6",
      ko = "#238B45")))
    data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                     exclude.animals, orderlevelcond) %>%
      select(., RFID, Condition) %>%
      column_to_rownames(., "RFID")
  } else if (analysis == "2arm_sd") {
    annotation <- list(Condition=(c(
      hc = "#CCECE6",
      sd = "#238B45")))
    data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                     exclude.animals, orderlevelcond) %>%
      select(., RFID, Condition) %>%
      column_to_rownames(., "RFID")
  } else if (analysis == "2arm_treat") {
    annotation <- list(Condition=(c(
      untreat = "#CCECE6",
      treat = "#238B45")))
    data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                     exclude.animals, orderlevelcond) %>%
      select(., RFID, Condition) %>%
      column_to_rownames(., "RFID")
   } else if (orderlevelcond == "gtblock") {
      annotation <- list(Condition=(c(
        wt_hc="#F7FCFD",
        wt_sd="#CCECE6",
        tg_hc="#238B45",
        tg_sd="#00441B")))
      data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual,  
                                       exclude.animals, orderlevelcond) %>%
        select(., RFID, Condition) %>%
        column_to_rownames(., "RFID")
    } else if (orderlevelcond == "etblock") {
      annotation <- list(Condition=(c(
        wt_hc="#F7FCFD",
        tg_hc="#238B45",
        wt_sd="#CCECE6",
        tg_sd="#00441B")))
      data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual,
                                       exclude.animals, orderlevelcond) %>%
        select(., RFID, Condition) %>%
        column_to_rownames(., "RFID")
  } else if (orderlevelcond == "other") {
    annotation <- list(Condition=(c(
      tg_hc="#238B45",
      tg_sd="#00441B",
      wt_hc="#F7FCFD",
      wt_sd="#CCECE6")))
    data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                     exclude.animals, orderlevelcond) %>%
      select(., RFID, Condition) %>%
      column_to_rownames(., "RFID")
  }
  
  # define legend label and size
  if(absoluteval == FALSE) {
    legend_labels = c("-3", "-1.5", "0", "+1.5", "+3", "+4.5")
  }
  if(absoluteval == TRUE) {
    legend_labels = c("0", "0.9", "+1.8", "2.7", "+3.6", "+4.5")
  }
  if(absoluteval == FALSE) {
    legend_breaks = seq(-3, 4.5, by = 1.5)
  }
  if(absoluteval == TRUE) {
    legend_breaks = seq(0, 4.5, by = 0.9)
  }
  
  # heatmapping of experiments
  pheatmap(data.animal.matrix,
           main = paste(title),
           fontsize = 12,
           fontsize_col = 10,
           show_rownames = F,
           treeheight_row = 43,
           cluster_cols = clustercols,
           cluster_rows = clusterrows,
           clustering_distance_rows = "correlation",
           color = colorRampPalette(rev(brewer.pal(n = 11, name = colorbrewname)))
            (length(seq(0, 20, by = 1))),
           legend_breaks = legend_breaks,
           legend_labels = legend_labels,
           border_color = F,
           annotation_row = data.animal.joined,
           annotation_colors = annotation)
  
  return(data.animal.matrix)
}


heatmapexp(directory = paste0(system.file("extdata", package = "ntbgraphics",
                                                                mustWork = T),"/"),
           analysis = "4arm_sd_tg",
           ordercolumns = "ntb",
           exclude.animals = FALSE,
           orderlevelcond = "gtblock",
           naomit = FALSE,
           directional = FALSE,
           absoluteval = FALSE,
           clustercols = TRUE,
           clusterrows = FALSE,
           colorbrewname = "Blues",
           title = "Example Data Heatmap")

# color for naomit!!!




