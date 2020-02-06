#' @title Heatmapping of NTB experiment dataset
#'
#' @author Paul Volkmann
#'
#' @name heatmapexp
#'
#' @description A function that produces a heatmap for an NTB dataset. (Warning: This function relies on function 'getexpdata'!)
#'
#' @param 'datamatrix': matrix of animals and corresponding NTB experiments (e.g. output of 'prepareheatmap')
#' @param 'directory': file directory of Animal List file
#' @param 'title': define the title of your heatmap
#'
#' @return heatmap
#'
#' @export
#'
#' @example
#' heatmapexp(data.animal.matrix, "/Users/username/Documents/Experiments/Cohort_1/")
#'
#' data.animal.joined <- getexpdata(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
#' data.animal.matrix <- prepareheatmap(data.animal.joined)
#' pheatmapout <- heatmapexp(data.animal.matrix, paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))


heatmapexp <- function(datamatrix, directory, title) {
        pheatmap(datamatrix,
                  main = paste(title),
                  fontsize = 12,
                  fontsize_col = 10,
                  show_rownames = F,
                  cluster_cols = T,
                  treeheight_row = 43,
                  cluster_rows = T,
                  clustering_distance_rows = "correlation",
                  color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdYlBu")))(length(seq(0, 20, by = 1))),
                  legend_breaks = seq(-3, 4.5, by = 1.5),
                  legend_labels = c("-3", "-1.5", "0", "+1.5", "+3", "+4.5"),
                  border_color = F,
                  annotation_row = getexpdata(directory) %>%
                                        dplyr::select(RFID, GT_Env) %>%
                                        column_to_rownames("RFID"),
                  annotation_colors = list(GT_Env=(c(
                                                 tg_hc="#238B45",
                                                 tg_sd="#00441B",
                                                 wt_hc="#F7FCFD",
                                                 wt_sd="#CCECE6"))))
}
