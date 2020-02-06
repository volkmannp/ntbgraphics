#' @title Heatmapping of NTB experiment dataset (2-arm)
#'
#' @author Paul Volkmann
#'
#' @name heatmapexp_2arm
#'
#' @description A function that produces a heatmap for an NTB 2-arm dataset. (Warning: This function relies on function 'getexpdata_2arm'!)
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
#' heatmapexp_2arm(data.animal.matrix, "/Users/username/Documents/Experiments/Cohort_1/")
#'
#' data.animal.joined <- getexpdata_2arm(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
#' data.animal.matrix <- prepareheatmap(data.animal.joined)
#' pheatmapout <- heatmapexp_2arm(data.animal.matrix, paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))


heatmapexp_2arm <- function(datamatrix, directory, title) {
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
           annotation_row = getexpdata_2arm(directory) %>%
             dplyr::select(RFID, Genotype) %>%
             column_to_rownames("RFID"),
           annotation_colors = list(Genotype=c(tg = "#238B45",wt = "#00441B")))
}
