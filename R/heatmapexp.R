#' @title Heatmapping of NTB experiment dataset
#'
#' @author Paul Volkmann
#'
#' @name heatmapexp
#'
#' @description A function that produces a heatmap for an NTB dataset and a data matrix with z-scored values.
#' (Requires function 'getexpdata' internally.)
#' For right formatting of your files, please consider the "ReadMe for ntbgraphics".
#'
#' @param 'directory': file directory of Behavior and Animal List files
#' @param 'analysis': specifying the kind of experiment performed - 4-arm or 2-arm
#' with either transgenic or knock-out animals or with social defeat looling at environmental condition
#' as group of interest
#' (respectively, choosing the kind of analysis preferred)
#' (default: "4arm")
#' @param 'clustercols': determine if columns should be clustered (boolean)
#' @param 'clusterrows': determine if rows should be clustered (boolean)
#' (default: TRUE)
#' @param 'orderplots': only, if 'clusterrows' = FALSE and analysis = "4arm";
#' defines order of plots with:
#' "other" for alphabetical order;
#' "gtblock" for order wt_hc, wt_sd, tg_hc, tg_sd;
#' "envblock" for order wt_hc, tg_hc, wt_sd, tg_sd;
#' for analysis = "2arm_x", order always is wt, x
#' (default: "other")
#' @param 'title': define the title of your heatmap (default: "Heatmap")
#'
#' @return heatmap and z-scored data matrix
#'
#' @export
#'
#' @example
#' heatmapexp(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
#'
#' data.animal.matrix <- heatmapexp(
#' directory = paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/")),
#' analysis = "2arm_tg",
#' title = "new_testdata_heatmap_09-04-2044")


heatmapexp <- function(directory, 
                       analysis = c("4arm", "2arm_tg", "2arm_ko", "2arm_sd"), 
                       orderplots = c("other", "gtblock", "envblock"),
                       clustercols = TRUE,
                       clusterrows = TRUE,
                       title = "Heatmap") {
        
        data.animal.matrix <- getexpdata(directory, analysis) %>%
                `if`(orderplots == "gtblock", mutate(.,GT_Env = factor(
                        GT_Env, levels = c("wt_hc", "wt_sd", "tg_hc", "tg_sd"))),.) %>%
                `if`(orderplots == "envblock", mutate(.,GT_Env = factor(
                        GT_Env, levels = c("wt_hc", "tg_hc", "wt_sd", "tg_sd"))),.) %>%
                `if`(analysis == "2arm_tg", mutate(.,Genotype = factor(Genotype, 
                                                                       levels = c("wt", "tg"))),.) %>%
                `if`(analysis == "2arm_ko", mutate(.,Genotype = factor(Genotype, 
                                                                       levels = c("wt", "ko"))),.) %>%
                `if`(analysis == "2arm_sd", mutate(.,Environmental = factor(Environmental, 
                                                                       levels = c("hc", "sd"))),.) %>%
                `if`(analysis == "4arm", arrange(.,GT_Env),.) %>%
                `if`(analysis == "2arm_tg", arrange(.,Genotype),.) %>%
                `if`(analysis == "2arm_ko", arrange(.,Genotype),.) %>%
                `if`(analysis == "2arm_sd", arrange(.,Environmental),.) %>%
                column_to_rownames(., "RFID") %>%
                select(Meanspeed:SerialLearn) %>%
                data.matrix() %>%
                na.omit %>%
                scale()
        
        # prepare annotation table and colors by analysis type
        if (orderplots == "other") {
                annotation <- list(GT_Env=(c(
                        tg_hc="#238B45",
                        tg_sd="#00441B",
                        wt_hc="#F7FCFD",
                        wt_sd="#CCECE6")))
                data.animal.joined <- getexpdata(directory, analysis) %>%
                        select(., RFID, GT_Env) %>%
                        column_to_rownames(., "RFID")
        } else if (orderplots == "gtblock") {
                annotation <- list(GT_Env=(c(
                        wt_hc="#F7FCFD",
                        wt_sd="#CCECE6",
                        tg_hc="#238B45",
                        tg_sd="#00441B")))
                data.animal.joined <- getexpdata(directory, analysis) %>%
                        select(., RFID, GT_Env) %>%
                        column_to_rownames(., "RFID")
        } else if (orderplots == "envblock") {
                annotation <- list(GT_Env=(c(
                        wt_hc="#F7FCFD",
                        tg_hc="#238B45",
                        wt_sd="#CCECE6",
                        tg_sd="#00441B")))
                data.animal.joined <- getexpdata(directory, analysis) %>%
                        select(., RFID, GT_Env) %>%
                        column_to_rownames(., "RFID")
        } else if (analysis == "2arm_tg") {
                annotation <- list(Genotype=(c(
                        wt = "#CCECE6",
                        tg = "#238B45")))
                data.animal.joined <- getexpdata(directory, analysis) %>%
                        select(., RFID, Genotype) %>%
                        column_to_rownames(., "RFID")
        } else if (analysis == "2arm_ko") {
                annotation <- list(Genotype=(c(
                        wt = "#CCECE6",
                        ko = "#238B45")))
                data.animal.joined <- getexpdata(directory, analysis) %>%
                        select(., RFID, Genotype) %>%
                        column_to_rownames(., "RFID")
        } else if (analysis == "2arm_sd") {
                annotation <- list(Environmental=(c(
                        hc = "#CCECE6",
                        sd = "#238B45")))
                data.animal.joined <- getexpdata(directory, analysis) %>%
                        select(., RFID, Environmental) %>%
                        column_to_rownames(., "RFID")
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
                 color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdYlBu")))
                 (length(seq(0, 20, by = 1))),
                 legend_breaks = seq(-3, 4.5, by = 1.5),
                 legend_labels = c("-3", "-1.5", "0", "+1.5", "+3", "+4.5"),
                 border_color = F,
                 annotation_row = data.animal.joined,
                 annotation_colors = annotation)
        return(data.animal.matrix)
}