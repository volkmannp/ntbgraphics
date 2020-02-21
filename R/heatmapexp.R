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
#' @param ordercolumns defines the order paradigm of experiment column appearance in final heatmap (if
#' clustercolumns is set FALSE) within quotation marks: "ntb", "rdoc", "manual";
#' order of experiments may be chronological with "ntb", follow RDoC clustering with "rdoc" or be customized
#' manually with "manual" (-> use 'ordercolumns_manual' for exact appearance; there, you may also choose to 
#' exclude experiments);
#' default: "ntb"
#' @param ordercolumns_manual customizes order of appearance and appearance itself of experiment columns 
#' in final heatmap (experiments that are not listed will not be included);
#' only if 'ordercolumns' = "manual";
#' user has to provide a vector containing characters within quotation marks (e.g. by using 
#' c("Meanspeed", "SerialLearn")) with all experiments he wants to include into the final heatmap with desired
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
#' defines order of data grouped by condition in final heatmap if clusterrows is set FALSE:
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
#' @param return.matrix,mean boolean that specifies if heatmap should only contain the mean of each group
#' for each experiment; grouping follows specification of groups to be analyzed as defined by 'analysis';
#' default: FALSE
#' @param healthy_norm boolean that specifies if mean matrix should be normalized to healthy controls by
#' subtracting all values by the healthy controls;
#' only if return.matrix and return.matrix.mean are TRUE; not possible for 2arm experiments;
#' default: FALSE
#' @param directional boolean that specifies if directionality paradigm following RDoC concept should be
#' applied; if TRUE columns 'Rotations', 'FreezeBase', 'Timeimmobile', 'Baseline', 'Activity', 'Choices' and
#' 'Meanspeed' are multiplied by -1; only applied and useful if 'absoluteval' is FALSE;
#' default: FALSE
#' @param absoluteval boolean that specifies if only absolute values of z-scored matrix should be given in
#' heatmap;
#' default: FALSE
#' @param clustercols boolean that determines if columns should be clustered;
#' default: TRUE
#' @param clusterrows boolean that determines if rows should be clustered;
#' default: TRUE
#' @param cutree_cols defines number of clusters the columns are divided into, based on the hierarchical 
#' clustering;
#' only if clustercols is TRUE;
#' default: 1
#' @param cutree_rows defines number of clusters the rows are divided into, based on the hierarchical 
#' clustering;
#' only if clusterrows is TRUE;
#' default: 1
#' @param palette specifies the color package to choose from for usage of palettes (find options below) for
#' color design of the heatmap within quotation marks;
#' available are: "cRP" (colorRampPalette from RColorBrewer), "viridis" (from viridis), "spaced" (from
#' ntbgraphics) for own customizable diverging color palette;
#' default: "cRP"
#' @param colorbrewname specifies the color palette used for drawing the heatmap within quotation marks;
#' only if palette is "cRP";
#' you may check your options with 'display.brewer.all()';
#' examples include: "YlOrRd", "YlGn", "Purples", "OrRd", "Greys", "Set1", "Pastel1", "Paired", "Spectral",
#' "RdYlBu" or "BrBG" and many more;
#' default: "PuOr"
#' @param viridisname specifies the color palette used for drawing the heatmap without (!) quotation marks;
#' only if palette is "viridis";
#' you may check out the five available options by just trying out;
#' available are: viridis, magma, plasma, inferno, cividis;
#' default: inferno
#' @param color_spaced1 specifies color1 of own diverging color palette within quotation marks;
#' only if palette is "spaced";
#' default: "mediumpurple"
#' @param color_spaced2 specifies color2 of own diverging color palette within quotation marks;
#' only if palette is "spaced";
#' default: "tan"
#' @param title defines the title of the heatmap; character within quotation marks;
#' default: "Heatmap"
#' @param saveplotdir file directory where to save heatmap within quotation marks;
#' you may set to FALSE if you do not want to save heatmap to PDF;
#' default: location of Behavior and Animal List files as specified in 'directory'
#'
#' @return heatmap and data matrix
#'
#' @export
#'
#' @examples heatmapexp(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
#'
#' @examples heatmapexp(directory = paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/")),
#'                      analysis = "4arm_sd_tg",
#'                      ordercolumns = "rdoc",
#'                      exclude.animals = c("900200000070142"),
#'                      orderlevelcond = "gtblock",
#'                      acceptable.nas = 0,
#'                      directional = TRUE,
#'                      clustercols = FALSE,
#'                      clusterrows = FALSE,
#'                      palette = "viridis",
#'                      viridisname = inferno,
#'                      title = "new_testdata_heatmap_09-04-2044",
#'                      saveplotdir = FALSE)


heatmapexp <- function(directory, 
                       analysis = c("2arm_ko","2arm_tg", "2arm_sd", "2arm_treat",
                                    "4arm_sd_ko", "4arm_sd_tg", "4arm_treat_ko", "4arm_treat_tg"),
                       ordercolumns = c("ntb", "rdoc", "manual"),
                       ordercolumns_manual,
                       exclude.animals = FALSE,
                       orderlevelcond = c("other", "gtblock", "etblock"),
                       acceptable.nas = "unlimited",
                       return.matrix.mean = FALSE,
                       healthy_norm = FALSE,
                       directional = FALSE,
                       absoluteval = FALSE,
                       clustercols = TRUE,
                       clusterrows = TRUE,
                       cutree_cols = 1,
                       cutree_rows = 1,
                       palette = c("cRP", "viridis", "spaced"),
                       colorbrewname = "PuOr",
                       viridisname = inferno,
                       color_spaced1 = "mediumpurple",
                       color_spaced2 = "tan",
                       title = "Heatmap",
                       saveplotdir = directory) {
        
        # ensure that in case of no provided argument, first one of list is taken
        palette <- palette[1]
        
        # check if saveplotdir exists
        if (saveplotdir != FALSE && dir.exists(saveplotdir) == FALSE) {
                stop(sprintf("The path for saving the heatmap as specified in saveplotdir `%s` does not exist!", 
                 saveplotdir))
        }
        
        # get data
        data.animal.matrix <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                         exclude.animals, orderlevelcond, acceptable.nas, return.matrix = T,
                                         return.matrix.mean, healthy_norm, naomit = FALSE, directional, 
                                         absoluteval) 
        
        # prepare annotation table and colors of groups by analysis type
        if (return.matrix.mean == TRUE && analysis == "4arm_sd_tg") {
                data.animal.joined <- matrix(c("wt_hc", "wt_sd", "tg_hc", "tg_sd",
                                               "wt_hc_mean", "wt_sd_mean", "tg_hc_mean",  "tg_sd_mean"), 
                                             nrow = 4, ncol =2)
                data.animal.joined <- as.data.frame(data.animal.joined)
                data.animal.joined <- column_to_rownames(data.animal.joined, "V1")
                names(data.animal.joined)[1] <- "Condition"
                annotation <- list(Condition=(c(
                        wt_hc_mean="#b4b4b4",
                        wt_sd_mean="#3c3c3c",
                        tg_hc_mean="#84dcff",
                        tg_sd_mean="#1e24fc")))
        } else if (return.matrix.mean == TRUE && analysis == "4arm_sd_ko") {
                data.animal.joined <- matrix(c("wt_hc", "wt_sd", "ko_hc", "ko_sd",
                                               "wt_hc_mean", "wt_sd_mean", "ko_hc_mean",  "ko_sd_mean"), 
                                             nrow = 4, ncol =2)
                data.animal.joined <- as.data.frame(data.animal.joined)
                data.animal.joined <- column_to_rownames(data.animal.joined, "V1")
                names(data.animal.joined)[1] <- "Condition"
                annotation <- list(Condition=(c(
                        wt_hc_mean="#b4b4b4",
                        wt_sd_mean="#3c3c3c",
                        ko_hc_mean="#84dcff",
                        ko_sd_mean="#1e24fc")))
        } else if (return.matrix.mean == TRUE && analysis == "4arm_treat_tg") {
                data.animal.joined <- matrix(c("wt_untreat", "wt_treat", "tg_untreat", "tg_treat",
                                               "wt_untreat_mean", "wt_treat_mean", 
                                               "tg_untreat_mean",  "tg_treat_mean"), 
                                             nrow = 4, ncol =2)
                data.animal.joined <- as.data.frame(data.animal.joined)
                data.animal.joined <- column_to_rownames(data.animal.joined, "V1")
                names(data.animal.joined)[1] <- "Condition"
                annotation <- list(Condition=(c(
                        wt_untreat_mean="#b4b4b4",
                        wt_treat_mean="#3c3c3c",
                        tg_untreat_mean="#84dcff",
                        tg_treat_mean="#1e24fc")))
        } else if (return.matrix.mean == TRUE && analysis == "4arm_treat_ko") {
                data.animal.joined <- matrix(c("wt_untreat", "wt_treat", "ko_untreat", "ko_treat",
                                               "wt_untreat_mean", "wt_treat_mean", 
                                               "ko_untreat_mean",  "ko_treat_mean"), 
                                             nrow = 4, ncol =2)
                data.animal.joined <- as.data.frame(data.animal.joined)
                data.animal.joined <- column_to_rownames(data.animal.joined, "V1")
                names(data.animal.joined)[1] <- "Condition"
                annotation <- list(Condition=(c(
                        wt_untreat_mean="#b4b4b4",
                        wt_treat_mean="#3c3c3c",
                        ko_untreat_mean="#84dcff",
                        ko_treat_mean="#1e24fc")))
                
        # define order of groups by analysis type
        } else if (analysis == "2arm_tg") {
                annotation <- list(Condition=(c(
                        wt = "#3c3c3c",
                        tg = "#84dcff")))
                data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                                 exclude.animals, orderlevelcond) %>%
                        select(., RFID, Condition) %>%
                        column_to_rownames(., "RFID")
        } else if (analysis == "2arm_ko") {
                annotation <- list(Condition=(c(
                        wt = "#3c3c3c",
                        ko = "#84dcff")))
                data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                                 exclude.animals, orderlevelcond) %>%
                        select(., RFID, Condition) %>%
                        column_to_rownames(., "RFID")
        } else if (analysis == "2arm_sd") {
                annotation <- list(Condition=(c(
                        hc = "#3c3c3c",
                        sd = "#84dcff")))
                data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                                 exclude.animals, orderlevelcond) %>%
                        select(., RFID, Condition) %>%
                        column_to_rownames(., "RFID")
        } else if (analysis == "2arm_treat") {
                annotation <- list(Condition=(c(
                        untreat = "#3c3c3c",
                        treat = "#84dcff")))
                data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual, 
                                                 exclude.animals, orderlevelcond) %>%
                        select(., RFID, Condition) %>%
                        column_to_rownames(., "RFID")
        } else if (orderlevelcond == "gtblock") {
                annotation <- list(Condition=(c(
                        wt_hc="#b4b4b4",
                        wt_sd="#3c3c3c",
                        tg_hc="#84dcff",
                        tg_sd="#1e24fc")))
                data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual,  
                                                 exclude.animals, orderlevelcond) %>%
                        select(., RFID, Condition) %>%
                        column_to_rownames(., "RFID")
        } else if (orderlevelcond == "etblock") {
                annotation <- list(Condition=(c(
                        wt_hc="#b4b4b4",
                        tg_hc="#84dcff",
                        wt_sd="#3c3c3c",
                        tg_sd="#1e24fc")))
                data.animal.joined <- getexpdata(directory, analysis, ordercolumns, ordercolumns_manual,  
                                                 exclude.animals, orderlevelcond) %>%
                        select(., RFID, Condition) %>%
                        column_to_rownames(., "RFID")
        } else if (orderlevelcond == "other") {
                annotation <- list(Condition=(c(
                        tg_hc="#84dcff",
                        tg_sd="#1e24fc",
                        wt_hc="#b4b4b4",
                        wt_sd="#3c3c3c")))
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
        
        # specify palette to use for colors
        if (palette == "cRP") {
                color_spec = colorRampPalette(
                        rev(brewer.pal(n = 11, name = colorbrewname)))(length(seq(0, 20, by = 1)))
        }
        if (palette == "viridis") {
                color_spec = viridisname(n = 21, begin = 0.15, end = 1)
        }
        if (palette == "spaced") {
                color_spec <- colordiverger(color1 = color_spaced1, color2 = color_spaced2,
                                            min.val = -5, max.val = 5)
        }
        
        # introducing breaks if columns are ordered following rdoc and not being clustered
        if(ordercolumns == "rdoc") {
                gapscol = c(5, 8, 12, 16)
        } else if (ordercolumns != "rdoc") {
                gapscol = FALSE
        }
        
        # heatmapping of experiments
        heatmap <- pheatmap(data.animal.matrix,
                            main = paste(title),
                            fontsize = 16,
                            fontsize_col = 12,
                            show_rownames = F,
                            treeheight_row = 43,
                            cluster_cols = clustercols,
                            cluster_rows = clusterrows,
                            cutree_cols = cutree_cols,
                            cutree_rows = cutree_rows,
                            gaps_col = gapscol,
                            clustering_distance_rows = "manhattan",
                            clustering_distance_cols = "manhattan",
                            color = color_spec,
                            legend_breaks = legend_breaks,
                            legend_labels = legend_labels,
                            border_color = F,
                            annotation_row = data.animal.joined,
                            annotation_colors = annotation)
        
        if (saveplotdir != FALSE) {
                pdf(paste0(saveplotdir, "/Heatmap.pdf"), width = 7, height = 5)
                print(heatmap)
                dev.off()
        }
        
        return(data.animal.matrix)
}
