#' @title Prepare data of NTB experiments for Heatmapping
#'
#' @author Paul Volkmann
#'
#' @name prepareheatmap
#'
#' @description A function that prepares NTB data for heatmap analysis including z-scoring.
#'
#' @param 'dataframe': dataframe of animals and corresponding NTB experiments (e.g. output of 'getexpdata')
#'
#' @return prepared matrix of z-scored behavioral measures
#'
#' @export
#'
#' @example
#' prepareheatmap(data.animal.joined)
#'
#' data.animal.joined <- getexpdata(paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"))
#' data.animal.matrix <- prepareheatmap(data.animal.joined)


prepareheatmap <- function(dataframe) {
  data.animal.matrix <- dataframe %>%
    column_to_rownames(., "RFID") %>%
    select(Meanspeed:SerialLearn) %>%
    data.matrix() %>%
    na.omit %>%
    scale()
}
