#' @title Create diverging color palette 
#'
#' @author Paul Volkmann
#'
#' @name colordiverger
#'
#' @description A function that takes two colors as its input and produces a color palette for plotting 
#' (z-scored) values (e.g. in a heatmap) with two diverging colors.
#'
#' @param color1 a color of R's predefined color names which is available in 4 graduations with names as 
#' follows for the example of cyan: cyan1, cyan2, cyan3, cyan4; 
#' you may check availability online or within R with 'colors()';
#' color1 defines the color for all negative values;
#' default: "mediumpurple"
#' @param color2 a color with the same demands as defined for color1;
#' color2 defines the color for all positive values;
#' default: "tan"
#' @param min.val provide the minimum value of the dataset the palette will be prepared for;
#' if you set min.val to -5 and max.val to 5, the palette will follow z-score graduations;
#' default: -5
#' @param max.val provide the maximum value of the dataset the palette will be prepared for;
#' default: 5
#' 
#' @return customized diverging color palette
#'
#' @export
#'
#' @examples my_color_palette <- colordiverger("springgreen", "steelblue")


colordiverger <- function(color1 = "mediumpurple", 
                          color2 = "tan",
                          min.val = -5,
                          max.val = 5) {
  
  # turn warnings off
  options(warn=-1)
  
  # define range of data
  data.range <- max.val-min.val
  
  # define color breaks
  brk1 <- c(seq(min.val, min.val+0.219*data.range, by = 0.01))
  brk2 <- c(seq(min.val+0.219*data.range, min.val+0.242*data.range, by = 0.01))
  brk3 <- c(seq(min.val+0.242*data.range, min.val+0.304*data.range, by = 0.01))
  brk4 <- c(seq(min.val+0.304*data.range, min.val+0.336*data.range, by = 0.01))
  brk5 <- c(seq(min.val+0.336*data.range, max.val-0.336*data.range, by = 0.01))
  brk6 <- c(seq(max.val-0.336*data.range, max.val-0.304*data.range, by = 0.01))
  brk7 <- c(seq(max.val-0.304*data.range, max.val-0.242*data.range, by = 0.01))
  brk8 <- c(seq(max.val-0.242*data.range, max.val-0.219*data.range, by = 0.01))
  brk9 <- c(seq(max.val-0.219*data.range, max.val, by = 0.01)) 

  # specify color palette
  colordesign <- c(colorRampPalette(colors = c(paste0(color1, 4), paste0(color1, 3)))(n = length(brk1)),
                 c(colorRampPalette(colors = c(paste0(color1, 3), paste0(color1, 2)))(n = length(brk2)),
                 c(colorRampPalette(colors = c(paste0(color1, 2), paste0(color1, 1)))(n = length(brk3)),
                 c(colorRampPalette(colors = c(paste0(color1, 1), "grey97"))(n = length(brk4)),
                 c(colorRampPalette(colors = c("grey97", "grey97"))(n = length(brk5)),
                 c(colorRampPalette(colors = c("grey97", paste0(color2, 1)))(n = length(brk6)),
                 c(colorRampPalette(colors = c(paste0(color2, 1), paste0(color2, 2)))(n = length(brk7)),
                 c(colorRampPalette(colors = c(paste0(color2, 2), paste0(color2, 3)))(n = length(brk8)),
                 c(colorRampPalette(colors = c(paste0(color2, 3), paste0(color2, 4)))(n = length(brk9)))))))))))
 
  return(colordesign)
  
  # turn warnings back on
  options(warn=0)
}

