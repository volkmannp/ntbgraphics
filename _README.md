---
output:
  html_document:
    code_folding: hide
  pdf_document: default
---
# ReadMe for ntbgraphics
\
This package includes basically two functions:

  - 'getexpdata' for import of the Animal List (containing animal RFIDs, corresponding Genotypes and optionally Environment) and Meta Behavior (containing animals and their behavioral measures) as well as formal preparation;
  - 'ploteachexp' for plotting of experiments of the prepared table and export of a PDF file.
  
The first function ('getexpdata') takes a directory as its input and uses two excel files from this directory ("Animal List" and "Meta Behavior" - mind correct spelling!). It joins the two tables included and creates a column "GT_Env" by uniting "Genotype" and "Environment". Furthermore, it discards all useless information not needed for analysis. Finally, it returns a table called "data.animal.joined".

The second function ('ploteachexp') takes the name of one of the columns of "data.animal.joined", specifically the experiments' names, and again a directory as its input. The data of the chosen column will be plotted and saved in a PDF file in the specified  directory.
\
\
\
As you might have realized, both functions assume a 4-arm experimental setup. For simple 2-arm experiments, two additional functions 'getexpdata_2arm' and 'ploteachexp_2arm' were created. They work exactly like the former, only preparing analysis of a 2-arm experiment.

``` {r echo=TRUE}

meta.data = read_excel("/Users/paul/Documents/Einführung R/Wahlfach/Meta Behavior.xlsx")
animal.list = read_excel("/Users/paul/Documents/Einführung R/Wahlfach/Animal List.xlsx")

data.animal.joined = animal.list %>%
  filter(Genotype!= 'NA') %>%
  unite(col="GT_Env", Genotype, Environmental, sep= "_", remove = FALSE) %>%
  left_join(meta.data, by = c("RFID" = "Animal")) %>%
  dplyr::mutate_at(vars(Meanspeed:SerialLearn),(funs(as.numeric))) %>%
  select(RFID, GT_Env, Meanspeed:SerialLearn)
  
head(data.animal.joined)

ymin = min(data.animal.joined$Cue, na.rm = TRUE)*0.25
ymax = max(data.animal.joined$Cue, na.rm = TRUE)*1.25

ggplot(data.animal.joined, aes_string(x="GT_Env", y="Cue", fill="GT_Env")) +

  geom_boxplot(alpha = 0.4) +
  
  scale_fill_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B")) +
  scale_color_manual(values=c("#F7FCFD", "#CCECE6", "#238B45", "#00441B")) +

  geom_point(pch = 21, stroke=0.93, position = position_jitterdodge()) +

  ylab("CueScore") +
  xlab("Conditions") +

  coord_cartesian(ylim=c(ymin, ymax)) +

  geom_signif(test = "t.test",  
           comparisons = list(c(1, 2),
                              c(3, 4),
                              c(2, 3),
                              c(2, 4)), 
           y=c(0.85*ymax, 0.85*ymax, 0.89*ymax, 0.95*ymax), 
           map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05), 
           textsize = 5,  tip_length = 0.005) +

  ggtitle("Cue") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 16)) +
  theme(panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key = element_blank(),
    strip.background = element_blank(),
    
    axis.line.y = element_line(colour = "black", size=0.6),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle=0, size=10),
    axis.text.y = element_text(angle=0, size=10),
    text = element_text(size=14),
    
    legend.text = element_text(size=9),
    legend.title = element_text(size=12))
```
