ReadMe for ntbgraphics
================

This package includes functions for importing, transforming and
visualization of NTB datasets:

  - ‘getexpdata’ for import of the Animal List (containing animal RFIDs,
    corresponding Genotypes and optionally Environment) and Meta
    Behavior (containing animals and their behavioral measures) as well
    as formal preparation;
  - ‘ploteachexp’ for plotting of given experiments as boxplots and
    exporting the result as a PDF file;
  - ‘loopplotexp’ for plotting all experiments within a dataset as
    boxplots and exporting the results in one PDF file;
  - ‘heatmapexp’ for plotting all experiments as a heatmap and producing
    a datamatrix with z-scored values.

All functions take a directory as their input, which specifies the
location of the two files “Animal List” and “Meta Behavior” (mind
correct spelling of these files - functions rely on specific names\!).
Furthermore, they require definition of the experimental setup in terms
of 4-arm or 2-arm design or required type of analysis (you may want to
analyze your 4-arm experiment as 2-arm, only looking at two conditions).
The default setup assumes a 4-arm experiment. Further aspects can be
customized.

All functions work on their own which means that they may rely
internally on one of the other functions without the user needing to run
them in advance.  
  
  
The following plot shows the general layout you can expect from the
boxplot functions (‘ploteachexp’ and ‘loopplotexp’).  
  
![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->  
  
  
  
  
The following map shows the general layout you can expect from the
‘heatmapexp’ function.  
  
![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
