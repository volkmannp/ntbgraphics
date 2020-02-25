ReadMe for ntbgraphics
================

# Table of contents

  - [Introduction](#introduction)
  - [Basic Principles](#basic-principles)
  - [Examples](#examples)
  - [Installation](#installation)
  - [Demo](#demo)

# Introduction

The following introduction to the R package ‘ntbgraphics’ aims not only
for expounding its features but also for facilitating access to it, even
for relatively unexperienced R users. Please refer to the
[Installation](#installation) part if you have not yet downloaded R
itself.

This package includes functions for importing, transforming and
visualization of NTB datasets:

  - **‘getexpdata’** for import of the Animal List (containing animal
    *RFIDs*, corresponding Genotypes and optionally Environment) and
    Meta Behavior (containing columns *Animal* and the animal’s
    behavioral measures) as well as formal preparation;
  - **‘ploteachexp’** for plotting of given experiments as boxplots and
    exporting the result as a PDF file;
  - **‘loopplotexp’** for plotting all experiments within a dataset as
    boxplots and exporting the results in one PDF file;
  - **‘heatmapexp’** for plotting all experiments as a heatmap and
    producing a datamatrix with z-scored values;
  - **‘pcatsneexp’** for PCA and tSNE results and cluster plots;
  - **‘colordiverger’** for creating a customized diverging color
    palette for visualization in different contexts.

[Go to Top](#top)

# Basic Principles

All functions (apart from ‘colordiverger’) take a **directory** as their
input, which specifies the location of the **two files** “Animal
List.xlsx” and “Meta Behavior.xlsx” (mind correct spelling of these
files - functions rely on specific names as given\!). Furthermore, the
user has to specify the kind of data provided, respectively the kind of
analysis he wants to perform by defining the correct **analysis**.
Please refer to the ‘getexpdata’ documentation for further information\!

Alternativley, you might call the arguments each function takes by
executing the following line of code:

``` r
## you may look for every other function the same way
formals(getexpdata)

## note: in case of many options for an argument, the first one listed also is the default setting
```

It is also important that you mind correct **formatting** of your excel
files. This includes:

  - at least two columns with information about ‘RFID’ and
    ‘Genotype’/‘Environmental’/‘Treatment’ in your Animal List
    with these exact titles;
  - at least one column with information about ‘Animal’ (matching the
    information in the ‘RFID’ column in the Animal List), and at least
    one behavioral test in your Meta Behavior with exact titles:    
    *“Animal” “Meanspeed” “Rotations” “Center” “Alternations” “Choices”
    “Context” “Cue” “FreezeBase” “Timeimmobile” “Baseline”
    “inhibition70” “inhibition75” “inhibition80” “SucPref” “PlacePref”
    “ReversalLearn” “Activity” “Nocturnal” “SerialLearn”*  
    *(-\> this is the current entity of all available experiment names
    for analysis and plotting/mapping; if you need to add more
    experiments to this list, please refer to the creator of this
    package)*  

Further aspects can be customized depending on the specific function
within that function.

All functions externally work on their own, which means that they may
rely internally on one of the other functions of the package without the
user needing to run them in advance.

For heatmapping and especially for PCA and tSNE, it is important to have
a sufficient number of animals for each group that ran through all of
the tests that have been performed in that cohort or that will be
analyzed. While ‘heatmapexp’ will display NAs (missing values) as 0 from
the z-scored matrix and therefor simply in a neutral color - which will
still lead to an interpretable result -, PCA and tSNE will exclude all
animals with missing values completely, and thus might look quite poor
with low animal numbers\!

[Go to Top](#top)

# Examples

In this section you can find some examples how the output of some of the
functions could look like. Be aware of the fact that you can customize
many aspects of these outputs; refer to the functions’ help pages for
further information.  

The following plot shows the general layout you can expect from the
boxplot functions (**‘ploteachexp’** and **‘loopplotexp’**)(please note:
random data; therefore most likely no convincing differences between
groups).

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->  
  
 

The following map shows the general layout you can expect from the
**‘heatmapexp’** function (please note: random data; therefore most
likely no convincing clustering).

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->  
  
 

The following plots show the general layout you can expect from the
**‘pcatsneexp’** function (please note: random data; therefore most
likely no convincing
clustering).

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->  
[Go to Top](#top)  
  
 

# Installation

If you want to install this package, the following lines of code provide
a simple way that does not rely on any dependencies except from you
**having installed [R from
CRAN](https://cran.r-project.org/mirrors.html) and ideally a GUI of your
choice, e.g. [RStudio](https://rstudio.com/products/rstudio/)**. You may
then copy all of the lines in the following paragraph or skip certain
lines if redundant. For further information regarding usage of R, check
out the following
[introduction](https://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf)
or any other introduction you may find
useful.

``` r
## install package devtools to get - amongst others - functions to access ntbgraphics 
## (and every other package on GitHub)
install.packages("devtools")
## install ntbgraphics with function 'install_github' from package devtools
devtools::install_github("volkmannp/ntbgraphics")
## that is it!

## you are now ready to use ntbgraphics on your computer and may load it using...
library(ntbgraphics)

## for examples on how to use the functions, read on below
```

For your information: You can use the command ‘install.packages’ for the
installation of devtools due to its availability on CRAN (Comprehensive
R Archive Network). Since ntbgraphics is not part of CRAN, its
installation needs another function (‘install\_github’) that in turn is
part of the devtools package.

[Go to Top](#top)

# Demo

After installing ntbgraphics, you probably want to explore the package
with some random data or might simply be curious how to specifically
deal with the functions provided. Thus, below you may find some lines of
code that address this inquisitiveness. However, they do not explore
every single possible option available for the functions of this package
which you may try out on your own.  

The example data used for the following is provided within the package
by being included in the installed files. You may simply copy all lines
and run them at once or copy indivdual lines/functions you have a
particular interest in.  

Note: Each function works independently of what you may have run in
advance as long as ‘ntbgraphics’ has been loaded. Although the example
shows the ‘getexpdata’ function as its very first, running it is not
necessary for the other functions to work. This holds true for every
single function\!    
Also note that the directory within these lines aims for working on
every computer by accessing the data provided within this package. If
you want to use your own files, the directory might rather look like
this:
“/Users/specificuser/Documents/experiments/ntb/run1”

``` r
## clear workspace and load package (with all functions and necessary dependencies included automatically)
rm(list = ls(all.names = TRUE))
library(ntbgraphics)

## (getexpdata) get modified table with data
data.animal.joined <- getexpdata(directory = paste0(system.file("extdata/", package = "ntbgraphics", 
                                                                mustWork = T),"/"),
                                 analysis = "4arm_sd_tg")

## (ploteachexp) plot a defined experiment
ploteachexp(expname = "Meanspeed",
            directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
            analysis = "4arm_sd_tg",
            saveplotdir = FALSE)

## (loopplotexp) plot all experiments
loopplotexp(directory = paste0(system.file("extdata", package = "ntbgraphics", mustWork = T),"/"),
            analysis = "4arm_sd_tg")

## (heatmapexp) print out heatmap
data.animal.matrix <- heatmapexp(directory = paste0(system.file("extdata", package = "ntbgraphics",
                                                                mustWork = T),"/"),
                                 analysis = "4arm_sd_tg",
                                 saveplotdir = FALSE)

## (pcatsneexp) plot PCA and tSNE
results <- pcatsneexp(directory = paste0(system.file("extdata/", package = "ntbgraphics", 
                                                     mustWork = T),"/"),
                      analysis = "4arm_sd_tg",
                      perplex =  7,
                      saveplotdir = FALSE)
### -> access results of pcatsneexp (requires to run pcatsneexp and store results as shown above)
results_pca <- results[["pca_analysis"]]
results_tsne <- results[["tsne_analysis"]]
```

[Go to Top](#top)
