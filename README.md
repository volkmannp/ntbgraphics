# Github_Repo

This package includes basically two functions:

  - 'getexpdata' for import of the Animal List (containing animal RFIDs and corresponding Genotypes and optionally Environment) and Meta Behavior (containing animals and their behavioral measures) as well as formal preparation;
  - 'ploteachexp' for plotting of experiments of the prepared table and export of a PDF file.
  
The first function ('getexpdata') takes a directory as its input and uses two excel files from this directory ("Animal List" and "Meta Behavior" - mind correct spelling!). It joins the two tables included and creates a column "GT_Env" by uniting "Genotype" and "Environment". Furthermore, it discards all useless information not needed for analysis. Finally, it returns a list called "data.animal.joined".

The second function ('ploteachexp') takes the name of one of the columns of "data.animal.joined", specifically one of the experiment's names, and again a directory as its input. The data of the chosen column will be plotted and save as a PDF file in the specified  directory.

As you might have realized, the 'getexpdata' functions assumes a 4-arm experimental setup. For simple 2-arm experiments, two additional function 'getexpdata_2arm' and 'ploteachexp_2arm' were created. The work exactly like the former, only preparing an analysis of a 2-arm experiment.

As you might have realized, the 'getexpdata' functions assumes a 4-arm experimental setup. For simple 2-arm experiments, two additional function 'getexpdata_2arm' and 'ploteachexp_2arm' were created. The work exactly like the former, only preparing an analysis of a 2-arm experiment.
