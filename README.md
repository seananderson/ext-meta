The folder contains code for a meta-analysis looking at the ecological and 
environmental determinants of extinction risk for bivalves and gastropods in
the fossil record. The code was largely written by Jarrett Byrnes.

The analysis scripts call some R files in the `r` folder and some data from the
`data` folder.

To re-run the analysis, run the following in an R console:

First, install these packages if you don't have them. It's fine to re-run this 
line if you aren't sure.

    install.packages(c("ggplot2", "gridExtra", "metafor", "RColorBrewer", "AICcmodavg", "knitr", "lme4", "plyr"))

Then set your working directory to the `analysis` folder.
For example, on my computer:

    setwd("~/src/ext-meta/analysis/")

Then run these lines of code:

    source("../r/data_clean_merge.r")
    library(knitr)
    knit2html("final.Rmd")
    source("pretty_data_overview.R")
    source("plot_effect_sizes.R")

