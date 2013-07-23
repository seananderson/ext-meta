The folder contains code for a meta-analysis looking at the ecological and 
environmental determinants of extinction risk for bivalves and gastropods in
the fossil record.

The analysis scripts call some R files in the "r" folder and some data from the
"data" folder.

To re-run the analysis, run the following in an R console:
[I (Sean) have tested this in 3.0.0 with current versions of the packages
as of 2013-07-10.]

First, install these packages if you don't have them. It's fine to re-run this 
line if you aren't sure.

    install.packages(c("ggplot2", "gridExtra", "metafor", "RColorBrewer", "AICcmodavg", "knitr", "lme4", "plyr"))

Then set your working directory to the "final_analysis_graphs_for_paper" folder.
For example, on my computer:

    setwd("~/Dropbox/NESCent-extinction/meta-analysis/extinction-meta-analysis/analysis")

Then run these lines of code:

    library(knitr)
    knit("final.Rmd")
    source("pretty_data_overview.R")
    source("plot_effect_sizes.R")
