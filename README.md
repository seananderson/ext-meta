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

    library(knitr)
    knit("final.Rmd")
    source("pretty_data_overview.R")
    source("plot_effect_sizes.R")

To recreate the HTML file you'll need to run the file `final.md` through a Markdown interpreter. For example, using Pandoc:

    pandoc final.md -o final.html
