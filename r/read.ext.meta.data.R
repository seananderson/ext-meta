# ====================================================================
# Purpose:       read in, check, and clean main dataset
# ====================================================================

ext.dat <- read.csv("../data/ext-meta-20111121.csv", skip = 1, strip.white = TRUE, blank.lines.skip = TRUE, stringsAsFactors = FALSE)

sink("../datachecks/frequency-of-terms.txt")
for(column.name in c("Taxon", "Tax.level", "Phylum", "Trait", "Trait.category", "Type", "Age.event", "Location", "Data.source", "Year")) {
  print("")
  print("###########################################")
  print(paste("Column =", column.name))
  freq.df <- as.data.frame(sort(table(ext.dat[ , column.name]), decreasing = TRUE))
  names(freq.df) <- "Frequency"
  print(freq.df)
}
sink()

