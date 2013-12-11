stageBins <- stageTime$Bin.name
extBins <- unique(c(levels(factor(ext$End.stage)), levels(factor(ext$Start.stage))))


extBins %in% stageBins
