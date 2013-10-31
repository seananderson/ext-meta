#########################################################################################################
# Take in Emily's Data file, add covariates based on stage and then translate the panopoly of
# metrics into log odds ratios
#
# Changelog
#
# 20121101 - changed proxy data file to new Hannisdal & Peters with corresponding change to choosing rows by time
# 20121030 - new input data file with cleaned Bretsky data
# 20121005 - new data file 20121005 with additional columns
#			- fixed naming convention for envtcols (required converting to a data frame, not supplying names)
#			- added sea level variables
# 20120914 - added in regular lnors and scrubbing data frame for NAs and infs
# 20120719 - new data file ext-meta-20120713.csv
# 20120710 - added bollide and OA from new data file
# 20120709 - new data file ext-meta-20120706.csv
# 20120425 - added in pbdb extinction magnitudes
# 20120314 - added in new extinction magnitude dataset
# 20120225 - added in new deltaO18 data
#########################################################################################################

#library and data
library(plyr)



#read in a set of formulate for calculating effect sizes and variances
source("../r/conversions.R")

detrend_ts <- function(x, y, label = "") {
  par(mfrow = c(2, 1))
  d <- data.frame(x, y)
  d <- na.omit(d)
  plot(d, main = label, type = "o", xlim = c(500, 0), ylab = label, xlab = "Geologic Time (Ma)")
  m <- lm(y ~ x, na.action = na.exclude)
  res.m <- as.numeric(residuals(m, na.action = na.exclude))
  plot(x, res.m, main = paste(label, "detrended"), type = "o", xlim = c(500, 0), ylab = "Residual", xlab = "Geologic Time (Ma)")
  abline(h = 0)
  res.m
}


#######

# load in the dataext-meta-20121005.csv
ext<-read.csv("../data/Meta-analysis-selectivity-data-2004-timescale.csv", skip=1, na.strings=c("NA", "N/A", ".", ""))

# some problems with extra spaces
ext$Trait.category<-gsub(" ", "", ext$Trait.category)

# fix some typos
ext$Start.stage<-gsub("Serravalian","Serravallian", ext$Start.stage)
ext$End.stage<-gsub("Serravalian","Serravallian", ext$End.stage)
ext$Start.stage<-gsub("Pridolian","unnamed Pridoli stage", ext$Start.stage)
ext$Start.stage<-gsub("Rhuddian","Rhuddanian", ext$Start.stage)
ext$Start.stage<-gsub("Barriasian", "Barremian", ext$Start.stage)#?
ext$End.stage<-gsub("Barriasian", "Barremian", ext$End.stage)#?
ext$Start.stage<-gsub("Barremanian", "Barremian", ext$Start.stage)
ext$End.stage<-gsub("Barremanian", "Barremian", ext$End.stage)
ext$Start.stage<-gsub("Albnian", "Albian", ext$Start.stage)
ext$End.stage<-gsub("Albnian", "Albian", ext$End.stage)
ext$End.stage<-gsub("Albnian", "Albian", ext$End.stage)
ext$Start.stage<-gsub("Burdiganian", "Burdigalian", ext$Start.stage)
ext$End.stage<-gsub("Burdiganian", "Burdigalian", ext$End.stage)
ext$Start.stage<-gsub("Sandbian","Sandblian", ext$Start.stage)

# fix taxon levels
ext$Tax.level<-gsub("Genera", "genera", ext$Tax.level)
ext$Tax.level<-gsub("Species", "species", ext$Tax.level)
ext$Tax.level<-factor(ext$Tax.level)

# delete blank columns:
ext <- ext[, -grep("X.[0-9]+", names(ext))]

# delete blank rows:
ext <- ext[-which(is.na(ext$study.ID)), ]

# Pull in environmental covariates
# Columns with start and end stages from the data file
stageIDX<-which(names(ext) %in% c("Start.stage", "End.stage"))

# read in the data set with the conversions between stage and time
stageTime <- read.csv("../data/stage-v-time-gradstein-2004-ts.csv")
names(stageTime)[1] <- "End..Ma."
names(stageTime)[2] <- "Start..Ma."
names(stageTime)[3] <- "Bin.name"
stageTime$Bin.name <- as.character(stageTime$Bin.name)
# Fix some typos:
stageTime$Bin.name[stageTime$Bin.name == "Olenikian"] <- "Olenekian"
stageTime$Bin.name[stageTime$Bin.name == "Gzelian"] <- "Gzhelian"
stageTime$Bin.name[stageTime$Bin.name == "Fammenian"] <- "Famennian"
stageTime$Bin.name[stageTime$Bin.name == "Pridoli"] <- "Pridolian"
stageTime$Bin.name[stageTime$Bin.name == "Sandbian"] <- "Sandblian" # replacing with typo
stageTime$Bin.name[stageTime$Bin.name == "Darriwillian"] <- "Darriwilian"

# add a "recent" row:
stageTime <- rbind(stageTime[1,], stageTime)
stageTime[1, "Bin.name"] <- "Recent"


# A function to take a lookup pair of stages and get the entire range of stages in between
getStageRange<-function(startStage, endStage){
  as.character(stageTime[which(stageTime$Bin.name==startStage):which(stageTime$Bin.name==endStage),]$Bin.name)
}

#A function to take a lookup pair of stages and get the entire range of stages in between
getTimeRange<-function(startStage, endStage){
  startTime.Ma<-stageTime[which(stageTime$Bin.name %in% startStage),]$Start..Ma.
  endTime.Ma<-stageTime[which(stageTime$Bin.name %in% endStage),]$End..Ma.
  return(c(startTime.Ma=startTime.Ma, endTime.Ma=endTime.Ma))
}

#####volvacanism and bollide impacts
#first, pull in volcanism and bolides
volcbolide<-read.csv("../data/Flood Basalt Bolide Ocean Acidification.csv")
volcbolide$State.stage<-gsub(" $", "", volcbolide$State.stage)

######extinction rate
extMag2<-read.csv("../data/PaleoDB-BC-rate-genus-bivalve-gast-1.csv")
extMag2$Bin.name <- as.character(extMag2$Bin.name)
extMag2$Bin.name[extMag2$Bin.name == "Sandbian"] <- "Sandblian" # replacing with a typo to match stageTime

######proxy data
proxy <- read.csv("../data/Prokoph_data_9.2013.csv")
proxy <- subset(proxy, data_subset == "All")
proxy <- rename(proxy, c("mean_d34S" = "del.34S", "Binned_top" = "top", "Binned_bottom" = "bottom"))
proxy <- proxy[order(proxy$top), ]

pdf("detrending-plots-prokoph.pdf")
proxy$del.34S <- with(proxy, detrend_ts(top, del.34S, "Prokoph d34S"))
proxy$mean_d18O <- with(proxy, detrend_ts(top, mean_d18O, "Prokoph d18O"))
proxy$mean_d13C <- with(proxy, detrend_ts(top, mean_d13C, "Prokoph d13C"))
dev.off()

######new sealevel data
sealevel<-read.csv("../data/Sea level residuals after 2nd-order polynomial fit.csv")


#### Data matching checks:
# Check: are all ext Start.stage and End.stage values in stageTime?
if(unique(unique(ext$Start.stage) %in% unique(stageTime$Bin.name)) != TRUE)
  stop("Not all starting stage names in ext match the stageTime dataset.")
if(unique(unique(ext$End.stage) %in% unique(stageTime$Bin.name)) != TRUE)
  stop("Not all ending stage names in ext match the stageTime dataset.")

# Also check: volcbolide$State.stage in stageTime$Bin.name
unique(ext$Start.stage)[!unique(ext$Start.stage) %in% volcbolide$State.stage]
unique(ext$End.stage)[!unique(ext$Start.stage) %in% volcbolide$State.stage]
# "Sandblian": Bretsky 1973 EPP
# "Dapingian": Bretsky 1973 EPP

unique(ext$Start.stage)[!unique(ext$Start.stage) %in% extMag2$Bin.name]
# good

counter <<- 0
# iterate over the whole data set to get volcanism and bolide info
envtCols <- t(apply(ext[,stageIDX], 1, function(arow){
  counter <<- counter+1

  #check for NAs
  flag<-0
  if(sum(is.na(arow))>0){
    flag<-1
    arow<-c("Recent", "Recent")
  }

  #check and see whether there were any volcanism or bolide impacts during these stages
  stageRange<-getStageRange(arow[1], arow[2])

  times<-getTimeRange(arow[1], arow[2])

  #based on the stages, get the row numbers of the volcanism bolid data file
  vbIDX <- which(volcbolide$State.stage %in% stageRange)
  exIDX2 <- which(extMag2$Bin.name %in% stageRange)

  proxyIDX<-c(which(proxy$bottom >= times[1])[1] : #first time within the stage
              which(proxy$top >= times[2])[1] #last time within the stage
    )

  #note, because of the orientation of the sealevel table, we need some error checking for recent
  sl2 <-  which(sealevel$Time..my. < times[2])[1]
  if(is.na(sl2)) sl2 <- nrow(sealevel)+1
  sealevelIDX<-c(which(sealevel$Time..my. <= times[1])[1] : #first time within the stage
             sl2-1 #last time within the stage
    )

  #get the means for each column of the environmental data
  #rgh - kludge to turn a df into a vector, as colwise turns
  #out data frames
  envt<-t(colwise(function(x) mean(x, na.rm=T))(proxy[proxyIDX,c(9,6,12)]))[,1] # 9, 6, 12: d18O, d13C, d34S

  #query whether there were any events, and set the return value to 1 if so
  vb<-as.numeric(colSums(volcbolide[vbIDX,3:6])>0)
  names(vb)<-names(volcbolide[3:6])

  ex2<-c(BC.extinction.ratePBDB=mean(extMag2$BC.extinction.rate[exIDX2], na.rm = TRUE))

    sea <- with(sealevel[sealevelIDX,], {
    	c(sea_level...first.diffs. = mean(sea_level...first.diffs., na.rm = TRUE),
    	sea_level_residuals = mean(sea_level_residuals, na.rm = TRUE))
    })

  ret<-c(times, vb, envt, ex2, sea)
  names(ret)<-names(ret)
  colNames <<- c(names(times), names(vb), names(envt), names(ex2), names(sea))

  if(flag==1) ret<-rep(NA, length(ret))

  return(ret)
}))

envtCols<-as.data.frame(envtCols)
names(envtCols) <- colNames

#debug
#plot(del.18O ~ d18O.observations, data= envtCols)

####MERGE THE COLUMNS INTO THE DATA FRAME
ext<-cbind(ext, envtCols)

#####
#translate different metrics into lnor
#####
#ext<-cbind(ext, with(ext, {lnORMH(X..Surv, X..Ext, Total...Surv, Total...Ext)}))

#FROM # surviving, # extinct
#both corrected and non-corrected lnor
ext<-cbind(ext, with(ext, {lnORMH(X..Surv, X..Ext, Total...Surv-X..Surv, Total...Ext-X..Ext)}))

#Nor the normal log odds ratio, but add 0.5 to avoid problems with infinite values
#a histogram check shows that we don't generate any outliers this way
reglnor<-with(ext, {lnORReg(0.5+X..Surv, 0.5+X..Ext, 0.5+Total...Surv-X..Surv, 0.5+Total...Ext-X..Ext)})
#reglnor<-with(ext, {lnORReg(X..Surv, X..Ext, Total...Surv-X..Surv, Total...Ext-X..Ext)})

names(reglnor)<-paste(names(reglnor), "Reg", sep="")
ext<-cbind(ext, reglnor)

#plot(ext$lnor, ext$lnorReg, xlim=c(-4,4))

#can we get mh values from rma for comparison
library(metafor)

#pull in chi square studies
chisqlnorinfo<-with(ext[which(!is.na(ext$x.square)),], {chisq2lnor(x.square, n=n)})
ext$lnor[which(!is.na(ext$x.square))]  <- chisqlnorinfo$lnor
ext$vlnor[which(!is.na(ext$x.square))]  <- chisqlnorinfo$vlnor

ext$lnorReg[which(!is.na(ext$x.square))]  <- chisqlnorinfo$lnor
ext$vlnorReg[which(!is.na(ext$x.square))]  <- chisqlnorinfo$vlnor

####################################
######data and cleaning
####################################

#clean out the cruft - studies with only one measurement
ext<-subset(ext, !is.na(ext$lnor) & !is.na(ext$vlnor))
ext<-subset(ext, is.finite(ext$lnorReg))

#ok, so, a tricky thing is that the observations - individual lines in the data -
#are from the same event over multiple different trait types.
#We're going to need to account for this and determine what 'clusters' of
#measurements using shared information exist for later analysis.
#fortunately, there is a column that kind of gets at this - effect..
#however, we need to purge out weird characters and NAs, as they are from studies where
#we cannot assess within study covariance that we'll need later
ext$effect.. <-factor(ext$effect..)
ext$effect..<-as.numeric(as.character(ext$effect..))

ext$cluster<-rep(NA, nrow(ext))
ext$cluster[1]<-1

#yeah, yeah, a for loop.   So sue me.  It's what I'm teaching this week.
for(i in 2:nrow(ext)){
	ext$cluster[i] <- ifelse(ext$effect..[i]<ext$effect..[i-1],ext$cluster[i-1]+1,ext$cluster[i-1])
}

# rename proxy data to match old column names:
ext <- plyr::rename(ext, c("mean_d18O" = "del.18O", "mean_d13C" = "del.13C"))

###################################
##### WRITE THE CLEAN DATA
###################################
write.table(ext, "../data/extinctionMetaClean.csv", sep=",", row.names=F)
