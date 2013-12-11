#########################################################################################################
# Take in Emily's Data file, add covariates based on stage and then translate the panopoly of
# metrics into log odds ratios
#
# Changelog
#
# 20131202 - removed detrend function, as it was not being used
# 20131202 - now using cleaned proxy data file
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
ext$Start.stage<-gsub("Recent","Pleistocene", ext$Start.stage)
ext$End.stage<-gsub("Recent","Pleistocene", ext$End.stage)
#ext$End.stage<-gsub("Pridolian", "Pridoli", ext$End.stage)

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
# and proxy data
stageTime <- read.csv("../data/cleanProxiesByStage_20131210.csv")
stageTime$Bin.name <- as.character(stageTime$Bin.name)

# A function to take a lookup pair of stages and get the entire range of stages in between
getStageRange<-function(startStage, endStage){
  as.character(stageTime[which(stageTime$Bin.name==startStage):which(stageTime$Bin.name==endStage),]$Bin.name)
}

getStageRangeIDX<-function(startStage, endStage){
  which(stageTime$Bin.name==startStage):which(stageTime$Bin.name==endStage)
}


#A function to take a lookup pair of stages and get the entire range of stages in between
getTimeRange<-function(startStage, endStage){
  startTime.Ma<-stageTime[which(stageTime$Bin.name %in% startStage),]$Start..Ma.
  endTime.Ma<-stageTime[which(stageTime$Bin.name %in% endStage),]$End..Ma.
  return(c(startTime.Ma=startTime.Ma, endTime.Ma=endTime.Ma))
}

#### Data matching checks:
# Check: are all ext Start.stage and End.stage values in stageTime?
# unique(ext$Start.stage) %in% unique(stageTime$Bin.name)
# unique(ext$End.stage) %in% unique(stageTime$Bin.name)

# iterate over the whole data set to get volcanism and bolide info
envtCols <- t(sapply(1:nrow(ext), function(i){
  arow <- ext[i,stageIDX]

  #check for NAs
  flag<-0
  if(sum(is.na(arow))>0){
    flag<-1
    arow<-c("Recent", "Recent")
  }

  #get the row numbers of the relevant stages
  stageRange<-getStageRange(arow[1,1], arow[1,2])
  stageRangeIDX<-getStageRangeIDX(arow[1,1], arow[1,2])


  #get the means for each column of the proxy data
  #rgh - kludge to turn a df into a vector, as colwise turns
  #out data frames
  ret<-colMeans(stageTime[stageRangeIDX,7:38], na.rm=T)
  return(ret)
}))

envtCols<-as.data.frame(envtCols)
envtCols$OA[which(envtCols$OA>0)]<-1
envtCols$Flood_basalt[which(envtCols$Flood_basalt>0)]<-1
envtCols$Bolide[which(envtCols$Bolide>0)]<-1
#names(envtCols) <- colNames

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

# rename some proxy data to match old column names:
#ext <- plyr::rename(ext, c("mean_d18O" = "del.18O", "mean_d13C" = "del.13C"))

###################################
##### WRITE THE CLEAN DATA
###################################
write.table(ext, "../data/extinctionMetaClean.csv", sep=",", row.names=F)
