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

#load in the dataext-meta-20121005.csv
#ext<-read.csv("../data/ext-meta-20121030.csv", skip=1, na.strings=c("NA", "N/A", ".", ""))
ext<-read.csv("../data/Meta-analysis-selectivity-data-2004-timescale.csv", skip=1, na.strings=c("NA", "N/A", ".", ""))

#some problems with extra spaces
ext$Trait.category<-gsub(" ", "", ext$Trait.category)

#fix some typos
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

#unnamed stages referenced back to the stage that they are unnamed for
#ext$End.stage<-gsub("unnamed Pridoli stage", "Pridolian", ext$End.stage)
#ext$Start.stage<-gsub("Sandbian","early Late Ordovician", ext$Start.stage)
#ext$Start.stage<-gsub("Dapingian","early Middle Ordovician", ext$Start.stage)
#ext$End.stage<-gsub("Floian","late Early Ordovician", ext$End.stage)
#ext$End.stage<-gsub("Floian","Floian", ext$End.stage)
#ext$End.stage<-gsub("Pridolian","unnamed Pridoli stage", ext$End.stage)
#ext$Start.stage<-gsub("early Late Ordovician", "Hirnantian", ext$Start.stage) #fix? ask emily
#ext$Start.stage<-gsub("early Middle Ordovician", "Sandblian", ext$Start.stage) #fix? ask emily
#ext$Start.stage<-gsub("late Early Ordovician", "Floian", ext$Start.stage) #fix? ask emily
#ext$End.stage<-gsub("late Early Ordovician", "Floian", ext$End.stage) #fix? ask emily

#fix taxon levels
ext$Tax.level<-gsub("Genera", "genera", ext$Tax.level)
ext$Tax.level<-gsub("Species", "species", ext$Tax.level)
ext$Tax.level<-factor(ext$Tax.level)

# delete blank columns:
ext <- ext[, -grep("X.[0-9]+", names(ext))]

####################
#
# Pull in environmental covariates
#
####################

#Columns with start and end stages from the data file
stageIDX<-which(names(ext) %in% c("Start.stage", "End.stage"))

#read in the data set with the conversions between stage and time
stageTime1<-read.csv("../data/Stage vs time.csv")
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

# add a "recent" row: (TODO someone should confirm this is OK)
stageTime <- rbind(stageTime[1,], stageTime)
stageTime[1, "Bin.name"] <- "Recent"

#A function to take a lookup pair of stages and get the entire range of stages in between
getStageRange<-function(startStage, endStage){
  #debug
  #print(paste(startStage, endStage, "\n", sep=" "))
  as.character(stageTime[which(stageTime$Bin.name==startStage):which(stageTime$Bin.name==endStage),]$Bin.name)
}

#A function to take a lookup pair of stages and get the entire range of stages in between
getTimeRange<-function(startStage, endStage){
  #debug
  #print(paste(startStage, endStage, "\n", sep=" "))
  startTime.Ma<-stageTime[which(stageTime$Bin.name %in% startStage),]$Start..Ma.
  endTime.Ma<-stageTime[which(stageTime$Bin.name %in% endStage),]$End..Ma.

  return(c(startTime.Ma=startTime.Ma, endTime.Ma=endTime.Ma))
}


#####volvacanism and bollide impacts
#first, pull in volcanism and bolides
volcbolide<-read.csv("../data/Flood Basalt Bolide Ocean Acidification.csv")
volcbolide$State.stage<-gsub(" $", "", volcbolide$State.stage)


######extinction rate
extMag<-read.csv("../data/Ext mag 3-15 Revised.csv")
extMag2<-read.csv("../data/Biv BC ext PBDB 4-17-12.csv")

names(extMag2)[6]<-paste(names(extMag2)[6], "PBDB", sep=".")

######proxy data
#proxy<-read.csv("../data/180 13C Sr 34S sealevel.csv")
#proxy1<-read.csv("../data/Hannisdal and Peters data with stage IDs.csv", na.strings="?")

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

#Veizer delta 18O
#veizer<-read.csv("../data/veizer_d18O.csv")
#veizer$Length<-veizer$StageBottom-veizer$StageTop

counter <<- 0

#iterate over the whole data set to get volcanism and bolide info
envtCols <- t(apply(ext[,stageIDX], 1, function(arow){
  counter <<- counter+1
  print(paste("row:",counter))

  
  #check for NAs
  flag<-0
  if(sum(is.na(arow))>0){
    flag<-1
    arow<-c("Recent", "Recent")
  }

  #debug
  print(arow)

  #check and see whether there were any volcanism or bolide impacts during these stages
  stageRange<-getStageRange(arow[1], arow[2])

  times<-getTimeRange(arow[1], arow[2])

  #based on the stages, get the row numbers of the volcanism bolid data file
  vbIDX <- which(volcbolide$State.stage %in% stageRange)
  exIDX <- which(extMag$Stage %in% stageRange)
  exIDX2 <- which(extMag2$Bin.name %in% stageRange)

  proxyIDX<-c(which(proxy$bottom >= times[1])[1] : #first time within the stage
              which(proxy$top >= times[2])[1] #last time within the stage
    )
  print(proxyIDX)

# when using original proxy file - but times were misaligned
#  proxyIDX<-c(which(proxy$Time..my. <= times[1])[1], #first time within the stage
#              which(proxy$Time..my. < times[2])[1]-1 #last time within the stage
#  )

  #note, because of the orientation of the sealevel table, we need some error checking for recent
  sl2 <-  which(sealevel$Time..my. < times[2])[1]
  if(is.na(sl2)) sl2 <- nrow(sealevel)+1
  sealevelIDX<-c(which(sealevel$Time..my. <= times[1])[1] : #first time within the stage
             sl2-1 #last time within the stage
    )


  #get the row numbers from the veizer file
#  veizerIDX<-c(which(veizer$StageBottom >= times[1])[1], #first time within the stage
#             which(veizer$StageTop > times[2])[1]-1 #last time within the stage
#  )
  
  #get the means for each column of the environmental data
  #rgh - kludge to turn a df into a vector, as colwise turns
  #out data frames
#  envt<-t(colwise(mean)(proxy[proxyIDX,2:ncol(proxy)]))[,1] #old proxy data
  #envt<-t(colwise(function(x) mean(x, na.rm=T))(proxy[proxyIDX,13:17]))[,1]
  envt<-t(colwise(function(x) mean(x, na.rm=T))(proxy[proxyIDX,c(9,6,12)]))[,1]

  #query whether there were any events, and set the return value to 1 if so
  vb<-as.numeric(colSums(volcbolide[vbIDX,3:6])>0)
  names(vb)<-names(volcbolide[3:6])

  ex<-c(BC.extinction.rate=mean(extMag$BC.extinction.rate[exIDX]))
  ex2<-c(BC.extinction.ratePBDB=mean(extMag2$BC.extinction.rate[exIDX2]))

#  vez<-t(colwise(function(x) mean(x, na.rm=T))(veizer[veizerIDX,4:10]))[,1]
#  vexWeight<-
#   vez<-with(veizer[veizerIDX,], {
#                       c(d18OresidualMean=mean(d18Oresidual),
#                         d18OresidualMeanWeighted=weighted.mean(d18Oresidual, Length),
#                         d18OresidualSD=sd(d18Oresidual),
#                         Dd18O.first.differences.Mean=mean(Dd18O.first.differences., na.rm=T),
#                       #  Dd18O.first.differences.MeanWeighted=weighted.mean(Dd18O.first.differences., length, na.rm=T),
#                         Dd18O.first.differences.SD=sd(Dd18O.first.differences., na.rm=T),
#                         Dd18O.first.differences.ABSMean=mean(abs(Dd18O.first.differences.), na.rm=T),
#                         Dd18O.first.differences.ABSMeanWeighted = weighted.mean(abs(Dd18O.first.differences.), Length, na.rm=T),
#                         d18O.meanObserved =mean(d18O.mean ),
#                         d18O.meanObservedWeighted = weighted.mean(d18O.mean, Length),
#                         d18O.meanObservedSD = sd(d18O.mean),
#                         d18O.CV = sd(d18O.mean)/ mean(d18O.mean)
#                         )})

    sea <- with(sealevel[sealevelIDX,], {
    	c(sea_level...first.diffs. = mean(sea_level...first.diffs.),
    	sea_level_residuals = mean(sea_level_residuals))
    }
    )


  ret<-c(times, vb, ex, envt, ex2, sea)
  names(ret)<-names(ret)
  colNames <<- c(names(times), names(vb), names(ex), names(envt), names(ex2), names(sea))
  
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
#extClean<-with(ext, ext[which(!is.na(X..Surv) &
#						!is.na(X..Ext) &
#						!is.na(Total...Surv) &
#						!is.na(Total...Ext)),])


#extClean<-with(extClean, extClean[which(!(X..Ext==0) &
#								  !(Total...Ext==0)),])
#nrow(extClean)

#mhVals<-rma.mh(X..Surv, X..Ext, Total...Surv-X..Surv, Total...Ext-X..Ext, data=ext)
#mhVals$yi
#length(mhVals$vi)
#nrow(ext)

#ec<-escalc("OR", ai=X..Surv, bi=X..Ext, ci=Total...Surv-X..Surv, di=Total...Ext-X..Ext, data=ext, to=c("only0"), add=c(1/2))

#ext<-cbind(ext, with(ext, {lnORMH(X..Surv, X..Ext, Total...Surv, Total...Ext)}))

#some studies reported just lnor and lnorCI from multivariate logistic regressions
#removed in 20120713 data
#ext$lnor[which(!is.na(ext$Log.odds))] <- ext$Log.odds[which(!is.na(ext$Log.odds))]
#ext$vlnor[which(!is.na(ext$Log.odds))] <- with(ext[which(!is.na(ext$Log.odds)),], {coefCI2var(C.I...95.., Genera)})

#pull in chi square studies
chisqlnorinfo<-with(ext[which(!is.na(ext$x.square)),], {chisq2lnor(x.square, n=n)})
ext$lnor[which(!is.na(ext$x.square))]  <- chisqlnorinfo$lnor
ext$vlnor[which(!is.na(ext$x.square))]  <- chisqlnorinfo$vlnor

ext$lnorReg[which(!is.na(ext$x.square))]  <- chisqlnorinfo$lnor
ext$vlnorReg[which(!is.na(ext$x.square))]  <- chisqlnorinfo$vlnor

#pull in t test studies
#*** 50: McClure 1995 NSE, Have: n.6, df.1, p..sur.extinct., p..before.after.
#*** 270-271: Hansen 1978 LDS, Has: Test.type, trait type species total, mean duration, z, NEED: total # of species (can calculate)




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



#######DATA CHECKS
#check for no lnor
#checkme<-which(is.na(ext$lnor) & !is.na(ext$Trait.category))
#ext[checkme,]

# bring in new d18O and d13C data:
# gr <- read.csv("../data/Grossman_d18O.d13C_with_high.lat.csv")
# gr$Binned_top <- round(gr$Binned_top, 2)
# ext$Binned_top <- round(ext$startTime.Ma, 2)
# d18Odat <- gr[gr$data_subset == "All" & gr$Binned_top < 500,c("Binned_top", "mean_d18O")]
# d13Cdat <- gr[gr$data_subset == "All" & gr$Binned_top < 500,c("Binned_top", "mean_d13C")]
# d18Odat$mean_d18O <- with(d18Odat, detrend_ts(Binned_top, mean_d18O, "Grossman d18O (All)"))
# d13Cdat$mean_d13C <- with(d13Cdat, detrend_ts(Binned_top, mean_d13C, "Grossman d13C (All)"))


# ext <- plyr::join(ext, d18Odat)
# ext <- plyr::join(ext, d13Cdat)

# ext$del.18O <- ext$mean_d18O
# ext$del.13C <- ext$mean_d13C
# ext$mean_d18O <- NULL # clean up
# ext$mean_d13C <- NULL # clean up


###################################
##### WRITE THE CLEAN DATA
###################################
write.table(ext, "../data/extinctionMetaClean.csv", sep=",", row.names=F)
