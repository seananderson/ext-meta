#########################################################################################################
# Take in all of the various proxy data files and merge them
# with the stageTime file, as the meta-analysis uses stages in order
# to differentiate between papers rather than times
#
# We're gonna have to write *very* good meta-data for this file later
#
# coded by Jarrett Byrnes
#
#
# Changelog
#
# 20131212 - Added some detrendended Prokoph data
# 20131211 - Cleaned odd 1st row issue
# 20131211 - Fixed which sealevel columns we're using
# 20131211 - Added max extinction rates
# 20131211 - Fixed Prokoph stage names
# 20131211 - Using time for volcanism/bolide data
# 20131211 - Added Prokoph data
# 20131103 - using gradstein stage names as they better match data
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
  par(mfrow=c(1,1))
  res.m
}

####################
#
# Pull in environmental covariates
#
####################

#Columns with start and end stages from the data file
stageIDX<-which(names(ext) %in% c("Start.stage", "End.stage"))

#read in the data set with the conversions between stage and time
#stageTime<-read.csv("../data/Stage vs time.csv")
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
  if(length(startTime.Ma)==0) startTime.Ma<-NA
  if(length(endTime.Ma)==0) endTime.Ma<-NA
  return(c(startTime.Ma=startTime.Ma, endTime.Ma=endTime.Ma))
}

getStage <- function(aTime){
  low <- which(stageTime$Start.MA. <=aTime)
  high <- which(stageTime$End..Ma. >= aTime)
  idx <- low[which(low %in% high)]
  as.character(stageTime$Bin.name[idx])
}


######extinction rate
extMag<-read.csv("../data/Ext mag 3-15 Revised.csv")
extMag2<-read.csv("../data/Biv BC ext PBDB 4-17-12.csv")

names(extMag2)[6]<-paste(names(extMag2)[6], "PBDB", sep=".")

#add times
extMag$Base_bottom..Ma. <- c(0,extMag$Base..Ma.[-nrow(extMag)])
extMag2$Base_bottom..Ma. <- c(0,extMag2$Base..Ma.[-nrow(extMag2)])

######proxy data
#proxy<-read.csv("../data/180 13C Sr 34S sealevel.csv")
proxy<-read.csv("../data/Hannisdal and Peters data with stage IDs.csv", na.strings="?")

#The Prokoph data
proxyProk<-read.csv("../data/Prokoph_data_9.2013.csv")
proxyProk <- subset(proxyProk, proxyProk$data_subset=="tropical no benthics")

#make stages match for easy processing
proxyProk$Binned_stage<-gsub("Sandbian", "Sandblian", proxyProk$Binned_stage)
proxyProk$Binned_stage<-gsub("Darriwillian", "Darriwilian", proxyProk$Binned_stage)
proxyProk$Binned_stage<-gsub("Fammenian", "Famennian", proxyProk$Binned_stage)
proxyProk$Binned_stage<-gsub("Gzelian", "Gzhelian", proxyProk$Binned_stage)
proxyProk$Binned_stage<-gsub("Olenikian", "Olenekian", proxyProk$Binned_stage)
proxyProk$Binned_stage<-gsub("Pridoli", "Pridolian", proxyProk$Binned_stage)
proxyProk$Binned_stage<-gsub("Pleistocene-Holocene", "Pleistocene", proxyProk$Binned_stage)

#pdf("figure/detrending-plots.pdf")
#detrend the delta 34S
proxy$del.34S.detrended <- with(proxy, detrend_ts(top, del.34S, "Hannisdal and Peters d34S"))
proxyProk$mean_d13C.detrended <- with(proxyProk, detrend_ts(Binned_top, mean_d13C, "Prokoph d13C"))
proxyProk$mean_d18O.detrended <- with(proxyProk, detrend_ts(Binned_top, mean_d18O, "Prokoph d18O"))
proxyProk$mean_d34S.detrended <- with(proxyProk, detrend_ts(Binned_top, mean_d34S, "Prokoph d34S"))


######new sealevel data
sealevel<-read.csv("../data/Sea level residuals after 2nd-order polynomial fit.csv")



#Veizer delta 18O
veizer<-read.csv("../data/veizer_d18O.csv")
veizer$Length<-veizer$StageBottom-veizer$StageTop



#Grossman Data
gr <- read.csv("../data/Grossman_d18O.d13C_with_high.lat.csv")
gr <- gr[which(gr$data_subset=="All"),]
names(gr)[4:9] <- paste(names(gr)[4:9], "gr", sep=".")
#get the indices in a data frame given the timing of a stage
getIdx <- function(stage_bottom, stage_top, bottom_vec, top_vec){
  idx <- which(bottom_vec>=stage_bottom & top_vec<=stage_top)

  #if nada, that means we're inside of a period
  if(length(idx)==0) {
    idx <- which(bottom_vec>=stage_bottom)[1]
  }

  return(idx)
}

#####volvacanism and bollide impacts
#first, pull in volcanism and bolides
#volcbolide<-read.csv("../data/Flood Basalt Bolide Ocean Acidification.csv")
volcbolide<-read.csv("../data/Flood Basalt Bolide Ocean Acidification_2.0.csv")
volcbolide$State.stage<-gsub(" $", "", volcbolide$State.stage)




#fix stage names
volcbolide$State.stage<-gsub("Sandbian", "Sandblian", volcbolide$State.stage)
volcbolide$State.stage<-gsub("unnamed Pridoli stage", "Pridolian", volcbolide$State.stage)
#volcbolide$State.stage<-gsub("middle Late Ordovician", "..", volcbolide$State.stage)
#volcbolide$State.stage<-gsub("early Late Ordovician", "..", volcbolide$State.stage)
#volcbolide$State.stage<-gsub("early Middle Ordovician", "Dapingian", volcbolide$State.stage)
#volcbolide$State.stage<-gsub("late Early Ordovician", "Floian", volcbolide$State.stage)
#volcbolide$State.stage<-gsub("Cambrian stage 4", "Furongian", volcbolide$State.stage)
#volcbolide$State.stage<-gsub("Cambrian stage 3", "..", volcbolide$State.stage)
volcbolide$State.stage<-gsub("Calabrian", "Pleistocene", volcbolide$State.stage)

#volcbolide[,2] -> vb.bins
#vb.bins[which(!(vb.bins %in% bins))]

#add times
volcbolide <- cbind(volcbolide, t(sapply(volcbolide$State.stage, function(x) getTimeRange(x,x))))

#fix errors in time due to bad stage names
# TODO NOTE THAT THERE AREN'T COLUMNS 7 and 8 IN THIS DATAFRAME IN THE LINE IT
# WAS ON.
# I have moved it down here to where I presume it should be and changed the
# column numbers to 8 and 9:
volcbolide[which(volcbolide$State.stage=="unnamed Pridoli stage"),8:9] <- c(418.8, 418.1)


##OK, time to combine them all
proxies <- sapply(1:nrow(stageTime),  function(x){
  arow <- stageTime[x,]

  veizerIDX <- which(veizer$StageBottom>=arow$End..Ma. & veizer$StageTop<=arow$Start..Ma.)
  grIDX <- getIdx(arow$End..Ma., arow$Start..Ma., gr$Binned_bottom, gr$Binned_top)
  volcbolideIDX <- getIdx(arow$End..Ma., arow$Start..Ma., volcbolide$endTime.Ma, volcbolide$startTime.Ma)
  exIDX <- getIdx(arow$End..Ma., arow$Start..Ma., extMag$Base_bottom..Ma., extMag$Base..Ma.)
  exIDX2 <- getIdx(arow$End..Ma., arow$Start..Ma., extMag2$Base_bottom..Ma., extMag2$Base..Ma.)
  sealevelIDX <- which(sealevel$Time..my. <= arow$End..Ma.)[1]
  proxyIDX <- getIdx(arow$End..Ma., arow$Start..Ma., proxy$bottom, proxy$top)
  vbIDX <- getIdx(arow$End..Ma., arow$Start..Ma., volcbolide$endTime.Ma, volcbolide$startTime.Ma)

  proxyProkIDX <- which(proxyProk$Binned_stage==arow$Bin.name)


  #Now that we have indices for each data file, create a new data frame that
  #extracts the proxy data we want from each one

  #use colMeans unless otherwise appropriate to average over multiple
  #time periods
  envt<-t(colwise(function(x) mean(x, na.rm=T))(proxy[proxyIDX,13:17]))[,1]

  #query whether there were any events, and set the return value to 1 if so
#  vb<-as.numeric(colSums(volcbolide[vbIDX,3:6])>0)
#  names(vb)<-names(volcbolide[3:6])

  #extinction rates
  ex<-c(BC.extinction.rate=mean(extMag$BC.extinction.rate[exIDX]))
  ex2<-c(BC.extinction.ratePBDB=mean(extMag2$BC.extinction.rate[exIDX2]))

  exMax<-c(BC.extinction.rate.max=max(extMag$BC.extinction.rate[exIDX], na.rm=T))
  ex2Max<-c(BC.extinction.ratePBDB.max=mean(extMag2$BC.extinction.rate[exIDX2], na.rm=T))


  #Data from Grossman
  gr <- colMeans(gr[grIDX,4:9])

  #query whether there were any events, and set the return value to 1 if so
  vb<-as.numeric(colSums(volcbolide[vbIDX,3:6])>0)
  names(vb)<-names(volcbolide[3:6])

  #sealevel
  sea <- with(sealevel[sealevelIDX,2:11], {
    c(sea_level...first.diffs. = mean(sea_level...first.diffs.),
      sea_level_residuals = mean(sea_level_residuals))
  })

  #Data from Prokoph
  prok <- colMeans(proxyProk[proxyProkIDX, c(6:17, 19:21)])

  names(prok) <- paste(names(prok), "prok", sep=".")

  return(c(envt, ex, ex2,exMax, ex2Max, gr, sea, prok, vb))
})


#blend the stageTime with the proxies, and write the definitive proxy file
proxiesByStage <- cbind(stageTime, as.data.frame(t(proxies)))

#fix first row and other errors
proxiesByStage <- proxiesByStage[-1,]
proxiesByStage$BC.extinction.rate.max[which(proxiesByStage$BC.extinction.rate.max==-Inf)] <- NA
proxiesByStage$BC.extinction.ratePBDB.max[which(is.nan(proxiesByStage$BC.extinction.ratePBDB.max))] <- NA

#write.csv(proxiesByStage, "../data/cleanProxiesByStage_20131027.csv", row.names=F)
#write.csv(proxiesByStage, "../data/cleanProxiesByStage_20131210.csv", row.names=F)
write.csv(proxiesByStage, "../data/cleanProxiesByStage_20131212.csv", row.names=F)
