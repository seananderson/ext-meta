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
# 20140611 - Added extMag3 dataset
# 20140610 - Re-wrote proxy generation code to correct errors using ranges
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

#read in the data set with the conversions between stage and time
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

# add a "recent" row and deal with end of the Pleistocene
stageTime <- rbind(stageTime[1,], stageTime)
stageTime[1, "Bin.name"] <- "Recent"
stageTime[1, "Start..Ma."] <- 0.01
stageTime[1, "End..Ma."] <- 0
stageTime[2, "End..Ma."] <- 0.01

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
extMag3<-read.csv("../data/RangeThrough_bcRate_2014.csv") #added 6/2014 after group feedback

#Fix bad extinction rate (2 orders of magnitude too high)
extMag3$BC.extinction.rate[which(extMag3$BC.extinction.rate>2)] <- NA

#Add the recent to extMag3
extMag3 <- rbind(NA, extMag3)
extMag3$Bin.name <- as.character(extMag3$Bin.name)
extMag3$Bin.name[1] <- "Recent"
extMag3$top[1] <- 0
extMag3$bottom[1] <- 1.8

names(extMag2)[6]<-paste(names(extMag2)[6], "PBDB", sep=".")
names(extMag3)[9]<-paste(names(extMag3)[9], "PBDB3", sep=".")

#add times for extMag and extMag2
extMag$Bin.name <- extMag$Stage
extMag$Base..Ma.[1:2] <- c(0.01,1.8)
extMag$Top..Ma. <- c(0,extMag$Base..Ma.[-nrow(extMag)])

extMag2$Base_bottom..Ma. <- c(0.01,1.8, extMag2$Base..Ma.[-c(1:2)])
extMag2$Top..Ma. <- c(0, extMag2$Base_bottom..Ma.[-nrow(extMag2)])

######proxy data
#proxy<-read.csv("../data/180 13C Sr 34S sealevel.csv")
proxy<-read.csv("../data/Hannisdal and Peters data with stage IDs.csv", na.strings="?")
proxy <- rbind(NA, proxy)
proxy$bottom[1] <- 2.59
proxy$top[1] <- 0

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
#reverse its order for easier processing
sealevel<-sealevel[nrow(sealevel):1,]


#Veizer delta 18O
veizer<-read.csv("../data/veizer_d18O.csv")
veizer$Length<-veizer$StageBottom-veizer$StageTop



#Grossman Data
gr <- read.csv("../data/Grossman_d18O.d13C_with_high.lat.csv")
gr <- gr[which(gr$data_subset=="All"),]
names(gr)[4:9] <- paste(names(gr)[4:9], "gr", sep=".")


#####volvacanism and bollide impacts
#first, pull in volcanism and bolides
volcbolide<-read.csv("../data/Flood Basalt Bolide Ocean Acidification_2.0.csv")
volcbolide$State.stage<-gsub(" $", "", volcbolide$State.stage)


#fix stage names
volcbolide$State.stage<-gsub("Sandbian", "Sandblian", volcbolide$State.stage)
volcbolide$State.stage<-gsub("unnamed Pridoli stage", "Pridolian", volcbolide$State.stage)
volcbolide$State.stage<-gsub("Calabrian", "Pleistocene", volcbolide$State.stage)
volcbolide$Bin.name <- volcbolide$State.stage
volcbolide$State.stage[which(volcbolide$State.stage=="Cambrian stage 4")] <- "Middle Cambrian"
volcbolide$State.stage[which(volcbolide$State.stage=="Cambrian stage 3")] <- "Early Cambrian"

#add times
volcbolide <- join(volcbolide, stageTime)

##############
#get the indices in a data frame given the timing of a stage
##############
getIdx <- function(stage_bottom, stage_top, bottom_vec, top_vec){
  #idx <- which(bottom_vec>=stage_bottom && top_vec<=stage_top)
  idx1 <- which(bottom_vec >= stage_bottom)[1]
  if(is.na(idx1)) return(NA) #for proxies that don't go that deep

  idx2 <- which(top_vec<=stage_top)
  if(length(idx2)==0) return(NA) #this is for proxies that'd don't have data for the recent

  idx2 <- idx2[length(idx2)]
  idx <- idx1:idx2

  #if nada, that means we're inside of a period
  if(length(idx)==0) {
    idx <- which(bottom_vec>=stage_bottom)[1]
  }

  return(idx)
}

#fix errors in time due to bad stage names
# TODO NOTE THAT THERE AREN'T COLUMNS 7 and 8 IN THIS DATAFRAME IN THE LINE IT
# WAS ON.
# I have moved it down here to where I presume it should be and changed the
# column numbers to 8 and 9:
volcbolide[which(volcbolide$State.stage=="unnamed Pridoli stage"),8:9] <- c(418.8, 418.1)


############
##OK, time to combine them all
#############
proxies <- sapply(1:nrow(stageTime),  function(x){
  arow <- stageTime[x,]
  print(x)

  veizerIDX <- which(veizer$StageBottom>=arow$End..Ma. & veizer$StageTop<=arow$Start..Ma.)
  grIDX <- getIdx(arow$Start..Ma., arow$End..Ma., gr$Binned_bottom, gr$Binned_top)

  vbIDX <- getIdx(arow$Start..Ma., arow$End..Ma., volcbolide$Start..Ma., volcbolide$End..Ma.)

  exIDX <- getIdx(arow$Start..Ma., arow$End..Ma., extMag$Base..Ma., extMag$Top..Ma.)

  exIDX2 <- getIdx(arow$Start..Ma., arow$End..Ma., extMag2$Base..Ma., extMag2$Top..Ma.)
  exIDX3 <- getIdx(arow$Start..Ma., arow$End..Ma., extMag3$bottom, extMag3$top)

 # sealevelIDX <- which(sealevel$Time..my. <= arow$End..Ma. & sealevel$Time..my. <= arow$Start..Ma.)
  sealevelIDX <- getIdx(arow$Start..Ma., arow$End..Ma., sealevel$Time..my., sealevel$Time..my.)

  proxyIDX <- getIdx(arow$Start..Ma., arow$End..Ma., proxy$bottom, proxy$top)

  proxyProkIDX <- which(proxyProk$Binned_stage==arow$Bin.name)


  #Now that we have indices for each data file, create a new data frame that
  #extracts the proxy data we want from each one

  #use colMeans unless otherwise appropriate to average over multiple
  #time periods
  envt<-t(colwise(function(x) mean(x, na.rm=T))(proxy[proxyIDX,13:17]))[,1]

  #now that we have indices, get valies

  #extinction rates
  ex<-c(BC.extinction.rate=mean(extMag$BC.extinction.rate[exIDX], na.rm=T))
  ex2<-c(BC.extinction.ratePBDB=mean(extMag2$BC.extinction.rate.PBDB[exIDX2], na.rm=T))
  ex3<-c(BC.extinction.rate.PBDB3=mean(extMag3$BC.extinction.rate.PBDB3[exIDX3], na.rm=T))

  exMax<-c(BC.extinction.rate.max=max(extMag$BC.extinction.rate[exIDX], na.rm=T))
  ex2Max<-c(BC.extinction.ratePBDB.max=mean(extMag2$BC.extinction.rate.PBDB[exIDX2], na.rm=T))
  ex3Max<-c(BC.extinction.rate.PBDB3.max=max(extMag3$BC.extinction.rate.PBDB3[exIDX3], na.rm=T))


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

  return(c(envt, ex, ex2, ex3, exMax, ex2Max, ex3Max, gr, sea, prok, vb))
})

proxies[which(proxies==-Inf, arr.ind=T)] <- NA

#blend the stageTime with the proxies, and write the definitive proxy file
proxiesByStage <- cbind(stageTime, as.data.frame(t(proxies)))

#fix first row and other errors
proxiesByStage <- proxiesByStage[-1,]
proxiesByStage$BC.extinction.rate.max[which(proxiesByStage$BC.extinction.rate.max==-Inf)] <- NA
proxiesByStage$BC.extinction.ratePBDB.max[which(is.nan(proxiesByStage$BC.extinction.ratePBDB.max))] <- NA

#write.csv(proxiesByStage, "../data/cleanProxiesByStage_20131027.csv", row.names=F)
#write.csv(proxiesByStage, "../data/cleanProxiesByStage_20131210.csv", row.names=F)
#write.csv(proxiesByStage, "../data/cleanProxiesByStage_20131212.csv", row.names=F)
write.csv(proxiesByStage, "../data/cleanProxiesByStage_20140612.csv", row.names=F)
