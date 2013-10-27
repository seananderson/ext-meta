#########################################################################################################
# Take in all of the various proxy data files and merge them
# with the stageTime file, as the meta-analysis uses stages in order
# to differentiate between papers rather than times
#
# We're gonna have to write *very* good meta-data for this file later
#
# coded by Jarrett Byrnes
#
# Last change: 10/28/2013
#
# Changelog
#
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
ext<-read.csv("../data/ext-meta-20121030.csv", skip=1, na.strings=c("NA", "N/A", ".", ""))

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

####################
#
# Pull in environmental covariates
#
####################

#Columns with start and end stages from the data file
stageIDX<-which(names(ext) %in% c("Start.stage", "End.stage"))

#read in the data set with the conversions between stage and time
stageTime<-read.csv("../data/Stage vs time.csv")

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

#####volvacanism and bollide impacts
#first, pull in volcanism and bolides
volcbolide<-read.csv("../data/Flood Basalt Bolide Ocean Acidification.csv")
volcbolide$State.stage<-gsub(" $", "", volcbolide$State.stage)


#add times
volcbolide <- cbind(volcbolide, t(sapply(volcbolide$State.stage, function(x) getTimeRange(x,x))))

#fix errors in time due to bad stage names
volcbolide[which(volcbolide$State.stage=="unnamed Pridoli stage"),7:8] <- c(418.8, 418.1)


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

#pdf("figure/detrending-plots.pdf")
#detrend the delta 34S
proxy$del.34S.detrended <- with(proxy, detrend_ts(top, del.34S, "Hannisdal and Peters d34S"))


######new sealevel data
sealevel<-read.csv("../data/Sea level residuals after 2nd-order polynomial fit.csv")



#Veizer delta 18O
veizer<-read.csv("../data/veizer_d18O.csv")
veizer$Length<-veizer$StageBottom-veizer$StageTop

counter <<- 0


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

proxies <- sapply(1:nrow(stageTime),  function(x){
  arow <- stageTime[x,]
  veizerIDX <- which(veizer$StageBottom>=arow$End..Ma. & veizer$StageTop<=arow$Start..Ma.)
  grIDX <- getIdx(arow$End..Ma., arow$Start..Ma., gr$Binned_bottom, gr$Binned_top)
  volcbolideIDX <- getIdx(arow$End..Ma., arow$Start..Ma., volcbolide$endTime.Ma, volcbolide$startTime.Ma)
  exIDX <- getIdx(arow$End..Ma., arow$Start..Ma., extMag$Base_bottom..Ma., extMag$Base..Ma.)
  exIDX2 <- getIdx(arow$End..Ma., arow$Start..Ma., extMag2$Base_bottom..Ma., extMag2$Base..Ma.)
  sealevelIDX <- which(sealevel$Time..my. <= arow$End..Ma.)[1]
  proxyIDX <- getIdx(arow$End..Ma., arow$Start..Ma., proxy$bottom, proxy$top)
  
  #Now that we have indices for each data file, create a new data frame that 
  #extracts the proxy data we want from each one
  
  #use colMeans unless otherwise appropriate to average over multiple
  #time periods
  envt<-t(colwise(function(x) mean(x, na.rm=T))(proxy[proxyIDX,13:17]))[,1]

  #query whether there were any events, and set the return value to 1 if so
  vb<-as.numeric(colSums(volcbolide[vbIDX,3:6])>0)
  names(vb)<-names(volcbolide[3:6])
  
  #extinction rates
  ex<-c(BC.extinction.rate=mean(extMag$BC.extinction.rate[exIDX]))
  ex2<-c(BC.extinction.ratePBDB=mean(extMag2$BC.extinction.rate[exIDX2]))
  
  #Data from Grossman
  gr <- colMeans(gr[grIDX,4:9])
  
  #sealevel
  sea <- with(sealevel[sealevelIDX,], {
    c(sea_level...first.diffs. = mean(sea_level...first.diffs.),
      sea_level_residuals = mean(sea_level_residuals))
  })
  
  return(c(envt, vb, ex, ex2, gr, sea))
})


#blend the stageTime with the proxies, and write the definitive proxy file
proxiesByStage <- cbind(stageTime, as.data.frame(t(proxies)))

write.csv(proxiesByStage, "../data/cleanProxiesByStage_20131027.csv", row.names=F)