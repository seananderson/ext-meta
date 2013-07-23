#########################################################################################################
# Merge and Compare Proxy Metrics
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
proxy<-read.csv("../data/Hannisdal and Peters data with stage IDs.csv", na.strings="?")


######new sealevel data
sealevel<-read.csv("../data/Sea level residuals after 2nd-order polynomial fit.csv")



#Veizer delta 18O
veizer<-read.csv("../data/veizer_d18O.csv")
veizer$Length<-veizer$StageBottom-veizer$StageTop

counter <<- 0

stageMat <- data.frame(startStage = stageTime$Bin.name, endStage = stageTime$Bin.name)

envtCols <- t(apply(stageMat, 1, function(arow){  counter <<- counter+1
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
  veizerIDX<-c(which(veizer$StageBottom >= times[1])[1], #first time within the stage
              which(veizer$StageTop > times[2])[1]-1 #last time within the stage
    )
  
  #get the means for each column of the environmental data
  #rgh - kludge to turn a df into a vector, as colwise turns
  #out data frames
#  envt<-t(colwise(mean)(proxy[proxyIDX,2:ncol(proxy)]))[,1] #old proxy data
  envt<-t(colwise(function(x) mean(x, na.rm=T))(proxy[proxyIDX,13:17]))[,1]
  
  
  #query whether there were any events, and set the return value to 1 if so
  vb<-as.numeric(colSums(volcbolide[vbIDX,3:6])>0)
  names(vb)<-names(volcbolide[3:6])
  
  ex<-c(BC.extinction.rate=mean(extMag$BC.extinction.rate[exIDX]))
  ex2<-c(BC.extinction.ratePBDB=mean(extMag2$BC.extinction.rate[exIDX2]))
  
  vez<-t(colwise(function(x) mean(x, na.rm=T))(veizer[veizerIDX,4:10]))[,1]
#  vexWeight<-
  vez<-with(veizer[veizerIDX,], {
                      c(d18OresidualMean=mean(d18Oresidual),
                        d18OresidualMeanWeighted=weighted.mean(d18Oresidual, Length),
                        d18OresidualSD=sd(d18Oresidual),
                        Dd18O.first.differences.Mean=mean(Dd18O.first.differences., na.rm=T),
                      #  Dd18O.first.differences.MeanWeighted=weighted.mean(Dd18O.first.differences., length, na.rm=T),
                        Dd18O.first.differences.SD=sd(Dd18O.first.differences., na.rm=T),
                        Dd18O.first.differences.ABSMean=mean(abs(Dd18O.first.differences.), na.rm=T),
                        Dd18O.first.differences.ABSMeanWeighted = weighted.mean(abs(Dd18O.first.differences.), Length, na.rm=T),
                        d18O.meanObserved =mean(d18O.mean ),
                        d18O.meanObservedWeighted = weighted.mean(d18O.mean, Length),
                        d18O.meanObservedSD = sd(d18O.mean),
                        d18O.CV = sd(d18O.mean)/ mean(d18O.mean)
                        )})
                        
    sea <- with(sealevel[sealevelIDX,], {
    	c(sea_level...first.diffs. = mean(sea_level...first.diffs.),
    	sea_level_residuals = mean(sea_level_residuals))
    }
    )
    
    
  ret<-c(times, vb, ex, envt, vez, ex2, sea)
  names(ret)<-names(ret)
  
  if(flag==1) ret<-rep(NA, length(ret))
  
  return(ret)
}))

envtCols<-as.data.frame(envtCols)

cmat <- cor(envtCols, use="pairwise.complete.obs")
cor(envtCols, use="pairwise.complete.obs", method="s")

library(reshape2)
write.csv(melt(cmat), "../envt_cors.csv", sep=",")
cmatMelt <- melt(cmat)

cmatMelt[which(cmatMelt[,3]>0.6),]