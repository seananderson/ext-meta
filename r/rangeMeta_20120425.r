###########
#load in data, methods, etc.
###########
source("./metaprep.r")
#with(ext, {pairs(cbind(del.18O, d18O.meanObserved, d18OresidualMean, d18OresidualMeanWeighted, d18OresidualSD, Dd18O.first.differences.Mean,Dd18O.first.differences.ABSMean, Dd18O.first.differences.SD))})

rangeData<-subset(ext, ext$Aggregate.Trait=="Geographic Range")
rangeData<-subset(rangeData, rangeData$Trait.category != "other")
rangeData$Trait.category<-factor(rangeData$Trait.category)


#subset down to bivalves
bivalves<-subset(rangeData, rangeData[["Bivalve..Gastropod"]]=="Bivalve")


#Check the bivalve data
#datacheck(bivalves, "Geographic Range")
levelcheck(bivalves, "Geographic Range")

#plain vanilla no covariates
bivalveRange<-traitMod(bivalves, "Geographic Range", nafilter=T)

bivalveRange
plot(bivalveRange)
ggplotMetaforCoefs.traitMeta(bivalveRange)+theme_bw(base_size=16)

#with extinction rate
covariateAnalysis(bivalves, "BC.extinction.rate")
covariateAnalysis(bivalves, "BC.extinction.rate.PBDB")


#with delta O18
covariateAnalysis(bivalves, "del.18O")
covariateAnalysis(bivalves, "d18O.meanObserved") #shows old pattern, so, detrend


#detrended by seth
covariateAnalysis(bivalves, "d18OresidualMean")
covariateAnalysis(bivalves, "d18OresidualMeanWeighted") #no difference when adding weighting - good!
covariateAnalysis(bivalves, "d18OresidualSD") #nada

#just to check...
cor(bivalves$d18OresidualMean, bivalves$d18OresidualSD) #no correlation
rma(yi=lnor, vi=vlnor, data=bivalves, mods=~Trait.category*d18OresidualMean*d18OresidualSD +0) #no real change when using both

#ok, what about the first differences
covariateAnalysis(bivalves, "Dd18O.first.differences.Mean")
covariateAnalysis(bivalves, "Dd18O.first.differences.ABSMean")
covariateAnalysis(bivalves, "Dd18O.first.differences.SD")

rma(yi=lnor, vi=vlnor, data=bivalves, mods=~Trait.category*Dd18O.first.differences.Mean*Trait.category*Dd18O.first.differences.SD +0) #no real change when using both



#Flood_basalt
covariateAnalysis(bivalves, "Flood_basalt")

#sea level
covariateAnalysis(bivalves, "sea_level")

#volcanism
covariateAnalysis(bivalves, "Volcanism")

#volcanism
covariateAnalysis(bivalves, "del.13C")

#hrm
#rma(yi=lnor, vi=vlnor, data=bivalves, mods=~Trait.category*del.18O*Volcanism*sea_level+0)

############
# Gastropods
#############


#gastropod subset
gastropods<-subset(rangeData, rangeData[["Bivalve..Gastropod"]]!="Bivalve")
levelcheck(gastropods, "Geographic Range")

#plain vanilla no covariates
gastropodRange<-traitMod(gastropods, "Geographic Range", nafilter=T)

gastropodRange
plot(gastropodRange)
ggplotMetaforCoefs.traitMeta(gastropodRange)+theme_bw(base_size=16)

#Cannot add covariates due to low sample size in intermediates

#############
# Pooled
#############
#plain vanilla no covariates
RangeMod<-traitMod(rangeData, "Geographic Range", nafilter=T)

RangeMod
plot(RangeMod)
ggplotMetaforCoefs.traitMeta(RangeMod)+theme_pub(base_size=16)

#with extinction rate
covariateAnalysis(rangeData, "BC.extinction.rate")

#with delta O18
covariateAnalysis(rangeData, "del.18O")

#Flood_basalt
covariateAnalysis(rangeData, "Flood_basalt")

#sea level
covariateAnalysis(rangeData, "sea_level")

#volcanism
covariateAnalysis(rangeData, "Volcanism")

