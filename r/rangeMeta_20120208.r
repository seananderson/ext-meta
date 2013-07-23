###########
#load in data, methods, etc.
###########
source("./metaprep.r")

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

#with delta O18
covariateAnalysis(bivalves, "del.18O")

#volcanism
covariateAnalysis(bivalves, "Volcanism")

#volcanism
covariateAnalysis(bivalves, "Bollide")


#Flood_basalt
covariateAnalysis(bivalves, "Flood_basalt")

#sea level
covariateAnalysis(bivalves, "sea_level")


#Acidification
covariateAnalysis(bivalves, "OA")


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