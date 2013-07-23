###########
#load in data, methods, etc.
###########
source("./metaprep.r")

abundData<-subset(ext, ext$Aggregate.Trait=="Abundance")
abundData<-subset(abundData, abundData$Trait.category != "other")
abundData$Trait.category<-factor(abundData$Trait.category)


#subset down to bivalves
bivalves<-subset(abundData, abundData[["Bivalve..Gastropod"]]=="Bivalve")


#Check the bivalve data
#datacheck(bivalves, "Abundance")
levelcheck(bivalves, "Abundance")

#plain vanilla no covariates
bivalveAbundance<-traitMod(bivalves, "Abundance", nafilter=T)

bivalveAbundance
plot(bivalveAbundance)
ggplotMetaforCoefs.traitMeta(bivalveAbundance)+theme_pub(base_size=16)

#with extinction rate
covariateAnalysis(bivalves, "BC.extinction.rate")


#with delta O18
covariateAnalysis(bivalves, "del.18O")
covariateAnalysis(bivalves, "d18O.observations")
covariateAnalysis(bivalves, "d18O.mean")
covariateAnalysis(bivalves, "d18O.StDev")
covariateAnalysis(bivalves, "d18Oresidual")
covariateAnalysis(bivalves, "Dd18O.first.differences.")

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
