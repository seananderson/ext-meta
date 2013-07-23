###########
#load in data, methods, etc.
###########
source("./metaprep.r")

habitData<-subset(ext, ext$Aggregate.Trait=="Life Habit")
habitData$Trait.category[which(habitData$Traut.category=="Epifuanal")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifauna")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifuanl")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifaunalcalciticouterlayer")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifaunalcompetelyaragonitic")]<-"Epifaunal"
                
habitData<-subset(habitData, habitData$Trait.category == "Infaunal" |  habitData$Trait.category == "Epifaunal")
habitData$Trait.category<-factor(habitData$Trait.category)


#subset down to bivalves
bivalves<-subset(habitData, habitData[["Bivalve..Gastropod"]]=="Bivalve")


#Check the bivalve data
#datacheck(bivalves, "Life Habit")
levelcheck(bivalves, "Life Habit")

#plain vanilla no covariates
bivalvehabit<-traitMod(bivalves, "Life Habit", nafilter=T)

bivalvehabit
plot(bivalvehabit)
ggplotMetaforCoefs.traitMeta(bivalvehabit)+theme_pub(base_size=16)

#with extinction rate
covariateAnalysis(bivalves, "BC.extinction.rate")


#with delta O18
covariateAnalysis(bivalves, "del.18O")

#Flood_basalt
covariateAnalysis(bivalves, "Flood_basalt")

#sea level
covariateAnalysis(bivalves, "sea_level")

#volcanism
covariateAnalysis(bivalves, "Volcanism")

covariateAnalysis(bivalves, "del.13C")
