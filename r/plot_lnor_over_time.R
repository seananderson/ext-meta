###########
#load in data, methods, etc.
###########
source("./metaprep.r")

rangeData<-subset(ext, ext$Aggregate.Trait=="Geographic Range")
rangeData<-subset(rangeData, rangeData$Trait.category != "other")
rangeData$Trait.category<-factor(rangeData$Trait.category)


#subset down to bivalves
bivalves<-subset(rangeData, rangeData[["Bivalve..Gastropod"]]=="Bivalve")

ggplot(aes(x=meanDate, y=lnor, ymin= lnor-1.96*sqrt(vlnor), ymax=lnor+1.96*sqrt(vlnor)), data=bivalves) +
	geom_point() + 
	geom_linerange() +
	facet_wrap(~Trait.category) +
	xlab("Mean Date of Period Sampled (mya)") + 
	ylab("Log Odds Ratio\n") +
	opts(title="Bivalves in Different Range Classes\n")+
	theme_bw(base_size=14)
	

#does amount of time sampled matter
ggplot(aes(x=timeSpan, y=lnor, ymin= lnor-1.96*sqrt(vlnor), ymax=lnor+1.96*sqrt(vlnor)), data=bivalves) +
  geom_point() + 
	geom_linerange() +
	facet_wrap(~Trait.category) +
	xlab("\nAmount of time Sampled") + 
	ylab("Log Odds Ratio\n") +
	opts(title="Bivalves in Different Range Classes\n")+
	theme_bw(base_size=14)
	


#and the gastropods
gastropods<-subset(rangeData, rangeData[["Bivalve..Gastropod"]]!="Bivalve")

ggplot(aes(x=meanDate, y=lnor, ymin= lnor-1.96*sqrt(vlnor), ymax=lnor+1.96*sqrt(vlnor)), data=bivalves) +
	geom_point() + 
	geom_linerange() +
	facet_wrap(~Trait.category) +
	xlab("Mean Date of Period Sampled (mya)") + 
	ylab("Log Odds Ratio\n") +
	opts(title="Gastropods in Different Range Classes\n") +
	theme_bw(base_size=14)




###########habitat


habitData<-subset(ext, ext$Aggregate.Trait=="Life Habit")
habitData$Trait.category[which(habitData$Traut.category=="Epifuanal")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifauna")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifuanl")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifaunalcalciticouterlayer")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifaunalcompetelyaragonitic")]<-"Epifaunal"
                
habitData<-subset(habitData, habitData$Trait.category == "Infaunal" |  habitData$Trait.category == "Epifaunal")
habitData$Trait.category<-factor(habitData$Trait.category)


#subset down to bivalves
bivalvesHabit<-subset(habitData, habitData[["Bivalve..Gastropod"]]=="Bivalve")

ggplot(aes(x=meanDate, y=lnor, ymin= lnor-1.96*sqrt(vlnor), ymax=lnor+1.96*sqrt(vlnor)), data=bivalvesHabit) +
  geom_point() + 
	geom_linerange() +
	facet_wrap(~Trait.category) +
	xlab("Mean Date of Period Sampled (mya)") + 
	ylab("Log Odds Ratio\n") +
	opts(title="Bivalves in Different Life Habit Classes\n") +
	theme_bw(base_size=14)
