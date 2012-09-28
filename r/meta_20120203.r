###########
#load in data, methods, etc.
###########
source("./metaprep.r")


#subset down to bivalves
bivalves<-subset(ext, ext[["Bivalve..Gastropod"]]=="Bivalve")
bivalves$Aggregate.Trait<-factor(bivalves$Aggregate.Trait)
levels(bivalves$Aggregate.Trait)

#gastropod subset
gastropods<-subset(ext, ext[["Bivalve..Gastropod"]]!="Bivalve")
gastropods$Aggregate.Trait<-factor(gastropods $Aggregate.Trait)


###What can we look at?
ddply(bivalves, .(Aggregate.Trait), function(adf) nrow(adf))
ddply(gastropods, .(Aggregate.Trait), function(adf) nrow(adf))
#answer: life habit and geographic range

#################################
######BIVALVES
#################################
#Abundance
datacheck(bivalves, "Abundance")
#bivalveAbundance<-traitMod(bivalves, "Abundance")

#Body Size
datacheck(bivalves, "Body Size")
levelcheck(bivalves, "Body Size")
#bivalveBody<-traitMod(bivalves, "Body Size")

#Escalation
datacheck(bivalves, "Escalation")
levelcheck(bivalves, "Escalation")
bivalveEscalation<-traitMod(bivalves, "Escalation")
bivalveEscalation
plot(bivalveEscalation)
ggplotMetaforCoefs.traitMeta(bivalveEscalation)

#Geographic Range
datacheck(bivalves, "Geographic Range")
levelcheck(bivalves, "Geographic Range")

#bivalveRange<-traitMod(bivalves, "Geographic Range")
bivalveRange<-traitMod(bivalves, "Geographic Range", nafilter=T)

bivalveRange
plot(bivalveRange)
ggplotMetaforCoefs.traitMeta(bivalveRange)

#with covariate
adf<-datacheck(bivalves, "Geographic Range", nafilter=T)
rma(yi=lnor, vi=vlnor, data=adf, mods=~Trait.category*del.18O+0)
qplot(del.18O, lnor, data=adf, facets=~Trait.category)+stat_smooth(method="lm")


#Life Habit
datacheck(bivalves, "Life Habit", nafilter=T)
levelcheck(bivalves, "Life Habit")
bivalveHabit<-traitMod(bivalves, "Life Habit", nafilter=T)
bivalveHabit
plot(bivalveHabit)
ggplotMetaforCoefs.traitMeta(bivalveHabit)


##############
#Gastropods
###############
#Body Size
datacheck(gastropods, "Body Size")
levelcheck(gastropods, "Body Size")

#Escalation
datacheck(gastropods, "Escalation")
levelcheck(gastropods, "Escalation")
gastropodEscalation<-traitMod(gastropods, "Escalation")
gastropodEscalation
plot(gastropodEscalation)
ggplotMetaforCoefs.traitMeta(gastropodEscalation)

#Geographic Range
datacheck(gastropods, "Geographic Range")
levelcheck(gastropods, "Geographic Range")

##Some plotting
forest(feed, slab=paste(feedBV$Trait.category, feedBV$In.text.Citation,  feedBV$Location, feedBV$Age..Event, sep=" "))
text(31, 50, "Log Odds Ratio [95% CI]", pos = 2)


ggplotMetaforCoefs(rng)+theme_bw(base_size=24)

ggplotMetaforCoefs(feed)+theme_bw(base_size=18)
ggplotMetaforCoefs(rngG)+theme_bw(base_size=18)
 
      
      
      
      
