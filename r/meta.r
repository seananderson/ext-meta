#library and data
library(metafor)
library(plyr)
library(lme4)
library(ggplot2)


#read in a set of formulate for calculating effect sizes and variances
source("./conversions.R")

#read in the data
ext<-read.csv("../data/extinctionMetaClean.csv")

#######
# helper functions
#######

#check the levels of a slice of a data set with a particular trait set
checkLevels<-function(adf, trait) levels(factor(adf[which(adf$Trait==trait),]$Trait.category))

####
#functions  and class to take a trait and output a metafor object and subsetted data file for diagnosis
#####
traitMod<-function(adf, trait){
	adf<-subset(adf, adf$Aggregate.Trait ==trait)
	adf<-adf[-which(is.na(adf$Trait.category)),]

	adf<-sort.data.frame(adf, ~Trait.category)
	adf$Trait.category<-factor(adf$Trait.category)
	adf <-adf[-which(is.infinite(adf$vlnor)),]

	#adf<-adf[-which(is.infinite(adf$lnor)),]
	
	res<-try(rma(yi=lnor, vi=vlnor, data=adf, mods=~Trait.category+0))
	
	ret<-list(model=res, data=adf)
	
	class(ret)<-"traitMeta"

	ret
	
	}
	
print.traitMeta<-function(obj) print(obj$model)
	
summary.traitMeta<-function(obj) summary(obj$model)

coef.traitMeta<-function(obj) coef(obj$model)

plot.traitMeta<-function(obj) plot(obj$model)

ggplotMetaforCoefs.traitMeta<-function(obj) ggplotMetaforCoefs(obj$model)

#rangeFit<-traitMod(bivalves, "Geographic Range")
#summary(rangeFit)


######data analysis

#subset down to bivalves
bivalves<-subset(ext, ext[["Bivalve..Gastropod"]]=="Bivalve")
bivalves$Trait<-purgef(bivalves$Trait)
levels(bivalves$Aggregate.Trait)


gastropods<-subset(ext, ext[["Bivalve..Gastropod"]]!="Bivalve")


#calculating lnOR via the MH method


###What can we look at?
ddply(bivalves, .(Aggregate.Trait), function(adf) nrow(adf))
ddply(gastropods, .(Aggregate.Trait), function(adf) nrow(adf))
#answer: life habit and geographic range


#fit a model for range
rngBV<-subset(bivalves, bivalves$Trait=="Geographic Range")
#clean the data
rngBV<-rngBV[-which(is.na(rngBV$Trait.category)),]
rngBV<-rngBV[-which(is.infinite(rngBV$vlnor)),]
#rngBV<-rngBV[-which(is.na(rngBV$vlnor)),]
rngBV$Trait.category<-factor(rngBV$Trait.category)
rngBV<-sort.data.frame(rngBV, ~Trait.category)

rng<-rma(yi=lnor, vi=vlnor, data=rngBV, mods=~Trait.category+0)
rngTemp<-rma(yi=lnor, vi=vlnor, data=rngBV, mods=~Trait.category*del.18O+0)

summary(rng)
plot(rng)

#mixed model approach
rngMER<-lmer(lnor ~ Trait.category + 0+  (1+Trait.category|In.text.Citation), data=rngBV, weights=vlnor)
summary(rngMER)

rngMER2<-lmer(lnor ~ Trait.category + 0+  (1+Trait.category|In.text.Citation) + (0+Trait.category|Aggregate.Age..Event), data=rngBV, weights=vlnor)

summary(rngMER2)
ranef(rngMER2)

##Some plotting
with(rngBV,{
forest(rng, slab=paste(Trait.category, In.text.Citation,  Location, Age..Event, sep=" "))
})
text(149, 101, "Log Odds Ratio [95% CI]", pos = 2)


#Body Size
#body<-rma(yi=lnor, vi=vlnor, data=subset(bivalves, bivalves$Aggregate.Trait=="Body Size"), mods=~Trait.category+0) #Too Small Sample size
#summary(body)

#############
#Feeding Mode
##############
feedBV<-bivalves[which(bivalves$Trait.category=="Epifaunal" | bivalves$Trait.category=="Infaunal"),]
feedBV$Trait.category<-purgef(feedBV$Trait.category)
feedBV<-sort.data.frame(feedBV, ~Trait.category)
feedBV<-feedBV[which(!is.na(feedBV$lnor)),]

feed<-rma(yi=lnor, vi=vlnor, data=feedBV, mods=~Trait.category+0) 
summary(feed)

#with lme4
feedMER<-lmer(lnor ~ Trait.category + 0+ (1|In.text.Citation), data=feedBV, weights=vlnor)
summary(feedMER)

##############
#Gastropods
###############
#range

#fit a model for range
rngGP<-subset(gastropods, gastropods$Trait=="Geographic Range")
#clean the data
#rngGP <-rngGP[-which(is.na(rngGP$Trait.category)),]
#rngGP <-rngGP[-which(is.infinite(rngGP$vlnor)),]
rngGP$Trait.category<-purgef(rngGP$Trait.category)
rngGP <-sort.data.frame(rngGP, ~Trait.category)

rngG<-rma(yi=lnor, vi=vlnor, data= rngGP, mods=~Trait.category+0)



##Some plotting
forest(feed, slab=paste(feedBV$Trait.category, feedBV$In.text.Citation,  feedBV$Location, feedBV$Age..Event, sep=" "))
text(31, 50, "Log Odds Ratio [95% CI]", pos = 2)


#plotting effect sizes using ggplot2
ggplotMetaforCoefs<-function(obj){
  oCoefs<-coef(obj)
  oCoefs$categories<-gsub("Trait.category", "", rownames(oCoefs))
  
  n<-colSums(obj$X, na.rm=T)
  
  oCoefs$categories<-paste(oCoefs$categories, n, sep="\nn=")

  ggplot(data=oCoefs)+geom_pointrange(aes(x=categories, y=estimate, ymin=ci.lb, ymax=ci.ub), size=1.5) +
    geom_abline(aes(intercept=0, slope=0), size=2, linetype=2) +
    xlab("")+
    ylab("Extinction Selectivity\n(Log Odds Ratio)\n")
}

ggplotMetaforCoefs(rng)+theme_bw(base_size=24)

ggplotMetaforCoefs(feed)+theme_bw(base_size=18)
ggplotMetaforCoefs(rngG)+theme_bw(base_size=18)
 
      
      
      
      
