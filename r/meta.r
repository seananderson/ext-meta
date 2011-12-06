#library and data
library(metafor)
library(plyr)
library(lme4)


#for calculating the lnor via the Mantel-Hazel method
lnORMH<-function(tpos, tneg, cpos, cneg){
  o <- tpos
  ohat <- (tpos+cpos)*(tpos+tneg)/(tpos+tneg+cpos+cneg)
  v <- ohat*((tpos+tneg)/(tpos+tneg+cpos+cneg))*((tneg+cneg)/(tpos+tneg+cpos+cneg-1))
	
	lnor <- (o-ohat)/v
	vlnor <- 1/v
  
  return(data.frame(lnor=lnor, vlnor=vlnor))
  
}

ext<-read.csv("../data/orzMeta-analysis_ marine selectivity 11-18.csv", skip=1, na.strings=c("NA", "N/A", ".", ""))

#ext<-cbind(ext, with(ext, {lnORMH(X..Surv, X..Ext, Total...Surv, Total...Ext)}))
ext<-cbind(ext, with(ext, {lnORMH(X..Surv, X..Ext, Total...Surv-X..Surv, Total...Ext-X..Ext)}))

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
rngBV$Trait.category<-purgef(rngBV$Trait.category)
rngBV<-sort.data.frame(rngBV, ~Trait.category)

rng<-rma(yi=lnor, vi=vlnor, data=rngBV, mods=~Trait.category+0)

summary(rng)
plot(rng)

#mixed model approach
rngMER<-lmer(lnor ~ Trait.category + 0+  (1+Trait.category|In.text.Citation), data=rngBV, weights=vlnor)
summary(rngMER)


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


##Some plotting
forest(feed, slab=paste(feedBV$Trait.category, feedBV$In.text.Citation,  feedBV$Location, feedBV$Age..Event, sep=" "))
text(31, 50, "Log Odds Ratio [95% CI]", pos = 2)

library(ggplot2)

#plotting effect sizes using ggplot2
ggplotMetaforCoefs<-function(obj){
  oCoefs<-coef(obj)
  oCoefs$categories<-gsub("Trait.category", "", rownames(oCoefs))
  
  n<-colSums(obj$X, na.rm=T)
  
  oCoefs$categories<-paste(oCoefs$categories, n, sep="\nn=")

  ggplot(data=oCoefs)+geom_pointrange(aes(x=categories, y=estimate, ymin=ci.lb, ymax=ci.ub), size=1.5) +
    geom_abline(aes(intercept=0, slope=0), size=2, linetype=2) +
    xlab("")+
    ylab("Log Odds Ratio")
}

ggplotMetaforCoefs(feed)+theme_bw(base_size=18)
ggplotMetaforCoefs(rng)+theme_bw(base_size=18)
 
      
      
      
      
