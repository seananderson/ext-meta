############################
# Prep data and functions for the meta-analysis
#
#
# Changelog
# 20121030 - added centered environmental variables to ext
############################


#library and data
library(metafor)
library(plyr)
library(lme4)
library(ggplot2)


#######
# helper functions
#######

#read in a set of formulate for calculating effect sizes and variances
source("../r/conversions.R")

#check the levels of a slice of a data set with a particular trait set
checkLevels<-function(adf, trait) levels(factor(adf[which(adf$Trait==trait),]$Trait.category))

####
#functions  and class to take a trait and output a metafor object and subsetted data file for diagnosis
#####
traitMod<-function(adf, trait, nafilter=F, ...){
  adf<-subset(adf, adf$Aggregate.Trait ==trait)
	if(nafilter){
		adf<-subset(adf, !is.na(adf$lnor))
		adf<-subset(adf, !is.na(adf$vlnor))
	}
	adf$Trait.category<-factor(adf$Trait.category)
	
	res<-rma(yi=lnor, vi=vlnor, data=adf, mods=~Trait.category+0, ...)
	
	ret<-list(model=res, data=adf)
	
	class(ret)<-"traitMeta"

	ret
	
	}

datacheck<-function(adf, trait, nafilter=F){
	adf<-subset(adf, adf$Aggregate.Trait == trait)
	if(nafilter){
		adf<-subset(adf, !is.na(adf$lnor))
		adf<-subset(adf, !is.na(adf$vlnor))
	}
	adf$Trait.category<-factor(adf$Trait.category)

	
	print(levels(adf$Trait.category))
	adf
	}

levelcheck<-function(adf, trait){
	adf<-subset(adf, adf$Aggregate.Trait == trait)
	#adf<-adf[-which(is.na(adf$Trait.category)),]
	
#	print(levels(factor(adf$Trait.category)))
	print(ddply(adf, .(Trait.category), function(x) nrow(x)))

	}	
	
print.traitMeta<-function(obj) print(obj$model)
	
summary.traitMeta<-function(obj) summary(obj$model)

coef.traitMeta<-function(obj) coef(obj$model)

plot.traitMeta<-function(obj) plot(obj$model)

#ggplotting

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


ggplotMetaforCoefs.traitMeta<-function(obj) ggplotMetaforCoefs(obj$model)


#rangeFit<-traitMod(bivalves, "Geographic Range")
#summary(rangeFit)

#################
# analysis and plotting for covariates
#################

#does expand grid over a range of values
fullcurve<-function(amodel, ...) {
  adf<-as.data.frame(expand.grid(...))
  apred<-predict(amodel, adf, se.fit =T, type="response")
  return(cbind(adf, data.frame(fit=apred[[1]], se.fit=apred[[2]])))

}

getCols<-function(rmaObj) colnames(predict(rmaObj, addx=T)$X)

traitCols<-function(cols) 1:(which(cols=="acovariate")-1)

#make a predicted data frame over all possible levels for an RMA object with a covariate
makePredDF<-function(adf, rmaFit, points=100, time=FALSE, usetime=244.15, method="rma"){

  m <- lnor ~ Trait.category * acovariate + 0
  if(time){ 
    m <- lnor ~ Trait.category * acovariate + meanDate + 0
  }
  
  covMax<-max(adf$acovariate, na.rm=T)
  covMin<-min(adf$acovariate, na.rm=T)
  ndf<-expand.grid(Trait.category=levels(adf$Trait.category), 
                   acovariate=seq(covMin, covMax, abs(covMin-covMax)/points))
  if(time){ndf<-cbind(ndf, meanDate=usetime)}

  ndf$lnor<-1
  nf<-model.frame(m, ndf)
  mm<-model.matrix(m, nf)
 
  if(method=="rma"){ 
   #get predictions based on the new data frame
   predDF<-predict(rmaFit, mm)
  
   sink("/dev/null") #supress pringint in next statement
   predDF<-cbind(ndf, as.data.frame(print(predDF)))
   sink()
  
   predDF<-within(predDF, {
     se<-as.numeric(se)
     pred<-as.numeric(pred)
     ci.lb<-as.numeric(ci.lb)
     ci.ub<-as.numeric(ci.ub)
     cr.lb<-as.numeric(cr.lb)
     cr.ub<-as.numeric(cr.ub)
   })
  }
  
  #for lme4 - based on methods from http://glmm.wikidot.com/faq
  #as there is no predict for lme4 on CRAN as of 9/2012
  if(method=="lmer"){
  	predDF<-ndf
  	predDF$pred<-mm %*% fixef(rmaFit) #yes, I know it's not an rma model
  	pvar1 <- diag(mm %*% tcrossprod(vcov(rmaFit),mm))
    tvar1 <- pvar1+VarCorr(rmaFit)$study.ID[1]  ## must be adapted for more complex models
    
    predDF<-within(predDF, {
     se<-2*sqrt(pvar1)
     pred<-as.numeric(pred)
     ci.lb<-predDF$pred-2*sqrt(pvar1)
     ci.ub<-predDF$pred+2*sqrt(pvar1)
     cr.lb<-predDF$pred-2*sqrt(tvar1)
     cr.ub<-predDF$pred+2*sqrt(tvar1)
   })
  }
  
  predDF
}


covariateAnalysis<-function(adf, covariate, return="analysis", points=100, time=F, plot=T, usetime=244.15, method="rma", ci="fixed", ...){
  adf$acovariate<-adf[[covariate]]
  
  if(method=="rma"){
  	if(time){
  	  ret<-rma(yi=lnor, vi=vlnor, data=adf, mods=~Trait.category*acovariate+meanDate +0, ...)
  	}else{
  	  ret<-rma(yi=lnor, vi=vlnor, data=adf, mods=~Trait.category*acovariate +0, ...)
  	}
  	
  	#make the output tables intelligible
  	rownames(ret$b)<-gsub("acovariate", covariate, rownames(ret$b))

  }
  
  if(method=="lmer"){
  	if(time){
  	  ret<-lmer(lnor~Trait.category*acovariate+meanDate +0 + (1|study.ID), weights=1/vlnor, data=adf, ...)
  	}else{
  	  ret<-lmer(lnor~Trait.category*acovariate +0 + (1|study.ID), weights=1/vlnor, data=adf, ...)
  	}
  	
  	#make the output tables intelligible
  	names(attr(ret, "fixef"))<-gsub("acovariate", covariate, names(attr(ret, "fixef")))

  }
  
  
  ##create new data for predict
  predDF<-makePredDF(adf, ret, points=points, time=time, usetime=usetime, method=method)
  
    retPlot<-ggplot(data=adf, aes( x=acovariate, y=lnor, ymin=lnor-1.96*sqrt(vlnor), ymax=lnor+1.96*sqrt(vlnor))) +
    geom_pointrange(size=1.2) +
    facet_wrap(~Trait.category) +
    theme_bw(base_size=16) +
    xlab(covariate)+
    ylab("Extinction Selectivity\n(log odds ratio)\n") +
    geom_hline(aes(y=0), lwd=1, lty=2)

 	if(ci=="fixed") {
 		retPlot<- retPlot + geom_ribbon(data=predDF, aes(x=acovariate, y=pred, ymin=ci.lb, ymax=ci.ub), fill="lightgrey", alpha=0.5) +
    geom_line(data=predDF, aes(x=acovariate, y=pred, ymin=ci.lb, ymax=ci.ub), colour="blue", size=1) 
    }
    
    if(ci=="random") {
 		retPlot<- retPlot + geom_ribbon(data=predDF, aes(x=acovariate, y=pred, ymin=cr.lb, ymax=cr.ub), fill="lightgrey", alpha=0.5) +
    geom_line(data=predDF, aes(x=acovariate, y=pred, ymin=cr.lb, ymax=cr.ub), colour="blue", size=1)
    }

  if(plot){
    print(retPlot)
  }
  
  if(return=="plot") ret<-retPlot
  return(ret)
}

#covariateAnalysis(bivalvesRangeData, "Flood_basalt")


#analysis with covariates
covariateAnalysisOld<-function(adf, covariate, return="analysis"){
  adf$acovariate<-adf[[covariate]]
  ret<-rma(yi=lnor, vi=vlnor, data=adf, mods=~Trait.category*acovariate+0)
#weight=1/vlnor,
 retPlot<-ggplot(data=adf, aes( x=acovariate, y=lnor, ymin=lnor-1.96*sqrt(vlnor), ymax=lnor+1.96*sqrt(vlnor))) +
  geom_pointrange(size=1.2) +
  facet_wrap(~Trait.category) +
  stat_smooth(method="lm") +
  theme_bw(base_size=16) +
  xlab(covariate)
  
  print(retPlot)
  
  if(return=="plot") return(retPlot)
  return(ret)
}

####################################
######data and cleaning
####################################

#read in the data
ext<-read.csv("../data/extinctionMetaClean.csv")


#clean out continuous studies
contExt<-ext[which(!is.na(ext$Trait.category)),]
#ext<-ext[-which(is.na(ext$Trait.category)),]

## add a few extra columns, and centered environmental predictors
ext$MultipleStages <- as.factor(with(ext, as.character(ext$Start.stage) == as.character(ext$End.stage)))
ext$Global.Regional <- as.factor(ext$Global.Regional)


centExt<-colwise(function(x) x-mean(x, na.rm=T))(ext[,60:94])
names(centExt)<-paste(names(centExt), ".cent", sep="")
ext <- cbind(ext, centExt)

