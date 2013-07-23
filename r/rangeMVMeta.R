## @knitr prep
source("../r/metaprep.r")



ext<-extClean

library(mvmeta)
library(reshape2)

ext$UIN<-with(ext, paste(study.ID, Bivalve..Gastropod, Tax.level, Aggregate.Age..Event, Aggregate.Trait, Start.stage,  End.stage, sep="--"))

rangeData<-subset(ext, ext$Aggregate.Trait=="Geographic Range")
rangeData<-subset(rangeData, rangeData$Trait.category != "other")
rangeData<-subset(rangeData, rangeData$Bivalve..Gastropod != "Bivalve and gastropod")
rangeData$Bivalve..Gastropod<-factor(rangeData$Bivalve..Gastropod)
rangeData$Trait.category<-factor(rangeData$Trait.category)
rangeData$BG<-as.numeric(rangeData$Bivalve..Gastropod)-1
rangeData$Tax.level<-factor(rangeData$Tax.level)

#subset down to bivalves
bivalvesRangeData<-subset(rangeData, rangeData[["Bivalve..Gastropod"]]=="Bivalve")
gastropodsRangeData<-subset(rangeData, rangeData[["Bivalve..Gastropod"]]!="Bivalve")
bivalvesRangeData$Tax.level<-factor(bivalvesRangeData$Tax.level)
gastropodsRangeData$Tax.level<-factor(gastropodsRangeData$Tax.level)


  brd<-subset(bivalvesRangeData, !is.na(bivalvesRangeData$lnor) && !is.na(bivalvesRangeData$lnor))

  brdM<-melt(brd, 
             id.vars=c("study.ID", "cluster", "UIN", "Trait.category", "Total...Ext", "Total...Surv",
                       "sea_level", "OA", "Volcanism"), 
             measure.vars=c("lnor", "vlnor", "X..Ext", "X..Surv"))

  brdNew<-dcast(brdM,  cluster+ Total...Ext + 
                Total...Surv+OA + sea_level +
                Volcanism + study.ID ~ variable+Trait.category)
###NORMAL LNOR
regs<-with(brdNew, 
                     {lnORReg(X..Surv_Broad, X..Ext_Broad, X..Surv_Narrow, X..Ext_Narrow)})
names(regs)<-paste(names(regs), "Reg", sep="")

regsFlip<-with(brdNew, 
  {lnORReg(X..Surv_Narrow, X..Ext_Narrow, X..Surv_Broad, X..Ext_Broad)})
names(regsFlip)<-paste(names(regsFlip), "RegFlip", sep="")

plot(lnorReg ~ lnorRegFlip, data=cbind(brdNew, regs, regsFlip))

brdNew<-cbind(brdNew, regs, regsFlip)
#####
#plot and check for dependence
plot(lnor_Narrow ~ lnor_Broad, data=brdNew)
plot(vlnor_Narrow ~ vlnor_Broad, data=brdNew)

plot(lnor_Broad ~ lnorReg, data=brdNew)
plot(lnor_Narrow ~ lnorRegFlip, data=brdNew)

plot(Total...Ext ~ I(X..Ext_Broad+X..Ext_Narrow), data=brdNew)

plot(Total...Surv ~ I(X..Surv_Broad+X..Surv_Narrow), data=brdNew)


rma(yi=lnorReg, vi=vlnorReg, data=brdNew)
lmer(lnor_Broad ~ 1 + (1|study.ID), weights=1/vlnor_Broad, data=brdNew)

 #OK, covariances...