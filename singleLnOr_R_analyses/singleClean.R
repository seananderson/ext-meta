##################################
##
## Code to analyze log-odds ratios
## using only single response variables 
## from NESCent Working Group
##
## Last updated 20120921
##
## Changelog
##################################

#####
# Load libraries and data
#####
## @knitr prep
source("../r/metaprep.r")
source("../r/lmerMetaPrep.R")

## @knitr loadBroad
## First, let's look at the broadly distributed data
## and the difference between broad and narro
## So, subset the data down to broad being the grouping trait of interest
broadData<-subset(ext, ext$Aggregate.Trait=="Geographic Range" &  ext$Trait.category == "Broad")
broadData$Tax.level<-factor(broadData$Tax.level)
bivalveBroadData<-subset(broadData, broadData[["Bivalve..Gastropod"]]!="Bivalve")
gastropodsBroadData<-subset(broadData, broadData[["Bivalve..Gastropod"]]!="Gastropod")
bivalveBroadData$Tax.level<-factor(bivalveBroadData$Tax.level)
gastropodsBroadData$Tax.level<-factor(gastropodsBroadData$Tax.level)



#####
### Construct catepillar plots
### showing log odds ratio and how it differs by
### taxonomic grouping and bivalve/gastropod split
#####

## @knitr effect.Plot.Broad
ord<-sort(broadData$lnorReg, index.return=T)$ix #sorting for catepillar plotting

catPlot.Broad<-ggplot(data=broadData[ord,], aes( y=lnorReg, x=1:nrow(broadData), 
                                           ymin=lnorReg-1.96*sqrt(vlnorReg), 
                                           ymax=lnorReg+1.96*sqrt(vlnorReg))) + 
              geom_point() + 
              geom_linerange()+ 
              theme_bw(base_size=18) + 
              xlab("") + 
              geom_hline(y=0, lty=2, lwd=1) + 
              theme(
                axis.text.x= element_blank(),
                axis.ticks.x = element_blank())
                      
catPlot.Broad+
  facet_grid(Tax.level ~ Bivalve..Gastropod, scales="free_x")

#####
### Examine the effect of taxonomic group and bivalve/gastropod
### for log odds ratio of broad v. narrow using lmer
#####
## @knitr BroadTaxaBGLMER
taxGenera.Broad<-lmer(lnorReg ~ Bivalve..Gastropod+Tax.level+ (1|study.ID), data=broadData, weights=1/vlnorReg)
taxGenera.Broad


#####
### Test the effect of adding tax level or bivalve/gastropod
### using chi square tests by dropping one predictor or the other
### (type II test) (although we have no interaction)
#####
## @knitr BroadTaxaBGAnova
bgDrop<-update(taxGenera.Broad, .~ . -Bivalve..Gastropod )
anova(taxGenera.Broad, bgDrop)

tDrop<-update(taxGenera.Broad, .~ . -Tax.level )
anova(taxGenera.Broad, tDrop)


#####
### As there was no tax level or bivalve/gastopod effect
### plot the pooled catepillar plot and show the mean and CI of the mean
#####
## @knitr BroadTaxaBGMeansSame
catPlot.Broad+geom_hline(aes(yintercept=fixef(taxGenera.Broad)[1]), color="red", lwd=2) +
  geom_ribbon(aes( 
                  ymin=rep(fixef(taxGenera.Broad)[1]-1.96*se.fixef(taxGenera.Broad)[1]), 
                  ymax=rep(fixef(taxGenera.Broad)[1]+1.96*se.fixef(taxGenera.Broad)[1])), 
                               fill="pink", alpha=0.5) +
  annotate("text", x=5, y=4.5,label=paste("mean = ", round(fixef(taxGenera.Broad)[1], 2), sep=""))+
  annotate("text", x=65, y=-2.2, label = paste("n = ", nrow(broadData), sep=""))



##########
### OK, so, we can pool.  Let's run a grand model!
###
###
### This is the big model.  Use names(broadData) to find out other things that
### can be added
##########

## @knitr bigBroadModel
covModel.Broad<-lmer(lnorReg ~ OA  + BC.extinction.rate.PBDB + 
                    d18OresidualMean + del.34S + (1|study.ID),
                     data=broadData, weights=1/vlnorReg)

covModel.Broad

## @knitr bigBroadModelDiagnostics
hist(residuals(covModel.Broad))
plot(residuals(covModel.Broad) ~ fitted(covModel.Broad))


#####
### Let's plot the grand model!
#####
## @knitr bigBroadModelCoefPlot
covModelCoefPlot.Broad<-coefPlot(covModel.Broad)

covModelCoefPlot.Broad 


## @knitr bigBroadModelCoefPlotSTD
coefPlot(covModel.Broad, std=T)



####What are the marginal effects from the model
## @knitr margOABroad
marginalLine(covModel.Broad, "OA", broadData)


## @knitr margdel.34SBroad
marginalLine(covModel.Broad, "del.34S", broadData)


## @knitr sectionbreak
########################################
## Epifauna v. Infauna
########################################

## @knitr loadEpifaunal
## First, let's look at the broadly distributed data
## and the difference between broad and narro
## So, subset the data down to broad being the grouping trait of interest
habitData<-subset(ext, ext$Aggregate.Trait=="Life Habit")
habitData$Trait.category[which(habitData$Traut.category=="Epifuanal")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifauna")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifuanl")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifaunalcalciticouterlayer")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Traut.category=="Epifaunalcompetelyaragonitic")]<-"Epifaunal"

habitData<-subset(habitData, habitData$Trait.category == "Epifaunal")
habitData$Trait.category<-factor(habitData$Trait.category)


#subset down to bivalves
bivalvesEpifaunal<-subset(habitData, habitData[["Bivalve..Gastropod"]]=="Bivalve")
bivalvesEpifaunal$Tax.level<-factor(bivalvesEpifaunal$Tax.level)

#####
### Construct catepillar plots
### showing log odds ratio and how it differs by
### taxonomic grouping and bivalve/gastropod split
#####


#####
### So, we pool.  Let's look at things, then
#####

## @knitr Epifaunal_lmer
meanModel.Epifaunal<-lmer(lnorReg ~ 1 + (1|study.ID), data=bivalvesEpifaunal, weights=1/vlnorReg)
summary(meanModel.Epifaunal)

#the catepillar plot
#I need to abstract this....
ordE<-sort(bivalvesEpifaunal$lnorReg, index.return=T)$ix #sorting for catepillar plotting

catPlot.Epifaunal<-ggplot(data=bivalvesEpifaunal[ordE,], aes( y=lnorReg, x=1:length(lnorReg),
                                                              ymin=lnorReg-1.96*sqrt(vlnorReg), 
                                                              ymax=lnorReg+1.96*sqrt(vlnorReg))) + 
                                                                geom_point() + 
                                                                geom_linerange() +
                                                                theme_bw(base_size=18) + 
                                                                xlab("") + 
                                                                geom_hline(y=0, lty=2, lwd=1) + 
                                                                theme(
                                                                  axis.text.x= element_blank(),
                                                                  axis.ticks.x = element_blank())+ xlab("")


catPlot.Epifaunal +geom_hline(aes(yintercept=fixef(meanModel.Epifaunal)[1]), color="red", lwd=2) +
  geom_ribbon(aes( 
    ymin=rep(fixef(meanModel.Epifaunal)[1]-1.96*se.fixef(meanModel.Epifaunal)[1]), 
    ymax=rep(fixef(meanModel.Epifaunal)[1]+1.96*se.fixef(meanModel.Epifaunal)[1])), 
              fill="pink", alpha=0.5) +
    annotate("text", x=5, y=4,label=paste("mean = ", round(fixef(meanModel.Epifaunal)[1], 2), sep="")) +
    annotate("text", x=25, y=-2, label = paste("n = ", nrow(bivalvesEpifaunal), sep=""))


#####
### OK, so, we can pool.  Let's run a grand model!
#####

## @knitr bigEpifaunaModel
covModel.Epifaunal<-lmer(lnorReg ~ OA  + BC.extinction.rate.PBDB + 
                         d18OresidualMean + del.34S + (1|study.ID),
                     data=bivalvesEpifaunal, weights=1/vlnorReg)

covModel.Epifaunal

## @knitr bigEpifaunaModelDiagnostics
hist(residuals(covModel.Epifaunal))
plot(residuals(covModel.Epifaunal) ~ fitted(covModel.Epifaunal))

## @knitr plotBigEpifaunaModel
coefPlot(covModel.Epifaunal)


## @knitr plotBigEpifaunaModel_STD
coefPlot(covModel.Epifaunal, std=TRUE)


####What are the marginal effects from the model
## @knitr margO18Epifauna
marginalLine(covModel.Epifaunal, "d18OresidualMean", bivalvesEpifaunal)


## @knitr marg34SEpifauna
marginalLine(covModel.Epifaunal, "del.34S", bivalvesEpifaunal) + xlim(c(14,20))
