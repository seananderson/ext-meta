##################################
##
## Final code to analyze log-odds ratios
## using only single response variables
## from NESCent Working Group for publication
##
## Created:       Jan 13, 2012
## Last modified: Jul 17, 2014
## Purpose:       Try plotting the effect sizes against the raw data.
## Additional description: More analyses can be found in singleLnOr_R_analyses/singleLnOr_rma.R
## Changelog
##
##################################

#####
# Load libraries and data
#####
## @knitr prep
library(metafor)
library(ggplot2)
library(gridExtra)

source("../r/metaprep.r")
source("../r/rmaPrep.R")
source("../r/catPlotFun.R")


#for prettier printing in Figures
levels(ext$Tax.level) <- c("Genera", "Species", "Subgenera")

## @knitr loadData
## First, let's look at the broadly distributed data
## and the difference between broad and narro
## So, subset the data down to broad being the grouping trait of interest
broadData<-subset(ext, ext$Aggregate.Trait=="Geographic Range" &  ext$Trait.category == "Broad")
broadData$Tax.level<-factor(broadData$Tax.level)
bivalveBroadData<-subset(broadData, broadData[["Bivalve..Gastropod"]]!="Bivalve")
gastropodsBroadData<-subset(broadData, broadData[["Bivalve..Gastropod"]]!="Gastropod")
bivalveBroadData$Tax.level<-factor(bivalveBroadData$Tax.level)
gastropodsBroadData$Tax.level<-factor(gastropodsBroadData$Tax.level)

## First, let's look at the broadly distributed data
## and the difference between broad and narro
## So, subset the data down to broad being the grouping trait of interest
habitData<-subset(ext, ext$Aggregate.Trait=="Life Habit")
habitData$Trait.category[which(habitData$Trait.category=="Epifuanal")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Trait.category=="Epifauna")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Trait.category=="Epifuanl")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Trait.category=="Epifaunalcalciticouterlayer")]<-"Epifaunal"
habitData$Trait.category[which(habitData$Trait.category=="Epifaunalcompetelyaragonitic")]<-"Epifaunal"

habitData<-subset(habitData, habitData$Trait.category == "Epifaunal")
habitData$Trait.category<-factor(habitData$Trait.category)
habitData$Tax.level<-factor(habitData$Tax.level)
habitData<-habitData[which(habitData$vlnorReg>0),]

habitData$study.ID <- factor(habitData$study.ID)


##########
## Grand Model for Broad
##########
## @knitr bigBroadModelRMAPrep
broadDataExtinction <- broadData[which(!is.na(broadData$BC.extinction.ratePBDB)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$del.18O)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$del.34S)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$del.13C)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$vlnorReg)),]

broadDataExtinctionProk <- broadDataExtinction[which(!is.na(broadDataExtinction$mean_d18O.prok)),]
broadDataExtinctionProk <- broadDataExtinctionProk[which(!is.na(broadDataExtinctionProk$mean_d34S.prok)),]
broadDataExtinctionProk <- broadDataExtinctionProk[which(!is.na(broadDataExtinctionProk$mean_d13C.prok)),]

## @knitr bigBroadModelRMA

covModel.Broad.RMA <-rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinction, mods=~BC.extinction.ratePBDB +
                           OA + del.18O + del.34S +del.13C)

covModel.Broad.RMA


covModel.Broad.RMA2 <-rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinction, mods=~BC.extinction.ratePBDB +
                           OA + mean_d18O.prok + mean_d34S.prok + mean_d13C.prok)

covModel.Broad.RMA2



covModel.Broad.RMA3 <-rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinction, mods=~BC.extinction.ratePBDB +
                            OA + mean_d18O.prok + mean_d34S.prok + mean_d13C.prok+meanDate)

covModel.Broad.RMA3



covModel.Broad.RMA4 <-rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinction, mods=~BC.extinction.ratePBDB +
                            OA + mean_d18O.detrended.prok + mean_d34S.detrended.prok + mean_d13C.detrended.prok)

covModel.Broad.RMA4

## @knitr coefCompareBroad


broadCoefPlot1 <- coefPlot(covModel.Broad.RMA, robust=F, std=T)+
 # scale_x_discrete(labels=c("Extinction Rate", expression(delta^18*O), expression(delta^13*C), expression(delta^34*S), "Acidification"), expand = c(0.15, 0)) +
  #annotate("text", x=5, y=-0.4, label="A)")+
  ylim(c(-0.9,0.9)) +
  coord_flip() +
  annotate("text", x=5.6, y=-0.35, label="Favours\nnarrow")+
  annotate("text", x=5.6, y=0.35, label="Favours\nbroad")+
  ggtitle("Hannisdal & Peters")

broadCoefPlot2 <- coefPlot(covModel.Broad.RMA2, robust=F, std=T)+
  # scale_x_discrete(labels=c("Extinction Rate", expression(delta^18*O), expression(delta^13*C), expression(delta^34*S), "Acidification"), expand = c(0.15, 0)) +
  #annotate("text", x=5, y=-0.4, label="A)")+
  ylim(c(-0.9,0.9)) +
  coord_flip() +
  annotate("text", x=5.6, y=-0.35, label="Favours\nnarrow")+
  annotate("text", x=5.6, y=0.35, label="Favours\nbroad")+
  ggtitle("Prokoph")


broadCoefPlot3 <- coefPlot(covModel.Broad.RMA3, robust=F, std=T)+
  # scale_x_discrete(labels=c("Extinction Rate", expression(delta^18*O), expression(delta^13*C), expression(delta^34*S), "Acidification"), expand = c(0.15, 0)) +
  #annotate("text", x=5, y=-0.4, label="A)")+
  ylim(c(-0.9,0.9)) +
  coord_flip() +
  annotate("text", x=5.6, y=-0.35, label="Favours\nnarrow")+
  annotate("text", x=5.6, y=0.35, label="Favours\nbroad")+
  ggtitle("Prokoph with Time")


broadCoefPlot4 <- coefPlot(covModel.Broad.RMA4, robust=F, std=T)+
  # scale_x_discrete(labels=c("Extinction Rate", expression(delta^18*O), expression(delta^13*C), expression(delta^34*S), "Acidification"), expand = c(0.15, 0)) +
  #annotate("text", x=5, y=-0.4, label="A)")+
  ylim(c(-0.9,0.9)) +
  coord_flip() +
  annotate("text", x=5.6, y=-0.35, label="Favours\nnarrow")+
  annotate("text", x=5.6, y=0.35, label="Favours\nbroad")+
  ggtitle("Prokoph Detrended")


grid.arrange(broadCoefPlot1, broadCoefPlot2, broadCoefPlot3, broadCoefPlot4, ncol=2)

## @knitr sectionbreak
########################################
## Epifauna v. Infauna
########################################



## @knitr epibigEpifaunaModel.RMA
habitDataGood <- habitData[which(!(is.na(habitData$BC.extinction.ratePBDB))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$lnorReg))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$del.34S))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$del.18O))),]


covModel.Epifaunal.rma <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGood,
                             mods =~ OA + BC.extinction.ratePBDB + del.18O + del.34S)


covModel.Epifaunal.rma

covModel.Epifaunal.rma2 <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGood,
                             mods =~ OA + BC.extinction.ratePBDB + mean_d18O.prok + mean_d34S.prok)


covModel.Epifaunal.rma2

covModel.Epifaunal.rma3 <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGood,
                              mods =~ OA + BC.extinction.ratePBDB + mean_d18O.prok + mean_d34S.prok + meanDate)


covModel.Epifaunal.rma3


covModel.Epifaunal.rma4 <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGood,
                              mods =~ OA + BC.extinction.ratePBDB + mean_d18O.detrended.prok + mean_d34S.detrended.prok)


covModel.Epifaunal.rma4


## @knitr epibigEpifaunaModel.RMA.Plot

epiCoefPlot <- coefPlot(covModel.Epifaunal.rma, habitDataGood, robust=F, std=T)+
#  scale_x_discrete(labels=c("Extinction Rate",  expression(delta^18*O), expression(delta^34*S), "Acidification"), expand = c(0.15, 0)) +
#  annotate("text", x=4, y=-1.0, label="B)")+
  ylim(c(-1.5,1.5)) +
  coord_flip() +
  annotate("text", x=4.6, y=-.7, label="Favours\ninfauna")+
  annotate("text", x=4.6, y=.7, label="Favours\nepifauna")


epiCoefPlot <- coefPlot(covModel.Epifaunal.rma, habitDataGood, robust=F, std=T)+
  #  scale_x_discrete(labels=c("Extinction Rate",  expression(delta^18*O), expression(delta^34*S), "Acidification"), expand = c(0.15, 0)) +
  #  annotate("text", x=4, y=-1.0, label="B)")+
  ylim(c(-2.5,2.5)) +
  coord_flip() +
  annotate("text", x=4.6, y=-.7, label="Favours\ninfauna")+
  annotate("text", x=4.6, y=.7, label="Favours\nepifauna") +
  ggtitle("Hannisdal & Peters")

epiCoefPlot2 <- coefPlot(covModel.Epifaunal.rma2, habitDataGood, robust=F, std=T)+
  #  scale_x_discrete(labels=c("Extinction Rate",  expression(delta^18*O), expression(delta^34*S), "Acidification"), expand = c(0.15, 0)) +
  #  annotate("text", x=4, y=-1.0, label="B)")+
  ylim(c(-2.5,2.5)) +
  coord_flip() +
  annotate("text", x=4.6, y=-.7, label="Favours\ninfauna")+
  annotate("text", x=4.6, y=.7, label="Favours\nepifauna") +
  ggtitle("Prokoph")

epiCoefPlot3 <- coefPlot(covModel.Epifaunal.rma3, habitDataGood, robust=F, std=T)+
  #  scale_x_discrete(labels=c("Extinction Rate",  expression(delta^18*O), expression(delta^34*S), "Acidification"), expand = c(0.15, 0)) +
  #  annotate("text", x=4, y=-1.0, label="B)")+
  ylim(c(-2.5,2.5)) +
  coord_flip() +
  annotate("text", x=4.6, y=-.7, label="Favours\ninfauna")+
  annotate("text", x=4.6, y=.7, label="Favours\nepifauna") +
  ggtitle("Prokoph with Date")

epiCoefPlot4 <- coefPlot(covModel.Epifaunal.rma4, habitDataGood, robust=F, std=T)+
  #  scale_x_discrete(labels=c("Extinction Rate",  expression(delta^18*O), expression(delta^34*S), "Acidification"), expand = c(0.15, 0)) +
  #  annotate("text", x=4, y=-1.0, label="B)")+
  ylim(c(-2.5,2.5)) +
  coord_flip() +
  annotate("text", x=4.6, y=-.7, label="Favours\ninfauna")+
  annotate("text", x=4.6, y=.7, label="Favours\nepifauna") +
  ggtitle("Prokoph Detrended")


grid.arrange(epiCoefPlot, epiCoefPlot2, epiCoefPlot3, epiCoefPlot4, ncol=2)

#### #### #### #### #### ####
#### Appendix Figures
#### #### #### #### #### ####

# Run jackknifed model with scaled predictors
# TODO: Jarrett: is this the correct model?

scaledat <- function(x) {
  x.scaled <- x / (2 * sd(x, na.rm = TRUE))
  x.scaled
}

broadDataExtinctionScaled <- broadDataExtinction
broadDataExtinctionScaled <- transform(broadDataExtinction, 
  BC.extinction.ratePBDB = scaledat(BC.extinction.ratePBDB), 
  mean_d18O.prok = scaledat(mean_d18O.prok), 
  mean_d34S.prok = scaledat(mean_d34S.prok), 
  mean_d13C.prok = scaledat(mean_d13C.prok))
# sd of OA should already be ~0.5. (actually around 0.42)

covModel.Broad.RMA2.scaled <-rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinctionScaled, mods=~BC.extinction.ratePBDB +
                           OA + mean_d18O.prok + mean_d34S.prok + mean_d13C.prok)
# Now for the habit model:
habitDataGoodScaled <- habitDataGood
habitDataGoodScaled <- transform(habitDataGood, 
  BC.extinction.ratePBDB = scaledat(BC.extinction.ratePBDB), 
  mean_d18O.prok = scaledat(mean_d18O.prok), 
  mean_d34S.prok = scaledat(mean_d34S.prok), 
  meanDate = scaledat(meanDate))

covModel.Epifaunal.rma3.scaled <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGoodScaled,
                              mods =~ OA + BC.extinction.ratePBDB + mean_d18O.prok + mean_d34S.prok + meanDate)


## @knitr  jackknife.figs

pdf("figure/broad-jackknife.pdf", width = 4, height = 8)
jackknifed_coefs_fun(covModel.Broad.RMA2.scaled, broadDataExtinctionProk, robust=F) + theme_bw()+
  scale_colour_grey(name="Study Removed\n") + ylab("Scaled coefficient estimate")
dev.off()

# TODO WARNING
# Error in rma(lnorReg, vi = vlnorReg, data = temp_dat, mods = temp_dat[,  :
# Processing terminated since k = 0.
pdf("figure/habit-jackknife.pdf", width = 4, height = 8)
jackknifed_coefs_fun(covModel.Epifaunal.rma3.scaled, habitDataGood, robust=F) +theme_bw()+
  scale_colour_grey(name="Study Removed\n") + ylab("Scaled coefficient estimate")
dev.off()
