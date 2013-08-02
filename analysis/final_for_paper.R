##################################
##
## Final code to analyze log-odds ratios
## using only single response variables 
## from NESCent Working Group for publication
##
## Created:       Jan 13, 2012
## Last modified: Aug 02, 2013
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


########################################
## Dataset Properties
########################################

multiHabit <- sum(as.numeric(habitData$MultipleStages)-1)
singleHabit <- nrow(habitData) -multiHabit



multiBroad <- sum(as.numeric(broadData$MultipleStages)-1)
singleBroad <- nrow(broadData) -multiBroad

## @knitr singleMulti
paste("Multiple Stages: ", multiHabit+multiBroad, "\nSingle Stages: ", singleBroad + singleHabit, "\n",sep="")


## @knitr broadnarrowstart
########################################
## Broad v. Narrow
########################################

#####
### Examine the effect of taxonomic group and bivalve/gastropod
### for log odds ratio of broad v. narrow using lmer
#####
## @knitr BroadTaxaB
taxGenera.Broad<-rma(yi = lnorReg, vi = vlnorReg, mods= ~ Bivalve..Gastropod+Tax.level, data=broadData)
taxGenera.Broad


## @knitr BroadMeanRma
broad.rma <- rma(yi = lnorReg, vi = vlnorReg, data=broadData)
broad.rma 

## @knitr blank1
#####
### Examine the effect of regional v. global studies
### for log odds ratio of broad v. narrow using lmer
#####
## @knitr BroadGlobalRegional
scale.Broad <- rma(yi = lnorReg, vi =vlnorReg,   mods=~ Global.Regional, data=broadData)
scale.Broad

## @knitr blank
#####
### Examine the effect of multiple stages on lnor
### for log odds ratio of broad v. narrow using lmer
#####
## @knitr BroadMultStages
multStage.Broad<-rma(yi = lnorReg, vi =vlnorReg,   mods=~ MultipleStages, data=broadData)
multStage.Broad


## @knitr blank2
##########
##  Time
##########

## @knitr BroadTime
time.Broad<-rma(yi = lnorReg, vi =meanDate,   mods=~ meanDate, data=broadData)
time.Broad


## @knitr blank3
##########
## Grand Mean
##########
## @knitr BroadMeanRma
broad.rma <- rma(yi = lnorReg, vi = vlnorReg, data=broadData)
broad.rma 

#######
#Fig 1
#######

## @knitr blank4
##########
## Grand Model
##########
## @knitr bigBroadModelRMA
broadDataExtinction <- broadData[which(!is.na(broadData$BC.extinction.ratePBDB)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$del.18O)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$del.34S)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$del.13C)),]

covModel.Broad.RMA <-rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinction, mods=~OA+ BC.extinction.ratePBDB + 
                           del.18O + del.34S +del.13C)

covModel.Broad.RMA 

write.csv(coef(covModel.Broad.RMA), "./broadCoefTable.csv", row.names=T)

broadCoefPlot <- coefPlot(covModel.Broad.RMA, robust=F, std=T)+
  scale_x_discrete(labels=c("Extinction Rate", expression(delta^18*O), expression(delta^13*C), expression(delta^34*S), "Acidification")) +
  annotate("text", x=5, y=-0.4, label="A)")+
  ylim(c(-0.5,0.5)) +
  coord_flip() +
  annotate("text", x=5.5, y=-0.35, label="Favours narrow")+
  annotate("text", x=5.5, y=0.35, label="Favours broad")

## @knitr sectionbreak
########################################
## Epifauna v. Infauna
########################################



## @knitr BroadGlobalRegionalHabit
scale.habit <- rma(yi = lnorReg, vi =vlnorReg,   mods=~ Global.Regional, data=habitData)
scale.habit


## @knitr bivalvesGastroHabit
bivalve.gastro.Epifaunal<-rma(yi=lnorReg, vi=vlnorReg, data=habitData, mod=~Bivalve..Gastropod-1)
bivalve.gastro.Epifaunal

#####
### So, we pool.  Let's look at things, then
#####


## @knitr epiTime
# SA 20130119: I changed bivalvesEpifaunal to habitatData. I believe
# this was just a mistake when copying the code over
time.Epifaunal<-rma(yi=lnorReg, vi=vlnorReg, data= habitData, mods=~meanDate)
time.Epifaunal

## @knitr Epifaunal_rma
meanModel.Epifaunal<-rma(yi=lnorReg, vi=vlnorReg, data=habitData)
meanModel.Epifaunal



## @knitr epibigEpifaunaModel.RMA
habitDataGood <- habitData[which(!(is.na(habitData$BC.extinction.ratePBDB))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$lnorReg))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$del.34S))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$del.18O))),]

covModel.Epifaunal.rma <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGood,
                             mods =~ OA  + BC.extinction.ratePBDB + del.18O + del.34S)

covModel.Epifaunal.rma 
write.csv(coef(covModel.Epifaunal.rma), "./epiCoefTable.csv", row.names=T)


epiCoefPlot <- coefPlot(covModel.Epifaunal.rma, habitDataGood, robust=F, std=T)+
  scale_x_discrete(labels=c("Extinction Rate",  expression(delta^18*O), expression(delta^34*S), "Acidification")) +
  annotate("text", x=4, y=-1.5, label="B)")+
  ylim(c(-1.75,1.75)) +
  coord_flip() + 
  annotate("text", x=4.5, y=-.5, label="Favours\ninfauna")+
  annotate("text", x=4.5, y=.5, label="Favours\nepifauna")

## @knitr Fig4
grid.arrange(broadCoefPlot+theme_bw(base_size=18), epiCoefPlot+theme_bw(base_size=18), ncol=2)

## @knitr Fig5

####What are the marginal effects from the model
del18marg <- marginalLine(covModel.Epifaunal.rma, "del.18O", habitDataGood, robust=F)+ 
  xlab("\n Detrended Delta O18") + 
  ylab("Component + Residual + Intercept Log Odds\n Ratios for Detrended Delta O18\n") +
  annotate("text", x=-4, y=8.75, label="A)") + scale_color_discrete(guide="none") +
  theme_bw(base_size=18)

del18MargData<- marginalData(covModel.Epifaunal.rma, "del.18O", habitDataGood)
write.csv(del18MargData, "./del18MargData.csv", row.names=F)

del34marg <- marginalLine(covModel.Epifaunal.rma, "del.34S", habitDataGood, robust=F) + 
  xlab("\n Delta S34") + ylab("Component + Residual + Intercept Log Odds\n Ratios for Delta 34S\n") +
  annotate("text", x=13.75, y=3.375, label="B)")+
  theme_bw(base_size=18)

del34margData<- marginalData(covModel.Epifaunal.rma, "del.34S", habitDataGood)
write.csv(del34margData, "./del34margData.csv", row.names=F)

#Extract Legend 
g_legend<-function(a.gplot){ 
  a.gplot <- a.gplot+scale_color_discrete("Study")
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(del34marg) 


grid.arrange(del18marg, del34marg+ scale_color_discrete(guide="none"), legend,  
             widths=c(3,3,1), nrow=1)

## @knitr appendix
#### #### #### #### #### #### 
#### Appendix Figures
#### #### #### #### #### #### 

## @knitr  jackknife.figs
jackknifed_coefs_fun(covModel.Broad.RMA, broadDataExtinction, robust=F) + theme_bw()+
  scale_colour_grey(name="Study Removed\n")


jackknifed_coefs_fun(covModel.Epifaunal.rma, habitDataGood, robust=F) +theme_bw()+
  scale_colour_grey(name="Study Removed\n")

## @knitr funnelPlots
funnel(broad.rma, main="Funnel Plot for Broad v. Narrow Analysis")
funnel(meanModel.Epifaunal, main="Funnel Plot for Epifauna v. Infauna Analysis")
