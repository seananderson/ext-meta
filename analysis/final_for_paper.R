##################################
##
## Final code to analyze log-odds ratios
## using only single response variables
## from NESCent Working Group for publication
##
## Created:       Jan 13, 2012
## Last modified: Mar 11, 2015
## Purpose:       Try plotting the effect sizes against the raw data.
## Additional description: More analyses can be found in singleLnOr_R_analyses/singleLnOr_rma.R
## Changelog
##
## Oct 7, 2014 - Changed Knoll et al 1996 to 2007 as per Rowan's request
## Jul 24, 2014 Made fig 2 2 panels, changed labels on Fig 4
## Dec 28,2013 - Added xAdd argumen to marginal plotting to allow for adjusting for
##                offsets from centering predictors
##################################

#####
# Load libraries and data
#####
## @knitr prep
library(metafor)
library(ggplot2)
library(gridExtra)
library(gtable)

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

#centering Function
cent <- function (x) x-mean(x, na.rm=T)





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
broadDataExtinction <- broadData[which(!is.na(broadData$BC.extinction.rate.PBDB3)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$mean_d18O.prok)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$mean_d34S.prok)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$mean_d13C.prok)),]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$OA)),]

#as we'll be using these predictors later
broadDataExtinction <- within(broadDataExtinction, {
  cent.extinction <- cent(BC.extinction.rate.PBDB3)
  cent.OA <- cent(OA)
  cent.d18O <- cent(mean_d18O.prok)
  cent.d34S <- cent(mean_d34S.prok)
  cent.d13C <- cent(mean_d13C.prok)
  detrend.cent.d18O <- cent(mean_d18O.detrended.prok)
  detrend.cent.d34S <- cent(mean_d34S.detrended.prok)
  detrend.cent.d13C <- cent(mean_d13C.detrended.prok)
  cent.meanDate <- cent(meanDate)
})

levels(broadDataExtinction$study.ID) <- gsub(" [A-Z]{3}", "", levels(broadDataExtinction$study.ID))
levels(broadDataExtinction$study.ID) <- gsub("[A-Z]{3}", "", levels(broadDataExtinction$study.ID))


covModel.Broad.RMA <- rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinction, mods=~cent.extinction +
      cent.OA + cent.d18O + cent.d34S + cent.d13C)

covModel.Broad.RMA

############## Sean testing 2015-03-10
# m <- covModel.Broad.RMA
#
# # component line:
# x <- seq(
#   min(broadDataExtinction$cent.d34S),
#   max(broadDataExtinction$cent.d34S),
#   length.out = 10)
# y <- coef(m)[["intrcpt"]] + coef(m)[["cent.d34S"]] * x
#
# # residuals around component:
# xres <- broadDataExtinction$cent.d34S
# yres <- coef(m)[["intrcpt"]] + coef(m)[["cent.d34S"]] * xres + residuals(m)
#
# plot(xres + mean(broadDataExtinction$mean_d34S.prok), yres,
#   ylab = "Intercept + component + residual", xlab = "d34S")
# lines(x + mean(broadDataExtinction$mean_d34S.prok), y)
# abline(h = 0, lty = 3)

# m <- covModel.Epifaunal.rma
# x <- seq(min(habitDataGood$cent.d18O), max(habitDataGood$cent.d18O), length.out = 10)
# y <- coef(m)[["intrcpt"]] + coef(m)[["cent.d18O"]] * x
# plot(x + mean(habitDataGood$mean_d18O.prok), y)
# abline(h = 0, lty = 3)

##############

#Also, the model with predictors detrended
covModel.Broad.RMA.detrended <- rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinction, mods=~cent.extinction +
                            cent.OA + detrend.cent.d18O + detrend.cent.d34S + detrend.cent.d13C)

covModel.Broad.RMA.detrended

write.csv(coef(covModel.Broad.RMA), "./broadCoefTable.csv", row.names=T)
write.csv(coef(covModel.Broad.RMA.detrended), "./broadCoefDetrendedTable.csv", row.names=T)

broadCoefPlot <- coefPlot(covModel.Broad.RMA, robust=F, std=T, num_sds=2)+
  coord_flip() +
  scale_x_discrete(labels=c(expression(delta^13*C), expression(delta^18*O), expression(delta^34*S), "Extinction Rate", "Acidification"), expand = c(0.15, 0)) +
  annotate("text", x=5, y=-0.2, label="(a)")+
  ylim(c(-0.25,0.25)) +
  annotate("text", x=5.6, y = -0.15, label="Favours\nnarrow")+
  annotate("text", x=5.6, y = 0.15, label="Favours\nbroad")

## @knitr broadModelWithTime
rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinction, mods=~BC.extinction.ratePBDB +
      cent.OA + cent.d18O + cent.d34S + cent.d13C+cent.meanDate)

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
habitDataGood <- habitData[which(!(is.na(habitData$BC.extinction.rate.PBDB3))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$lnorReg))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$mean_d18O.prok))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$mean_d34S.prok))),]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$vlnorReg))),]

#get rid of trailing letters
levels(habitDataGood$study.ID) <- gsub(" [A-Z]{3}", "", levels(habitDataGood$study.ID))
levels(habitDataGood$study.ID) <- gsub("[A-Z]{3}", "", levels(habitDataGood$study.ID))

#as we'll be using these predictors later
habitDataGood <- within(habitDataGood, {
  cent.extinction <- cent(BC.extinction.rate.PBDB3)
  cent.OA <- cent(OA)
  cent.d18O <- cent(mean_d18O.prok)
  cent.d34S <- cent(mean_d34S.prok)
  cent.meanDate <- cent(meanDate)
  detrend.cent.d18O <- cent(mean_d18O.detrended.prok)
  detrend.cent.d34S <- cent(mean_d34S.detrended.prok)
  detrend.cent.d13C <- cent(mean_d13C.detrended.prok)
})

covModel.Epifaunal.rma <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGood,
                              mods =~ cent.OA+ cent.extinction + cent.d18O + cent.d34S)

covModel.Epifaunal.rma

#And now detrended
covModel.Epifaunal.rma.detrend <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGood,
                             mods =~ cent.OA+ cent.extinction + detrend.cent.d18O + detrend.cent.d34S)

covModel.Epifaunal.rma.detrend

write.csv(coef(covModel.Epifaunal.rma.detrend), "./epiCoefDetrendedTable.csv", row.names=T)
write.csv(coef(covModel.Epifaunal.rma), "./epiCoefTable.csv", row.names=T)


epiCoefPlot <- coefPlot(covModel.Epifaunal.rma, habitDataGood, robust=F, std=T, num_sds=2)+
  coord_flip() +
  scale_x_discrete(labels=c(expression(delta^18*O), expression(delta^34*S), "Extinction Rate", "Acidification"), expand = c(0.15, 0)) +
  annotate("text", x=4, y=-0.4, label="(b)")+
  ylim(c(-0.5,0.5)) +
  annotate("text", x=4.6, y=-.25, label="Favours\ninfauna")+
  annotate("text", x=4.6, y=.25, label="Favours\nepifauna")



## @knitr epibigEpifaunaModel.RMA.checktime
rma(yi = lnorReg, vi = vlnorReg, data=habitDataGood,
    mods =~ cent.OA + cent.extinction + cent.d18O + cent.d34S + cent.meanDate)


## @knitr Fig4
grid.arrange(broadCoefPlot+theme_bw(base_size=18), epiCoefPlot+theme_bw(base_size=18), ncol=2)

## @knitr Fig5

####What are the marginal effects from the model
####After adjusting for centering the predictor
del18marg <- marginalLine(covModel.Epifaunal.rma, "cent.d18O",
                          habitDataGood, robust=F, xAdd=mean(habitDataGood$mean_d18O.prok))+
  xlab("\n Delta O18") +
  ylab("Component + Residual + Intercept Log Odds\n Ratios for Delta O18\n") +
  annotate("text", x=-4, y=2.75, label="(a)") +
  theme_bw(base_size=18)+
  annotate("text", x=-1, y=-2, label="Favours\ninfauna")+
  annotate("text", x=-3.5, y=2, label="Favours\nepifauna")

del18MargData <- marginalData(covModel.Epifaunal.rma, "cent.d18O", habitDataGood)
write.csv(del18MargData, "./del18MargData.csv", row.names=F)


#The Figure
colormatch <- sapply(levels(habitDataGood$study.ID), function(alev){
  as.character(habitDataGood$color.ID[which(as.character(habitDataGood$study.ID)==alev)][1])
})

fig5a <- del18marg +
  scale_color_manual(guide="none", values=colormatch)

#make an extracted legend
leg1 <- del18marg + guides(linetype=FALSE, size=FALSE)
legend.colour <- gtable_filter(ggplot_gtable(ggplot_build(leg1)), "guide-box")


#The Detrended Figure
del18margDetrend <- marginalLine(covModel.Epifaunal.rma.detrend, "detrend.cent.d18O",
                          habitDataGood, robust=F, xAdd=mean(habitDataGood$mean_d18O.detrended.prok))+
  xlab("\n Detrended Delta O18") +
  ylab("Component + Residual + Intercept Log Odds\n Ratios for Detrended Delta O18\n") +
  annotate("text", x=-2.5, y=3, label="(b)") +
  theme_bw(base_size=18)+
  annotate("text", x=2, y=-1.5, label="Favours\ninfauna")+
  annotate("text", x=-1.5, y=2, label="Favours\nepifauna")

fig5b <- del18margDetrend +
  scale_color_manual(guide="none", values=colormatch)


grid.arrange(fig5a, fig5b, legend.colour, widths=c(3,3,1), ncol=3)

## @knitr appendix
#### #### #### #### #### ####
#### Appendix Figures
#### #### #### #### #### ####

## @knitr  jackknife.figs

scale_ <- function(x) x / (2 * sd(x, na.rm = TRUE))
# make a scaled model that we'll use to plot the jackknife plot:
broadDataExtinction <- transform(broadDataExtinction,
  cent.extinction.scaled = scale_(cent.extinction),
  cent.d18O.scaled = scale_(cent.d18O),
  cent.d34S.scaled = scale_(cent.d34S),
  cent.d13C.scaled = scale_(cent.d13C))
covModel.Broad.RMA.scaled <- rma(yi = lnorReg, vi = vlnorReg,
  data=broadDataExtinction, mods=~cent.extinction.scaled +
      cent.OA + cent.d18O.scaled + cent.d34S.scaled + cent.d13C.scaled)
p <- jackknifed_coefs_fun(covModel.Broad.RMA.scaled, broadDataExtinction, robust=FALSE) + theme_bw()+
  scale_colour_grey(name="Study Removed\n") + ylab("Scaled coefficient estimate")
ggsave("figure/jackknife-epi-broad.pdf", width = 6, height = 6)

habitDataGood <- transform(habitDataGood,
  cent.extinction.scaled = scale_(cent.extinction),
  cent.d18O.scaled = scale_(cent.d18O),
  cent.d34S.scaled = scale_(cent.d34S))
covModel.Epifaunal.rma.scaled <- rma(yi = lnorReg, vi = vlnorReg,
  data=habitDataGood, mods=~cent.extinction.scaled +
      cent.OA + cent.d18O.scaled + cent.d34S.scaled)
# Life Habit Model

p <- jackknifed_coefs_fun(covModel.Epifaunal.rma.scaled, habitDataGood, robust=FALSE) +theme_bw()+
  scale_colour_grey(name="Study Removed\n") + ylab("Scaled coefficient estimate")
ggsave("figure/jackknife-epi-scaled.pdf", width = 6, height = 6)

# Life Habit Model Detrended
#jackknifed_coefs_fun(covModel.Epifaunal.rma.detrend, habitDataGood, robust=F) +theme_bw()+
  #scale_colour_grey(name="Study Removed\n")

## @knitr funnelPlots
funnel(broad.rma, main="Funnel Plot for Geographic Range (Broad to Narrow) Analysis")
funnel(meanModel.Epifaunal, main="Funnel Plot for Life Habit (Epifauna v. Infauna) Analysis")

## @knitr scaledfunnelPlots
scaledat <- function(x) {
  x.scaled <- x / (2 * sd(x, na.rm = TRUE))
  x.scaled
}


broadDataExtinctionScaled <- broadDataExtinction
broadDataExtinctionScaled <- transform(broadDataExtinction,
                                       BC.extinction.rate.PBDB3 = scaledat(BC.extinction.rate.PBDB3),
  mean_d18O.prok = scaledat(mean_d18O.prok),
  mean_d34S.prok = scaledat(mean_d34S.prok),
  mean_d13C.prok = scaledat(mean_d13C.prok))
# sd of OA should already be ~0.5. (actually around 0.42)

covModel.Broad.RMA2.scaled <-rma(yi = lnorReg, vi = vlnorReg, data=broadDataExtinctionScaled, mods=~BC.extinction.rate.PBDB3 +
                           OA + mean_d18O.prok + mean_d34S.prok + mean_d13C.prok)
# Now for the habit model:
habitDataGoodScaled <- habitDataGood
habitDataGoodScaled <- transform(habitDataGood,
   BC.extinction.rate.PBDB3 = scaledat(BC.extinction.rate.PBDB3),
  mean_d18O.prok = scaledat(mean_d18O.prok),
  mean_d34S.prok = scaledat(mean_d34S.prok),
  meanDate = scaledat(meanDate))

covModel.Epifaunal.rma3.scaled <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGoodScaled,
                                     mods =~ OA + BC.extinction.rate.PBDB3 + mean_d18O.prok + mean_d34S.prok)


covModel.Epifaunal.rma.time.covarite.scaled <-rma(yi = lnorReg, vi = vlnorReg, data=habitDataGoodScaled,
                              mods =~ OA + BC.extinction.rate.PBDB3 + mean_d18O.prok + mean_d34S.prok + meanDate)

pdf("figure/broad-jackknife.pdf", width = 4, height = 8)
jackknifed_coefs_fun(covModel.Broad.RMA2.scaled, broadDataExtinctionScaled, robust=F) + theme_bw()+
  scale_colour_grey(name="Study Removed\n") + ylab("Scaled coefficient estimate")
dev.off()

# TODO WARNING
# Error in rma(lnorReg, vi = vlnorReg, data = temp_dat, mods = temp_dat[,  :
# Processing terminated since k = 0.
pdf("figure/habit-jackknife-detrended.pdf", width = 4, height = 8)
jackknifed_coefs_fun(covModel.Epifaunal.rma3.scaled, habitDataGood, robust=F) +theme_bw()+
  scale_colour_grey(name="Study Removed\n") + ylab("Scaled coefficient estimate")
dev.off()

pdf("figure/habit-jackknife-time-covariate.pdf", width = 4, height = 8)
jackknifed_coefs_fun(covModel.Epifaunal.rma.time.covarite.scaled, habitDataGood, robust=F) +theme_bw()+
  scale_colour_grey(name="Study Removed\n") + ylab("Scaled coefficient estimate")
dev.off()


### Make component + residual plots for all predictors for SOM:

pretty_names <- data.frame(
  pretty = c("Extinction rate", "Acidification", "d18O", "d34S", "d13C"),
  ugly = c("cent.extinction", "cent.OA", "cent.d18O", "cent.d34S", "cent.d13C"),
  unscaled_names = c("BC.extinction.rate.PBDB3", "OA", "mean_d18O.prok", "mean_d34S.prok", "mean_d13C.prok"))

cr1 <- lapply(1:5, function(i) {
  this_coef <- as.character(pretty_names$ugly[i])
  marginalLine(
    covModel.Broad.RMA, this_coef,
    broadDataExtinction, robust=FALSE,
    xAdd=mean(broadDataExtinction[,names(broadDataExtinction) == pretty_names$unscaled_names[i]], na.rm = TRUE)) +
  theme_bw(base_size = 13) +
  xlab(pretty_names$pretty[i]) +
  ylab(paste("Intercept + Component + \nResidual for", pretty_names$pretty[i]))
  })
pdf("figure/comp-res-broad.pdf", width = 12, height = 12)
gridExtra::grid.arrange(cr1[[1]], cr1[[2]], cr1[[3]], cr1[[4]], cr1[[5]])
dev.off()


cr1 <- lapply(1:4, function(i) {
  this_coef <- as.character(pretty_names$ugly[i])
  marginalLine(
    covModel.Epifaunal.rma, this_coef,
    habitDataGood, robust=FALSE,
    xAdd=mean(habitDataGood[,names(habitDataGood) == pretty_names$unscaled_names[i]], na.rm = TRUE)) +
  theme_bw(base_size = 13) +
  xlab(pretty_names$pretty[i]) +
  ylab(paste("Intercept + Component + \nResidual for", pretty_names$pretty[i]))
  })
pdf("figure/comp-res-epi.pdf", width = 12, height = 8)
gridExtra::grid.arrange(cr1[[1]], cr1[[2]], cr1[[3]], cr1[[4]])
dev.off()
