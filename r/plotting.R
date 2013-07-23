###SOME BASICS ON CONTROLLING PLOTS

#I use ggplot2 - http://had.co.nz/ggplot2/ - to build these plots.  The axes and colors are pretty straightforward to manupulate, so have fun!

###########
#load in data, methods, etc.
###########
source("./metaprep.r")

rangeData<-subset(ext, ext$Aggregate.Trait=="Geographic Range")
rangeData<-subset(rangeData, rangeData$Trait.category != "other")
rangeData$Trait.category<-factor(rangeData$Trait.category)


#subset down to bivalves
bivalves<-subset(rangeData, rangeData[["Bivalve..Gastropod"]]=="Bivalve")


#Check the bivalve data
#datacheck(bivalves, "Geographic Range")
levelcheck(bivalves, "Geographic Range")

#plain vanilla no covariates
bivalveRange<-traitMod(bivalves, "Geographic Range", nafilter=T)

bivalveRange
plot(bivalveRange) #plots the diagnostics

#the real plot
p<-ggplotMetaforCoefs.traitMeta(bivalveRange)
p

#change the y axis
p+ylab("This is a new y axis")

#add a line break
p+ylab("This is a new y axis\n")

#give it a black and white theme
p+theme_pub(base_size=16)

#do both
p+theme_pub(base_size=16) + ylab("this is a new axis\n")

#enlarge the font
p+theme_pub(base_size=30) + ylab("this is a new axis\n")

#now add an x axis
p+theme_pub(base_size=30) + ylab("this is a new axis\n") + xlab("An X axis!")

###########
#with extinction rate - note, return=plot returns a ggplot2 object to work with
p2<-covariateAnalysis(bivalves, "BC.extinction.rate", return="plot")

p2

#change the y axis
p2+ylab("Extinction Selectivity\n")

#make the line red - method=lm is what the covariateAnalysis uses to create that line
p2+stat_smooth(method="lm", colour="red")

#in fact, here are the default plotting arguments for covariateAnalysis - you can change any of them by adding them to p2
 # retPlot<-ggplot(data=adf, aes(x=acovariate, y=lnor, ymin=lnor-1.96*sqrt(vlnor), ymax=lnor+1.96*sqrt(vlnor))) +
  # geom_pointrange(size=1.2) +
  # facet_wrap(~Trait.category) +
  # stat_smooth(method="lm") +
  # theme_pub(base_size=16) +
  # xlab(covariate)
  
#but note, you are only overlaying new layers here.

p2+theme_bw()

