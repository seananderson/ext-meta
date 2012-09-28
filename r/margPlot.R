######
## plots a variable after
## detrending from a lmer object
marginalPlot<-function(object, variable, dataFrame){

  Marg<-cbind(dataFrame, getMarginal(object, dataFrame))
  varName<-paste(variable, "Marginal", sep="")
  Marg$cilnorhi = Marg[[varName]] + 1.96*Marg$sdlnorReg
  Marg$cilnorlo = Marg[[varName]] - 1.96*Marg$sdlnorReg
  
  marginal.data.plot<-ggplot(data=Marg, aes_string(y=varName, x=variable,
                                               ymin="cilnorlo", 
                                               ymax="cilnorhi")) + 
                                                 geom_point() +
                                                 geom_linerange()+
                                                 theme_bw(base_size=18) + 
                                                 xlab(variable)  + 
                                                 ylab(paste("marginal log odds ratio\n for ", variable, sep=""))+
                                                 geom_hline(aes(yintercept=0), lty=2, lwd=1.5)

  marginal.data.plot

}

#dataFrame <-broadMarg

#variable <- "OA"

#object <-covModel.Broad


#marginalPlot(covModel.Broad, "OA", broadData)
#marginalPlot(covModel.Broad, "del.34S", broadData)
#marginalPlot(covModel.Broad, "d18OresidualMean", broadData)
#marginalPlot(covModel.Broad, "BC.extinction.rate.PBDB", broadData)

names(cars)
afun<-function(z){
  #ggplot(data=cars, aes(x=dist, y=z))+ geom_point()
  ggplot(data=cars, aes_string(x="dist", y=z))+ geom_point()
  #ggplot(data=cars, aes(x=dist, y=get(z)))+ geom_point()
}

afun("speed")