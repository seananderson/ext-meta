
marginalLine<-function(object, variable, dataFrame, nsamp=50, xAdd=0){
  margPlot<-marginalPlot(object, variable, dataFrame, xAdd=xAdd)
  
  blankMat<-data.frame(matrix(rep(0, nsamp * length(fixef(object)[-1])), nrow=nsamp))

  blankMat[,which(names(fixef(object))[-1] == variable)]<-
    seq(min(dataFrame[[variable]], na.rm=T)-1, max(dataFrame[[variable]], na.rm=T)+1, length.out=nsamp)
  
  names(blankMat)<-names(fixef(object))[-1]
  
  predictFit<-predictSE.mer(object, blankMat, level=0, se.fit=TRUE)
    
  predictFitDF<-predict.lmer(object, blankMat)
  predictFitDF[[paste(variable, "Marginal", sep="")]]<-predictFitDF$fit
  predictFitDF$sdlnorReg<-rep(NA, nrow(predictFitDF))
  predictFitDF<-cbind(predictFitDF, blankMat)
  varName<-paste(variable, "Marginal", sep="")
  
  predictFitDF$cilnorlo<-predictFitDF$cilo.fr
  predictFitDF$cilnorhi<-predictFitDF$cihi.fr
  
  predictFitDF$variable <- predictFitDF$variable+xAdd
  
  margPlot+geom_ribbon(data=predictFitDF, aes_string(x=variable, 
                                                       y="fit",
                                                       ymin="cilo.fr", 
                                                       ymax="cihi.fr"),
                                fill="red", alpha="0.4", lwd=1) +
                                  geom_line(data=predictFitDF, aes_string(x=variable, y=varName), color="red", lwd=1.5)
  
}



#marginalLine(covModel.Broad, "OA", broadData)
#marginalLine(covModel.Broad, "del.34S", broadData)
#marginalLine(covModel.Broad, "d18OresidualMean", broadData)
#marginalLine(covModel.Broad, "BC.extinction.rate.PBDB", broadData)+xlim(c(-0.001,0.7))


#goodIDX<-which(dataFrame$lnorReg %in% attr(object, "frame")$lnorReg)