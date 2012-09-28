library(lme4)
library(AICcmodavg)

ext$meanDateHMYA<-ext$meanDate/100
ext$sdlnorReg<-1.96*sqrt(ext$vlnorReg)

#mimic se.ranef from arm, so I don't have to load arm
se.fixef<-function(obj) attr(summary(obj), "coefs")[,2]


#get standardized coefficients from an lmer
stdCoef.lmer<-function(object) {
  sdy<-sd(attr(object, "y"))
  sdx<-apply(attr(object, "X"), 2, sd)
  sc<-fixef(object)*sdx/sdy
  se<-se.fixef(object)*sdx/sdy
  return(list(stdcoef=sc, stdse=se))
}

#get the marginal values including intercepts
#from an lmer object holding all but one predictor constant

getMarginal<-function(object, dataFrame, intercept=T, ranintercept=T){
  goodIDX<-which(dataFrame$lnorReg %in% attr(object, "frame")$lnorReg)
  
  #get the data we'll be using for calculations
  objData<-attr(object, "frame")
  objX<-attr(object, "X")[,-1]
  
  #multiply each column by the appropriate fixed effect
  predSingle<-data.frame(t(t(objX) * fixef(object)[-1]))
  names(predSingle)<-names(fixef(object))[-1]
  
  
  #get the intercepts due to random effects
  ranIDX<-match(names(ranef(object)), names(objData))
  ranefVec<-ranef(object)[[1]][match(as.character(objData[,ranIDX]), rownames(ranef(object)[[1]])),]
  
  #get the residuals + the grand intercept + random residual
  residInt<-residuals(object) 
  if(intercept) residInt<- residInt + fixef(object)[1]
  if(ranintercept) residInt<-residInt+ranefVec
  
  #get the marginal values
  marg<-residInt+predSingle
  
  #slot NAs back into the marginal values so that this can be cbound onto a data frame
  ret<-data.frame(matrix(rep(NA, nrow(dataFrame) * ncol(marg)), nrow=nrow(dataFrame)))
  
  ret[goodIDX,]<-marg[,]
  names(ret)<-paste(names(marg), "Marginal", sep="")
  
  ret
}

#a way to plot coefficients from lmer objects with ggplot2
coefPlot<-function(obj, std=F){
  object<-as.data.frame(attr(summary(obj), "coefs"))
  coefString<-"Model Coefficient"
  
  object$coefs<-rownames(object)
  names(object) <- gsub(" ", ".", names(object))
  
  if(std){
    scoefs<-stdCoef.lmer(obj)
    object[,1]<-scoefs$stdcoef
    object[,2]<-scoefs$stdse
    coefString<-"Standardized\nModel Coefficient"
    object<-object[-1,]
  }  
  
  ggplot(data=object, 
         aes(x= coefs, y= Estimate, ymin=Estimate-Std..Error*1.96, ymax=Estimate+Std..Error*1.96)) +
           geom_point(size=3) + 
           geom_linerange(size=1.5) +
           theme_bw() +
           xlab("") +
           ylab(coefString) +
           geom_hline(aes(yintercept=0), lty=2, lwd=1.5)
}

#predict from lmer from Ben Bolker's Wiki
#not working yet...
predict.lmer<-function(object, newdat){
  #sort the columns properly for making a model matrix
  newdat<-newdat[,na.omit(match(names(attributes(terms(object))$dataClasses), names(newdat)))]
  newdat<-cbind(data.frame("(Intercept)" = rep(1, times=nrow(newdat))), newdat)
  names(newdat)[1]<-"(Intercept)"
  #newdat[[names(attributes(terms(paredModel.Epifaunal))$dataClasses[1])]]<-rep(NA, nrow(newdat))
  
  #  mm <- model.matrix(formula(object),newdat)
  fit <- as.matrix(newdat) %*% fixef(object)
  pvar1 <- diag(as.matrix(newdat) %*% tcrossprod(vcov(object),newdat))
  tvar1 <- pvar1+VarCorr(object)$study.ID[1]  ## must be adapted for more complex models
  newdat <- data.frame(
    fit = fit
    , cilo.f = fit-2*sqrt(pvar1)
    , cihi.f = fit+2*sqrt(pvar1)
    , cilo.fr = fit-2*sqrt(tvar1)
    , cihi.fr = fit+2*sqrt(tvar1)
  )
  
  newdat
}
#debug(predict.lmer)
#predict.lmer(covModel.Broad, blankMat)
#newdat<-blankMat

source("../r/margLine.R")
source("../r/margPlot.R")