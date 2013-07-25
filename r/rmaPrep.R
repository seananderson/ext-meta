##########################################
#
# Methods for visualizing results from rma fits
# from the metafor library
#
# Last Updated Oct 31, 2012
##########################################
library(reshape)

coefPlot <- function(amod, adf, std=F, robust=T) {
  ctab <- coef(summary(amod))
  if(robust) ctab <- robustSE(amod, adf$study.ID)
  mult <-  1
  lab <- "\nCoefficient\n"
  if(std) {
    ctab <- ctab[-1,]
    mult <- colwise(sd)(as.data.frame(amod$X[,-1])) / sd(amod$yi)
    lab <- "\nStandardized Coefficient\n"
  }
  
  ctab <- ctab*t(mult)
  ctab$coef <- rownames(ctab)
  
  #return a graph
  ggplot(data=ctab, mapping=aes(x=coef, y=estimate, ymin=ci.lb, ymax=ci.ub)) +
    geom_pointrange(size=1.5) +
    geom_hline(y=0, lty=2, lwd = 1.3) +
    theme_bw() +
    ylab(lab) + xlab("")
  
  
}


#get a robust variance-covariance matrix
robustVcov <- function(rma.obj, cluster=NULL, CI=.95) {
  # m: no. of clusters; assumed independent if not specified
  # rma.obj$not.na: complete cases
  if (is.null(cluster)) {
    m=length(rma.obj$X[rma.obj$not.na,1])
  } else {
    m=nlevels(unique(as.factor(cluster[rma.obj$not.na])))
  }
  res2 <- diag(residuals(rma.obj)^2)
  X <- rma.obj$X
  b <- rma.obj$b
  W <- diag(1/(rma.obj$vi+rma.obj$tau2))     # Use vi+tau2
  meat <- t(X) %*% W %*% res2 %*% W %*% X    # W is symmetric
  bread <- solve( t(X) %*% W %*% X)
  V.R <- bread %*% meat %*% bread            # Robust sampling covariance matrix
  
  V.R
}



#removes effect of variables being controlled for
margAdjust <- function(coefs, X, y, variable){
  #what are we NOT going marginal to
  idx <- c(1, which(colnames(X)==variable))
  if(length(idx)==1) stop("Bad Variable Name!")
  X <- X[,-idx]
  coefs <- coefs[-idx]
  
   X %*% coefs  
}


marginalPlot <- function(obj, variable, dataFrame){
  #for rma
  coefs <- coef(summary(obj))[,1]
  X <- obj$X
  predictor <- X[,which(rownames(coef(summary(obj)))==variable)]
  y <- as.numeric(obj$yi)

  margPoints <- y - margAdjust(coefs, X, y, variable)
  
  subdf <- data.frame(predictor = predictor, margPoints = margPoints, study.ID = dataFrame$study.ID)
  ggplot() + geom_point(data = subdf, mapping=aes(x=predictor, y=margPoints, color=study.ID)) +
    xlab(variable) + ylab(paste("Marginal Values for ", variable, sep="")) +
    theme_bw()
}

marginalLine <- function(obj, variable, dataFrame, interval = "fit", robust=T){
  #for rma
  coefs <- coef(summary(obj))[,1]
  se.coefs <- coef(summary(obj))[,2]
  varCoefs <- vcov(obj)
  idx <- which(rownames(coef(summary(obj)))==variable)
  
  #some important values
  X <- obj$X
  predictor <- X[,idx]
  y <- as.numeric(obj$yi)
  
  marPlot <- marginalPlot(obj, variable, dataFrame)
  #pred <- data.frame(predict(obj)[1:6])
  #pred$X <- predictor
  
  #or, a smooth gradient - except this gives me VERY choppy intervals
  newPred <- seq(min(predictor), max(predictor), .01)
  predMat <- matrix(rep(colMeans(X), length(newPred)), ncol=ncol(X), byrow=T)
  predMat[,idx] <- newPred
  colnames(predMat) <- colnames(X)
  
  pred <- data.frame( yHat = predMat %*% coefs)
  pred <- within(pred, {
         yvar <- diag(as.matrix(predMat) %*% tcrossprod(varCoefs, predMat))
         if(robust) yvar <- diag(as.matrix(predMat) %*% tcrossprod(robustVcov(obj, dataFrame$study.ID), predMat))
          ci.lb <- yHat - 1.96* sqrt(yvar)
          ci.ub <- yHat + 1.96* sqrt(yvar)
  })
  
  pred$X <- newPred
  
  
  pred[,1:3] <-  pred[,1:3] - margAdjust(coefs, predMat, pred[,1], variable)
  
  if(interval=="fit")  marPlot <- marPlot + geom_ribbon(data=pred, mapping=aes(x=X, ymin=ci.lb, ymax=ci.ub), fill="lightgrey", alpha=0.5)
 # if(interval=="prediction")  marPlot <- marPlot + geom_ribbon(data=pred, mapping=aes(x=X, ymin=cr.lb, ymax=cr.ub), fill="lightgrey", alpha=0.5)

  marPlot <- marPlot +
    geom_abline(intercept=coefs[1], slope=coefs[idx], size=1.5, col="red") +
    geom_hline(y=0, lty=2, lwd=1.5)
  
  marPlot
  
  
}



marginalData <- function(obj, variable, dataFrame){
  #for rma
  coefs <- coef(summary(obj))[,1]
  X <- obj$X
  predictor <- X[,which(rownames(coef(summary(obj)))==variable)]
  y <- as.numeric(obj$yi)
  
  margPoints <- y - margAdjust(coefs, X, y, variable)
  
  subdf <- data.frame(predictor = predictor, margPoints = margPoints, 
                      study.ID = dataFrame$study.ID, 
                      Bivalve..Gastropod = dataFrame$Bivalve..Gastropod,
                      Start.stage = dataFrame$Start.stage,
                      End.stage = dataFrame$End.stage,
                      Taxon = dataFrame$Taxon,
                      Tax.level=dataFrame$Tax.level)
  return(subdf)
}


jackknifed_coefs_fun  <- function(obj, adf, robust=T){
  ndf <- data.frame(lnorReg = obj$yi, vlnorReg = obj$vi, study.ID = adf$study.ID)
  ndf <- cbind(ndf, obj$X[,-1])
  
  #from Sean
  jackknifed_coefs <- ldply(unique(ndf$study.ID), function(x) { 
    temp_dat <- subset(ndf, !study.ID %in% x)
    broad_rma_jackknife <- rma(lnorReg, vi = vlnorReg, data = temp_dat, mods=temp_dat[,-(1:3)], method = "REML")
    coefs <- coef(summary(broad_rma_jackknife))
    if(robust) coefs <- robustSE(broad_rma_jackknife, cluster = temp_dat$study.ID)
    
    coef_df <- as.data.frame(t(coefs$estimate))
    ci.lb_df <- as.data.frame(t(coefs$ci.lb))
    ci.ub_df <- as.data.frame(t(coefs$ci.ub))
    names(coef_df) <- rownames(coefs)
    names(ci.lb_df) <- rownames(coefs)
    names(ci.ub_df) <- rownames(coefs)
    ci.lb_df <- melt(ci.lb_df); names(ci.lb_df)[2] <- "ci.lb"
    ci.ub_df <- melt(ci.ub_df); names(ci.ub_df)[2] <- "ci.ub"
    coef_df <- melt(coef_df); names(coef_df)[2] <- "coef"
    coef_df_out <- data.frame(coef_df, ci.lb = ci.lb_df[,2], ci.ub = ci.ub_df[,2])
    data.frame(study.ID.jackknifed = x, coef_df_out)
  })
  p <- ggplot(jackknifed_coefs) + 
          geom_pointrange(aes(x = variable, y = coef, ymin = ci.lb, ymax = ci.ub, 
                        colour = study.ID.jackknifed), position = position_dodge(height = 0, width = -0.6)) + 
          coord_flip() + xlab("") + ylab("Coefficient estimate") + 
          geom_hline(yintercept = 0, lty = 1, col = "#00000080")  
  
  p
}


