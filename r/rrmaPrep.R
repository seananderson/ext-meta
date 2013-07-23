##########################################
#
# Methods for visualizing results from rma fits
# from the metafor library
#
# Last Updated Nov 7, 2012
##########################################
library(reshape)
source("../hedges_etal_2010/rrma_hedges.R")
source("../hedges_etal_2010/compare.R")

coefPlot.rrma <- function(amod, adf, std=F) {
  ctab <- amod$est
  mult <-  1
  lab <- "Coefficient\n"
  if(std) {
    ctab <- ctab[-1,]
    mult <- colwise(sd)(as.data.frame(amod$X[,-c(1:2)])) / sd(amod$yi)
    lab <- "Standardized Coefficient\n"
  }
  
  ctab[,-1] <- ctab[,-1]*t(mult)
  
  #return a graph
  ggplot(data=ctab, mapping=aes(x=beta, y=estimate, ymin=ci.lb, ymax=ci.ub)) +
    geom_pointrange(size=1.5) +
    geom_hline(y=0, lty=2, lwd = 1.3) +
    theme_bw() +
    ylab(lab) + xlab("")
  
  
}



#removes effect of variables being controlled for
margAdjust <- function(coefs, X, y, variable){
  #what are we NOT going marginal to
  idx <- c(1, which(colnames(X)==variable))
  if(length(idx)==1) stop("Bad Variable Name!")
  X <- X[,-idx]
  coefs <- coefs[-idx]
  
  X %*% t(coefs)  
}


marginalPlot.rrma <- function(obj, variable){
  #for rma
  coefs <- coef(obj)
  X <- obj$X[,-1]
  predictor <- X[,which(names(coef(obj))==variable)]
  y <- as.numeric(obj$yi)
  
  margPoints <- as.numeric(y - margAdjust(coefs, X, y, variable))
  
  subdf <- data.frame(predictor = predictor, margPoints = margPoints, study.ID = obj$mf$study_id)
  
  ggplot() + geom_point(data = subdf, mapping=aes(x=predictor, y=margPoints, color=study.ID)) +
    xlab(variable) + ylab(paste("Residual Values for Controlling for All But", variable, sep="")) +
    theme_bw()
}

marginalLine.rrma <- function(obj, variable, interval = "fit"){
  #for rma
  coefs <- coef(obj)
  se.coefs <- obj$est[,3]
  varCoefs <- vcov(obj)
  idx <- which(names(coef(obj))==variable)
  
  #some important values
  X <- obj$X[,-1]
  predictor <- X[,which(names(coef(obj))==variable)]
  y <- as.numeric(obj$yi)
  
  
  marPlot <- marginalPlot.rrma(obj, variable)
  #pred <- data.frame(predict(obj)[1:6])
  #pred$X <- predictor
  
  #or, a smooth gradient - except this gives me VERY choppy intervals
  newPred <- seq(min(predictor), max(predictor), .01)
  predMat <- matrix(rep(colMeans(X), length(newPred)), ncol=ncol(X), byrow=T)
  predMat[,idx] <- newPred
  colnames(predMat) <- colnames(X)
  
  pred <- data.frame( yHat = predMat %*% coefs)
  pred <- within(pred, {
    yvar <- diag(as.matrix(predMat) %*% tcrossprod(vcov(obj), predMat))
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



jackknifed_coefs_fun  <- function(obj, adf){
  ndf <- data.frame(lnorReg = obj$yi, vlnorReg = obj$vi, study.ID = adf$study.ID)
  ndf <- cbind(ndf, obj$X[,-1])
  
  #from Sean
  jackknifed_coefs <- ldply(unique(ndf$study.ID), function(x) { 
    temp_dat <- subset(ndf, !study.ID %in% x)
    broad_rma_jackknife <- rma(lnorReg, vi = vlnorReg, data = temp_dat, mods=temp_dat[,-(1:3)], method = "REML")
    coefs <- robustSE(broad_rma_jackknife, cluster = temp_dat$study.ID)
    
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

