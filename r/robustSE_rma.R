# from https://stat.ethz.ch/pipermail/r-help/2010-June/242542.html
#### Robust SE based on Hedges et al., (2010) Eq. 6 on Research Synthesis Methods
#### rma.obj: object fitted by metafor()
#### cluster: indicator for clusters of studies

robustSE <- function(rma.obj, cluster=NULL, CI=.95) {
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
  p <- length(b)                             # no. of predictors including intercept
  se <- sqrt( diag(V.R)*m/(m-p) )            # small sample adjustment (Eq.7)
  tval <- b/se
  pval <- 2*(1-pt(abs(tval),df=(m-p)))
  crit <- qt( (1-CI)/2, df=(m-p), lower.tail=FALSE )
  ci.lb <- b-crit*se
  ci.ub <- b+crit*se
  data.frame(estimate=b, se=se, tval=tval, pval=pval, ci.lb=ci.lb, ci.ub=ci.ub)
}


#robustSE(rma(yi = lnorReg, vi = vlnorReg, data=broadData, mods=~OA + BC.extinction.ratePBDB.cent + d18OresidualMean.cent + del.34S.cent), cluster=broadData$study.ID)
