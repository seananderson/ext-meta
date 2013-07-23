
robust.se <- function(data,X.full,rho) {
  p <- ncol(X.full)-2
  N <- max(data$study)
  sumXWX <- 0
  sumXWy<- 0
  sumXWJWX <- 0
  sumXWVWX <- 0
  sumXW.sig.m.v.WX <- 0
  
  for (i in (1: N)) {	
    tab <- data[data$study == i,]
    W <- diag(tab$weights,tab$k[1])
    tab2 <- X.full[X.full$study==i,]
    tab3 <- cbind(tab2[-c(1)])
    X <- data.matrix(tab3)
    dimnames(X) <- NULL 
    y <- cbind(tab$effect.size) 
    one <- cbind(rep(1,tab$k[1]))
    J <- one%*%t(one)
    sigma <- (tab$s%*%t(tab$s))
    vee <- diag(tab$s^2,tab$k[1])	
    SigmV <- sigma - vee
    
    sumXWX <- sumXWX + t(X)%*%W%*%X
    sumXWy <- sumXWy + t(X)%*%W%*%y
    sumXWJWX <- sumXWJWX + t(X)%*%W%*%J%*%W%*%X
    sumXWVWX <- sumXWVWX + t(X)%*%W%*%vee%*%W%*%X
    sumXW.sig.m.v.WX <- sumXW.sig.m.v.WX + t(X)%*%W%*%SigmV%*%W%*%X
  }
  
  b <- solve(sumXWX)%*%sumXWy
  X <- data.matrix(X.full[-c(1)])
  dimnames(X) <- NULL
  data$pred <- X%*%b
  data$e <- data$effect.size - data$pred
  W <- diag(data$weights)
  sumW <- sum(data$weights)
  Qe <-t(data$e)%*%W%*%data$e
  
  denom <- sumW - sum(diag(solve(sumXWX)%*%sumXWJWX))
  term1 <- (Qe - N + sum(diag(solve(sumXWX)%*%sumXWVWX))) /denom
  term2 <- (sum(diag(solve(sumXWX)%*%sumXW.sig.m.v.WX ))) / denom
  tau.sq1 <- term1 + rho*term2
  tau.sq <- ifelse(tau.sq1<0,0,tau.sq1)
  data$r.weights <- 1/(data$k*(data$mean.v + tau.sq))
  
  sumXWX.r <- 0
  sumXWy.r<- 0
  for (i in (1: N)) {	
    tab <- data[data$study == i,]
    W <- diag(tab$r.weights,tab$k[1])
    tab2 <- X.full[X.full$study==i,]
    tab3 <- cbind(tab2[-c(1)])
    X <- data.matrix(tab3)
    dimnames(X) <- NULL 
    
    y <- cbind(tab$effect.size) 
    
    sumXWX.r <- sumXWX.r + t(X)%*%W%*%X
    sumXWy.r <- sumXWy.r + t(X)%*%W%*%y
  }
  
  b.r <- solve(sumXWX.r)%*%sumXWy.r
  X <- data.matrix(X.full[-c(1)])
  dimnames(X) <- NULL
  data$pred.r <- X%*%b.r
  data$e.r <- cbind(data$effect.size) - data$pred.r
  
  sumXWeeWX.r <- 0 
  for (i in (1:N)) {
    tab <- data[data$study== i,]
    sigma.hat.r <- tab$e.r%*%t(tab$e.r)
    W <- diag(tab$r.weights,tab$k[1])
    tab2 <- X.full[X.full$study==i,]
    tab3 <- cbind(tab2[-c(1)])
    X <- data.matrix(tab3)
    dimnames(X) <- NULL 
    
    sumXWeeWX.r <- sumXWeeWX.r + t(X)%*%W%*%sigma.hat.r%*%W%*%X
  }
  
  VR.r <- solve(sumXWX.r)%*%sumXWeeWX.r%*%solve(sumXWX.r)
  
  SE <- c(rep(0,p+1))
  for (i in (1:(p+1))) {
    SE[i] <-sqrt(VR.r[i,i])*sqrt(N/(N-(p+1)))
  }
  
  labels <- seq(0,p-1,1)
  output <- cbind(labels,b.r,SE)
  colnames(output) <- c("beta", "estimate","SE")
  list("Tau.sq estimate" = tau.sq, "Estimates & Robust SE's"= output)
}


compare <- function(data,X.full) {
  rho <- seq(0,1,.1)
  tau.sq.v <- function(data,X.full,rho) {
    p <- ncol(X.full)-2
    N <- max(data$study)
    sumXWX <- 0
    sumXWy<- 0
    sumXWJWX <- 0
    sumXWVWX <- 0
    sumXW.sig.m.v.WX <- 0
    
    for (i in (1: N)) {	
      tab <- data[data$study == i,]
      W <- diag(tab$weights,tab$k[1])
      tab2 <- X.full[X.full$study==i,]
      tab3 <- cbind(tab2[-c(1)])
      X <- data.matrix(tab3)
      dimnames(X) <- NULL 
      y <- cbind(tab$effect.size) 
      one <- cbind(rep(1,tab$k[1]))
      J <- one%*%t(one)
      sigma <- (tab$s%*%t(tab$s))
      vee <- diag(tab$s^2,tab$k[1])	
      SigmV <- sigma - vee
      
      sumXWX <- sumXWX + t(X)%*%W%*%X
      sumXWy <- sumXWy + t(X)%*%W%*%y
      sumXWJWX <- sumXWJWX + t(X)%*%W%*%J%*%W%*%X
      sumXWVWX <- sumXWVWX + t(X)%*%W%*%vee%*%W%*%X
      sumXW.sig.m.v.WX <- sumXW.sig.m.v.WX + t(X)%*%W%*%SigmV%*%W%*%X
    }
    
    b <- solve(sumXWX)%*%sumXWy
    X <- data.matrix(X.full[-c(1)])
    dimnames(X) <- NULL
    data$pred <- X%*%b
    data$e <- data$effect.size - data$pred
    W <- diag(data$weights)
    sumW <- sum(data$weights)
    Qe <-t(data$e)%*%W%*%data$e
    
    denom <- sumW - sum(diag(solve(sumXWX)%*%sumXWJWX))
    term1 <- (Qe - N + sum(diag(solve(sumXWX)%*%sumXWVWX))) /denom
    term2 <- (sum(diag(solve(sumXWX)%*%sumXW.sig.m.v.WX ))) / denom
    tau.sq1 <- term1 + rho*term2
    tau.sq <- ifelse(tau.sq1<0,0,tau.sq1)
    tau.sq
  }
  
  tau.sq <- c(rep(0,length(rho)))
  for (i in (1:length(rho))) {	
    tau.sq[i] <-tau.sq.v(data,X.full,rho[i])
  }
  output <- cbind(rho,tau.sq)
  colnames(output) <- c("rho","tau.sq")
  output
}
