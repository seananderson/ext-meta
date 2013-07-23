##############
# functions to generate meta-analytic effect sizes and effect size variances


#Chi square to lnor conversion based on rosenthal in cooper and hedges
chisq2lnor<-function(x2, df=1, n){
	
	#1) convert x2 to r	using Rosenthal's forumula from cooper & Hedges
	rdf <- chisq2r(x2, n)
	
	#2) convert r to d using Cooper and Hedges ch 12
	ddf<-r2d(rdf$r, rdf$vr)
	
	#3) convert d to lnor from Rosenthal and return
  	return(d2lnor(ddf$d, ddf$vd))
	
	}

#hi square to lnor conversion based on rosenthal, formula for VR from cooper and hedged
chisq2r<-function(x2, n){
	r=sqrt(x2/n)
	
	return(data.frame(r=r, vr=(1-r^2)^2/(n-1)))
	}

#convert a correlateion to Hedges D from Cooper & Hedges ch12
r2d <- function(r, vr){
	d <- 2*r/sqrt(1-r^2)
	vd<-4*vr/(1-r^2)^3
	
	return(data.frame(d=d, vd=vd))

	}

#convert a Hedge's D to a lnOR from Cooper & Hedges ch12
d2lnor<-function(d, vd){
	#convert d to lnor
	lnor<-pi*d/sqrt(3)
	vlnor<-pi^2*vd/3
	
	return(data.frame(lnor=lnor, vlnor=vlnor))
	}

#convert a students T test to a Hedge's D from cooper & hedges ch 12
t2d<-function(t, n){
	#convert t to d
	d<-t/sqrt(n-1)
	}
	
#convert a student's t to a lnor
t2lnor<-function(t, n){
	d
	}

#####
#convert the 95% Ci from fitted coefficients/effect size to a variance for that effect size	
######
coefCI2var<-function(ci, n, int=95){
	int<-(1-(int/100))/2

	se<-int/qt(int, n-1, lower.tail=F)
	
	se^2
	}
	
######
#for calculating the lnor and vnlor via the Mantel-Haenszel method
#######
lnORMH<-function(tpos, tneg, cpos, cneg){
  o <- tpos
  ohat <- (tpos+cpos)*(tpos+tneg)/(tpos+tneg+cpos+cneg)
  v <- ohat*((tpos+tneg)/(tpos+tneg+cpos+cneg))*((tneg+cneg)/(tpos+tneg+cpos+cneg-1))
	
	lnor <- (o-ohat)/v
	vlnor <- 1/v
  
  return(data.frame(lnor=lnor, vlnor=vlnor))
  
}

######
#for calculating the lnor without the Mantel-Haenszel method
#######
lnORReg<-function(tpos, tneg, cpos, cneg){
	p1<-tpos/(tpos+tneg)
	p2<-cpos/(cpos+cneg)
	lnor <- log((p1/(1-p1)) / (p2/(1-p2)))
	vlnor <- 1/tpos + 1/tneg + 1/cpos + 1/cneg
  
  return(data.frame(lnor=lnor, vlnor=vlnor))
  
}


################
# methods for converging durations to lnor
################

#turn multiple durations into # survived # extinct
dur2SurvExt<-function(durVec, nVec){
	
	#1/duration is the probability of going extinct in the selected interval
	probVec<-1/durVec
	
	#weight by number of species
	wt<-nVec/sum(nVec)

	#get the mean probability
	prob<-weighted.mean(provVec, wt)
	
	return(c(X..Ext=prob*sum(nVec), X..Surv=(1-prob*sum(nVec))))
	
	}

#takes a data frame with a trait value column, a duration column, and a # of species column
#and generates a return data frame with 	
dur2NumTab<-function(adf){
	require(plyr)
	rdf<-ddply(adf, .("Trait.Category"), function(x) with(x, dur2SurvExt(Duration..MY., X..Species)))
	
	rdf$Total...Ext<-rep(sum(rdf$X..Ext))
	rdf$Total...Surv<-rep(sum(rdf$X...Surv))
	
	return(rdf)
}
