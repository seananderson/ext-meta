catPlotFun <- function(data, response, variance, studyID, group=NA, size=F) {
 
 require(ggplot2)
 require(plyr) 
 
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("data", "response", "variance", "studyID", "group"), 
             names(mf), 0L)
  
  data<-cbind(data, 
              response = eval(substitute(response), data),
              variance = eval(substitute(variance), data),
              group = eval(substitute(group), data),
              studyID = eval(substitute(variance), data))
  
  data$CIresponse <- 1.96*sqrt(data$variance)
  
  rankTab <- NA
  mf <- match.call(expand.dots = FALSE)
  
  #rank the data
  data$rank <- rank(data$response)
  
  #but if it's grouped...
  if(m[5] !=0 ){
    data$number <- 1:nrow(data)
    rankTab <- ddply(data, .(group), summarise, rank = rank(response)-length(response)/2, number=number)
    data$rank <- sapply(data$number, function(x) rankTab[which(rankTab$number==x),]$rank)
    
  }
  
  catPlots <- ggplot(data=data) 
  if(size==T){
    catPlots <- catPlots +geom_point(mapping=aes(x=rank, y=response, color=studyID, size=K))
  }else{catPlots <- catPlots + geom_point(mapping=aes(x=rank, y=response, color=studyID))
        }
  
  catPlots <- catPlots +
    xlab("") +
    ylab("Response") +
    geom_linerange(mapping=aes(x=rank, ymin=response-CIresponse, ymax=response+CIresponse)) +
    geom_hline(yintercept=0, lty=2)

  #add facets if there's a group
  if(m[5] !=0 ) {
  	if(length(m[5]==1)) catPlots <- catPlots + facet_grid(~group, scale="free_x") 
  	if(length(m[5]==2)) catPlots <- catPlots + facet_grid(group[1]~group[2], scale="free_x") 
 }
  catPlots +
  theme_bw() +
  theme(
      axis.text.x= element_blank(),
      axis.ticks.x = element_blank()) + guides(colour = guide_legend(ncol = 2))
  
  
}


#catPlotFun(marine, LRR1, VLRR1, Ref, Ycat)
