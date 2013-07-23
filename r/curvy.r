
p <- ggplot(mtcars, aes(wt, mpg))+ geom_point() + geom_point(aes(colour = factor(cyl)))
p

alm<-lm(mpg ~ wt+factor(cyl), data=mtcars)

summary(alm)

p+stat_smooth(method="lm", formula = y~x)



predict(alm, data.frame(wt=1, mpg=4, cyl=4), se.fit =T)


fullcurve<-function(amodel, ...) {
  adf<-as.data.frame(expand.grid(...))
  apred<-predict(amodel, adf, se.fit =T, type="response")
  return(cbind(adf, data.frame(fit=apred[[1]], se.fit=apred[[2]])))

}
#debug(fullcurve)
ndf<-fullcurve(alm, wt=1:5,  cyl=c(4,6,8))


names(ndf)[which(names(ndf)=="fit")]<-"mpg"


p+  
  geom_ribbon(aes(x=wt, ymin=mpg-se.fit, ymax=mpg+se.fit, group=factor(cyl) ), fill="lightgrey", alpha=0.5, data=ndf) +
  geom_line(aes(x=wt, y=mpg, colour=factor(cyl)), data=ndf)

