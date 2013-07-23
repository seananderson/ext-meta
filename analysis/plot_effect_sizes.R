# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 16, 2012
# Last modified: Jul 10, 2013
# Purpose:       plot the effect sizes with an overall mean rma
# ====================================================================

# first, run "final_for_paper.R"
## @knitr ploteffLoad
plot_effect_sizes <- function(dat, rma.model) {
  par(mar = c(3, 10.5, 1, 1), cex = 0.8)
  par(mgp = c(2, 0.5, 0))
  par(tck = -0.03)
  dat <- ddply(dat, "In.text.Citation", transform, mean.lnorReg = mean(lnorReg))
  dat <- dat[order(-dat$mean.lnorReg, dat$lnorReg),] 
  dat <- transform(dat, label.show = c(1, diff(as.numeric(dat$In.text.Citation))), label.In.text.Citation = In.text.Citation)
  dat$label.In.text.Citation <- as.character(dat$label.In.text.Citation)
  for(i in 1:(nrow(dat)-1)) {
    if(dat$In.text.Citation[i] == dat$In.text.Citation[i+1]) dat$label.In.text.Citation[i] <- ""
  }
  dat <- dat[order(-dat$mean.lnorReg, -dat$lnorReg),] 
  dat <- transform(dat, plot.ID = 1:nrow(dat))
  with(dat, plot.default(lnorReg, plot.ID, xlim = c(-5, 5), ylim = c(-2, nrow(dat)+0.5), axes = FALSE, ylab = "", xlab = "Log odds ratio", yaxs = "i"))
  with(dat, segments(lnorReg - 2*sqrt(vlnorReg),plot.ID,  lnorReg + 2*sqrt(vlnorReg), plot.ID))
  box()
  axis(1)
  abline(v = 0, lty = 2)

# center citation labels:
  study.labs <- subset(dat, label.In.text.Citation != "")[,c("In.text.Citation", "plot.ID")]
  study.labs$lab.loc <- NA
  for(i in 1:(nrow(study.labs) - 1)) study.labs$lab.loc[i] <- mean(c(study.labs$plot.ID[i], study.labs$plot.ID[i+1]))
  study.labs$lab.loc[nrow(study.labs)] <- study.labs$plot.ID[nrow(study.labs)] + 0.5
  study.labs$lab.loc <- study.labs$lab.loc - 0.5

  #axis(2, at = 1:nrow(dat), labels = dat$label.In.text.Citation, las = 1, tick = FALSE, cex.axis = 0.8)
  with(study.labs, axis(2, at = lab.loc, labels = In.text.Citation, las = 1, tick = FALSE, cex.axis = 0.8))

  abline(h = 1:nrow(dat), col = "#00000010")
  rects <- subset(dat, label.In.text.Citation != "")[,c("plot.ID")]
  odd <- seq(1, 19, 2) # 19 is random... just needs to be big enough
  if(length(rects) %% 2 == 1) rects <- c(rects, nrow(dat) + 1) # if odd, add rect on end
  for(i in odd) rect(-6, rects[i]-0.5, 6, rects[i+1]-0.5, col = "#00000020", border = FALSE)
## Sean changed these lines on 2013-07-10:
  #est <- coef(rma.model)$estimate
  #se <- coef(rma.model)$se
## to:
  est <- rma.model$b[1]
  se <- rma.model$se

  polygon(c(est, est - 1.96*se, est, est + 1.96*se), -c(0.7, 1, 1.3, 1), col = "grey40")
  axis(2, at = -0.7, labels = "Meta-analytic mean", las = 1, tick = FALSE, cex.axis = 0.8)
}

## @knitr Fig1
pdf("broad_narrow_rma.pdf", width = 3.7, height = 7.5)
plot_effect_sizes(broadData, rma.model = broad.rma)
dev.off()
plot_effect_sizes(broadData, rma.model = broad.rma)

## @knitr Fig2
pdf("inf_epi_rma.pdf", width = 3.7, height = 6)
plot_effect_sizes(habitData, meanModel.Epifaunal)
dev.off()
plot_effect_sizes(habitData, meanModel.Epifaunal)

# now fix up the labels (spacing and the et al.s)

