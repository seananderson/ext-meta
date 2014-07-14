# Purpose:       Try plotting the effect sizes against the raw data.

#Figure 2: Sean's summary graph of (A) log-adds geo range (i.e., GSA slide 10), (2) log-odds life habit (i.e., GSA slide 14), (3) ext rates, (4) del18O residuals, (5) del13C, (6) del34S, (7) OAE, and (8) predation through time


#d18dat <- temp[gr$data_subset == "All" ,c("Binned_Age", "mean_d18O")]

#####
source("../r/data_clean_merge.r")
source("../r/metaprep.r")
source("../r/lmerMetaPrep.R")
# sealevel : Time..my. and del.34S and del.18O
# extMag has BC.extinction.rate and Midpoint..Ma.
stageTime <- transform(stageTime, Midpt = Start..Ma. - (Start..Ma. - End..Ma.)/2)

# volcbolide has State.stage and OA, will need to merge with
# stageTime[,c("Bin.name", "Start..Ma.", "Midpt", "End..Ma.")]
#names(stageTime)[2] <- "State.stage"
#volcbolide2 <- merge(volcbolide, stageTime[,c("State.stage", "Start..Ma.", "Midpt", "End..Ma.")], all.x = TRUE)

# get ORs in a nice format for plotting:
broad_d <- subset(ext, Aggregate.Trait == "Geographic Range" & Trait.category == "Broad")[,c("study.ID", "lnorReg", "vlnorReg", "meanDate")]
broad_d$effect_type <- "range"
broad_d$study.ID <- as.character(broad_d$study.ID)
broad_d$study_num <- as.numeric(as.factor(broad_d$study.ID))

epi_d <- subset(ext, Aggregate.Trait == "Life Habit" & Trait.category %in% c("Epifaunal", "Epifauna", "Epifuanl", "Epifaunalcalciticouterlayer", "Epifaunalcompetelyaragonitic"))[,c("study.ID", "lnorReg", "vlnorReg", "meanDate")]
epi_d$effect_type <- "habit"
epi_d$study.ID <- as.character(epi_d$study.ID)
epi_d$study_num <- as.numeric(as.factor(epi_d$study.ID))


effect_d <- rbind(broad_d, epi_d)

library(RColorBrewer)
pal <- paste(brewer.pal(9, "Set1"), "80", sep = "")
pal <- c(pal, pal, pal) # just in case for now, repeat the colours
source("gg_color_hue.R")
set.seed(20) # to get the same colours each time
pal <- paste(gg_color_hue(max(effect_d$study_num)), "80", sep = "")[sample(1:max(effect_d$study_num), max(effect_d$study_num))]

effect_d <- transform(effect_d, u = lnorReg + 1.96*sqrt(vlnorReg), l = lnorReg - 1.96*sqrt(vlnorReg), circle_rad = 1/(sqrt(vlnorReg/pi)), study_col = pal[study_num])
effect_d$study_col <- as.character(effect_d$study_col)
#area = pi * r^2
# r = sqrt(area / pi)

pdf("figure/selectivity-data-overview.pdf", width = 7, height = 5.5)
#l <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7))
l <- matrix(c(1, 1, 2, 2, 3,  4, 5, 6, 7))
layout(l)
par(mar = c(1,1,0.7,0.1), oma = c(3.1, 2.3, 1.5, 0), cex = 0.7)
#par(mfrow = c(7, 1), mar = c(1,1,0.7,0.5), oma = c(1.5, 0, 1, 0), cex = 0.7)
xlim <- c(570, 0)
par(xpd = NA)
line_col <- "#00000090"
ylabel <- function(text, line = -1.3) mtext(text, side = 3, cex = 0.8, line = line, col = "grey50", adj = 0)
ann <- function(x, y, add.last = TRUE) {

  x <- x[!is.na(y)]
  y <- y[!is.na(y)]

  min.pos <- which.min(y)
  max.pos <- which.max(y)
  x.min <- x[min.pos]
  y.min <- y[min.pos]
  x.max <- x[max.pos]
  y.max <- y[max.pos]

  last.pos <- which.min(x)
  x.last <- x[last.pos]
  y.last <- y[last.pos]

  text(x.min, y.min, sprintf("%.1f", round(y.min, 1)), col = "grey50", pos = 1, cex = 0.8, offset = 0.25)
  text(x.max, y.max, sprintf("%.1f", round(y.max, 1)), col = "grey50", pos = 3, cex = 0.8, offset = 0.25)
  if(add.last)
    text(x.last, y.last, sprintf("%.1f", round(y.last, 1)), col = "grey50", pos = 3, cex = 0.8, offset = 0.25)
}


plot_effects <- function(dat) {
  with(dat, plot(xlim[1] - meanDate, lnorReg, xlim = rev(xlim), axes = F, xlab = "", col = "#00000060", type = "n", ylab = ""))
  with(dat, segments(xlim[1] - meanDate, l, xlim[1] - meanDate, u, col = "#00000050"))
  with(dat, symbols(xlim[1] - meanDate, lnorReg, circles = circle_rad, bg = study_col, inches = FALSE, add = TRUE, col = "#00000030", lwd = 0.5))
  #with(dat, symbols(xlim[1] - meanDate, lnorReg, circles = circle_rad, bg = "#00000020", inches = FALSE, add = TRUE, col = "#00000030", lwd = 0.5))
  #segments(xlim[2], 0, xlim[1], 0, col = "#00000030")
  segments(40, 0, xlim[1], 0, col = "#00000030")
}

# argh! there's a bug with symbols so the xlim can't be flipped:
plot_effects(subset(effect_d, effect_type == "range"))
ylabel(expression((a)~Geographic~range))
text(-7, 0.8, "Favours broad", col = "grey50", pos = 4)
text(-7, -0.8, "Favours narrow", col = "grey50", pos = 4)
axis(2, col = "grey50", col.axis = "grey50", tck = -0.05, at = c(-2, 0, 2), hadj = 1, las = 1)
mtext("Log odds ratio", side = 2, line = 2.1, cex = 0.8, col = "grey50", adj= 16.9)

plot_effects(subset(effect_d, effect_type == "habit"))
ylabel(expression((b)~Life~habit))
text(-7, 0.8, "Favours epifauna", col = "grey50", pos = 4)
text(-7, -0.8, "Favours infauna", col = "grey50", pos = 4)
axis(2, col = "grey50", col.axis = "grey50", tck = -0.05, at = c(-2, 0, 2), hadj = 1, las = 1)
#mtext("Odds ratio", side = 2, line = 2, cex = 0.7, col = "grey50", adj= 0)

# legend? we'll need 2 and we'll need to get creative...
col.df <- effect_d
col.df <- col.df[!duplicated(col.df[,c("study.ID", "study_col", "effect_type")]), ]
#with(subset(col.df, effect_type == "range"), legend(x = 400, y = 4, legend = study.ID, col = "#00000030", pt.bg = study_col, bty = "n", pch = 21))

with(stageTime, plot(Midpt, rep(1, nrow(stageTime)), pch = c(21, 19)[OA+1], xlim = xlim, axes = FALSE, xlab = "", col = c("#00000020", "#00000080")[OA+1], cex = 1.5, ylab = ""))
ylabel(expression((c)~OA~event))

with(subset(stageTime, !is.na(mean_d18O.prok)), plot(Midpt, mean_d18O.prok, type = "l", xlim = xlim, axes = FALSE, xlab = "", col = line_col, ylab = ""))
ylabel(expression((d)~delta^18*O))
#ylabel("residuals")
with(stageTime, ann(Midpt, mean_d18O.prok))


with(subset(stageTime, !is.na(mean_d13C.prok)), plot(Midpt, mean_d13C.prok, type = "l", xlim = xlim, axes = FALSE, xlab = "", col = line_col, ylab = ""))
ylabel(expression((e)~delta^13*C))
with(stageTime, ann(Midpt, mean_d13C.prok))

with(subset(stageTime, !is.na(mean_d34S.prok)), plot(Midpt, mean_d34S.prok, type = "l", xlim = xlim, axes = FALSE, xlab = "", col = line_col, ylab = ""))
ylabel(expression((f)~delta^34*S))
with(stageTime, ann(Midpt, mean_d34S.prok))


with(stageTime, plot(Midpt, BC.extinction.rate.PBDB3, xlim = xlim, axes = FALSE, type = "l", xlab = "", col = line_col, ylab = ""))
ylabel(expression((g)~Extinction~rate))
with(stageTime, ann(Midpt, BC.extinction.ratePBDB, add.last = FALSE))

#with(stageTime, segments(Start..Ma., rep(0, nrow(stageTime)), Start..Ma., rep(0.7, nrow(stageTime)), col = "#00000017"))

axis(1, col.axis = "grey50", col = "grey50", tck = -0.09, padj = -1.2, cex.axis = 1.1, line = 1.15)
mtext("Geologic time (Ma)", side = 1, line = 2.7, col = "grey50", cex = 0.8)
#axis(2)

source("time_scale.R") # based on Carl's function; add rects()
tscales(top = -0.01, s.bot = -0.22, reso = "periods", cex = 0.9, lwd = 1)

dev.off()

