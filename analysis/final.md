RMA Outputs
========================================================
  
  Here we go...

















Broad v. Narrow Baseline
========================================================

First, we have to verify our assumptions about pooling all of the data.

Does grouping matter?

```r
taxGenera.Broad <- rma(yi = lnorReg, vi = vlnorReg, mods = ~Bivalve..Gastropod + 
    Tax.level, data = broadData)
taxGenera.Broad
```

```
## 
## Mixed-Effects Model (k = 80; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.3553 (SE = 0.1245)
## tau (square root of estimated tau^2 value):             0.5960
## I^2 (residual heterogeneity / unaccounted variability): 49.52%
## H^2 (unaccounted variability / sampling variability):   1.98
## 
## Test for Residual Heterogeneity: 
## QE(df = 77) = 149.2280, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2,3): 
## QM(df = 2) = 1.4411, p-val = 0.4865
## 
## Model Results:
## 
##                              estimate      se     zval    pval    ci.lb
## intrcpt                        1.1432  0.1731   6.6053  <.0001   0.8040
## Bivalve..GastropodGastropod    0.1126  0.2180   0.5163  0.6057  -0.3147
## Tax.levelSpecies              -0.2358  0.2104  -1.1204  0.2626  -0.6482
##                               ci.ub     
## intrcpt                      1.4825  ***
## Bivalve..GastropodGastropod  0.5399     
## Tax.levelSpecies             0.1767     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



Does Multistage matter?

```r
multStage.Broad <- rma(yi = lnorReg, vi = vlnorReg, mods = ~MultipleStages, 
    data = broadData)
multStage.Broad
```

```
## 
## Mixed-Effects Model (k = 80; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.3658 (SE = 0.1255)
## tau (square root of estimated tau^2 value):             0.6048
## I^2 (residual heterogeneity / unaccounted variability): 50.45%
## H^2 (unaccounted variability / sampling variability):   2.02
## 
## Test for Residual Heterogeneity: 
## QE(df = 78) = 151.8594, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.2337, p-val = 0.6288
## 
## Model Results:
## 
##                     estimate      se    zval    pval    ci.lb   ci.ub     
## intrcpt               0.9790  0.1741  5.6224  <.0001   0.6377  1.3203  ***
## MultipleStagesTRUE    0.1052  0.2175  0.4835  0.6288  -0.3212  0.5315     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Does Global v. Regional matter?

```r
scale.Broad <- rma(yi = lnorReg, vi = vlnorReg, mods = ~Global.Regional, data = broadData)
scale.Broad
```

```
## 
## Mixed-Effects Model (k = 80; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.3622 (SE = 0.1247)
## tau (square root of estimated tau^2 value):             0.6018
## I^2 (residual heterogeneity / unaccounted variability): 50.25%
## H^2 (unaccounted variability / sampling variability):   2.01
## 
## Test for Residual Heterogeneity: 
## QE(df = 78) = 151.9811, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.4515, p-val = 0.5016
## 
## Model Results:
## 
##                   estimate      se    zval    pval    ci.lb   ci.ub     
## intrcpt             0.9926  0.1311  7.5701  <.0001   0.7356  1.2495  ***
## Global.Regional1    0.1449  0.2156  0.6719  0.5016  -0.2777  0.5674     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Does Time Matter?

```r
time.Broad <- rma(yi = lnorReg, vi = meanDate, mods = ~meanDate, data = broadData)
time.Broad
```

```
## 
## Mixed-Effects Model (k = 80; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0 (SE = 1.0284)
## tau (square root of estimated tau^2 value):             0
## I^2 (residual heterogeneity / unaccounted variability): 0.00%
## H^2 (unaccounted variability / sampling variability):   1.00
## 
## Test for Residual Heterogeneity: 
## QE(df = 78) = 5.4225, p-val = 1.0000
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.0066, p-val = 0.9351
## 
## Model Results:
## 
##           estimate      se    zval    pval    ci.lb   ci.ub   
## intrcpt     0.9550  0.4591  2.0802  0.0375   0.0552  1.8548  *
## meanDate    0.0007  0.0091  0.0814  0.9351  -0.0172  0.0186   
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


If we pool everything, what is our grand mean Log Odds Ratio for Broad v. Narrow?

```r
broad.rma <- rma(yi = lnorReg, vi = vlnorReg, data = broadData)
broad.rma
```

```
## 
## Random-Effects Model (k = 80; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of total heterogeneity): 0.3534 (SE = 0.1217)
## tau (square root of estimated tau^2 value):      0.5945
## I^2 (total heterogeneity / total variability):   49.94%
## H^2 (total variability / sampling variability):  2.00
## 
## Test for Heterogeneity: 
## Q(df = 79) = 152.1462, p-val < .0001
## 
## Model Results:
## 
## estimate       se     zval     pval    ci.lb    ci.ub          
##   1.0456   0.1034  10.1103   <.0001   0.8429   1.2483      *** 
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

####### Fig 1
```


Figure 1
========================================================

```r
pdf("broad_narrow_rma.pdf", width = 3.7, height = 7.5)
plot_effect_sizes(broadData, rma.model = broad.rma)
dev.off()
```

```
## pdf 
##   2
```

```r
plot_effect_sizes(broadData, rma.model = broad.rma)
```

![plot of chunk Fig1](figure/Fig1.png) 




Epifauna v. Infauna Baseline
========================================================
  
Are Bivalves or Gastropods Different?

```r
bivalve.gastro.Epifaunal <- rma(yi = lnorReg, vi = vlnorReg, data = habitData, 
    mod = ~Bivalve..Gastropod - 1)
bivalve.gastro.Epifaunal
```

```
## 
## Mixed-Effects Model (k = 49; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.4387 (SE = 0.1667)
## tau (square root of estimated tau^2 value):             0.6623
## I^2 (residual heterogeneity / unaccounted variability): 59.42%
## H^2 (unaccounted variability / sampling variability):   2.46
## 
## Test for Residual Heterogeneity: 
## QE(df = 47) = 118.6184, p-val < .0001
## 
## Test of Moderators (coefficient(s) 1,2): 
## QM(df = 2) = 0.8539, p-val = 0.6525
## 
## Model Results:
## 
##                              estimate      se     zval    pval    ci.lb
## Bivalve..GastropodBivalve     -0.1248  0.1468  -0.8507  0.3950  -0.4125
## Bivalve..GastropodGastropod    0.1098  0.3042   0.3609  0.7182  -0.4865
##                               ci.ub   
## Bivalve..GastropodBivalve    0.1628   
## Bivalve..GastropodGastropod  0.7061   
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

##### So, we pool.  Let's look at things, then
```

No - they are broadly the same.


Does global v. regional extinction matter?

```r
scale.habit <- rma(yi = lnorReg, vi = vlnorReg, mods = ~Global.Regional, data = habitData)
scale.habit
```

```
## 
## Mixed-Effects Model (k = 49; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.4391 (SE = 0.1677)
## tau (square root of estimated tau^2 value):             0.6626
## I^2 (residual heterogeneity / unaccounted variability): 58.38%
## H^2 (unaccounted variability / sampling variability):   2.40
## 
## Test for Residual Heterogeneity: 
## QE(df = 47) = 119.4409, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 1.9268, p-val = 0.1651
## 
## Model Results:
## 
##                   estimate      se     zval    pval    ci.lb   ci.ub   
## intrcpt            -0.0067  0.1425  -0.0473  0.9622  -0.2861  0.2726   
## Global.Regional1   -0.5303  0.3821  -1.3881  0.1651  -1.2791  0.2185   
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Does time matter?

```r
# SA 20130119: I changed bivalvesEpifaunal to habitatData. I believe this
# was just a mistake when copying the code over
time.Epifaunal <- rma(yi = lnorReg, vi = vlnorReg, data = habitData, mods = ~meanDate)
time.Epifaunal
```

```
## 
## Mixed-Effects Model (k = 49; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.4313 (SE = 0.1653)
## tau (square root of estimated tau^2 value):             0.6567
## I^2 (residual heterogeneity / unaccounted variability): 58.85%
## H^2 (unaccounted variability / sampling variability):   2.43
## 
## Test for Residual Heterogeneity: 
## QE(df = 47) = 114.0778, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 1.0288, p-val = 0.3104
## 
## Model Results:
## 
##           estimate      se     zval    pval    ci.lb   ci.ub   
## intrcpt    -0.2087  0.1821  -1.1460  0.2518  -0.5656  0.1482   
## meanDate    0.0015  0.0014   1.0143  0.3104  -0.0014  0.0043   
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


If we pool everything, what is our grand mean Log Odds Ratio for Epifaunal v. Infaunal?

```r
meanModel.Epifaunal <- rma(yi = lnorReg, vi = vlnorReg, data = habitData)
meanModel.Epifaunal
```

```
## 
## Random-Effects Model (k = 49; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of total heterogeneity): 0.4339 (SE = 0.1640)
## tau (square root of estimated tau^2 value):      0.6587
## I^2 (total heterogeneity / total variability):   59.09%
## H^2 (total variability / sampling variability):  2.44
## 
## Test for Heterogeneity: 
## Q(df = 48) = 119.9424, p-val < .0001
## 
## Model Results:
## 
## estimate       se     zval     pval    ci.lb    ci.ub          
##  -0.0808   0.1318  -0.6133   0.5397  -0.3391   0.1775          
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Figure 2
========================================================

```r
pdf("inf_epi_rma.pdf", width = 3.7, height = 6)
plot_effect_sizes(habitData, meanModel.Epifaunal)
dev.off()
```

```
## pdf 
##   2
```

```r
plot_effect_sizes(habitData, meanModel.Epifaunal)
```

![plot of chunk Fig2](figure/Fig2.png) 

```r

# now fix up the labels (spacing and the et al.s)
```



Modeled Results
========================================================
The Fitted Model for Broad v. Narrow

```r
broadDataExtinction <- broadData[which(!is.na(broadData$BC.extinction.ratePBDB)), 
    ]

covModel.Broad.RMA <- rma(yi = lnorReg, vi = vlnorReg, data = broadDataExtinction, 
    mods = ~OA + BC.extinction.ratePBDB + d18OresidualMean + del.34S + del.13C)

covModel.Broad.RMA
```

```
## 
## Mixed-Effects Model (k = 73; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.2179 (SE = 0.1081)
## tau (square root of estimated tau^2 value):             0.4668
## I^2 (residual heterogeneity / unaccounted variability): 35.24%
## H^2 (unaccounted variability / sampling variability):   1.54
## 
## Test for Residual Heterogeneity: 
## QE(df = 67) = 105.9153, p-val = 0.0017
## 
## Test of Moderators (coefficient(s) 2,3,4,5,6): 
## QM(df = 5) = 8.5172, p-val = 0.1299
## 
## Model Results:
## 
##                         estimate      se     zval    pval    ci.lb   ci.ub
## intrcpt                   2.5903  0.8527   3.0379  0.0024   0.9191  4.2615
## OA                        0.2164  0.3104   0.6973  0.4856  -0.3919  0.8247
## BC.extinction.ratePBDB   -0.0955  1.0194  -0.0937  0.9253  -2.0934  1.9024
## d18OresidualMean         -0.0627  0.2042  -0.3068  0.7590  -0.4629  0.3376
## del.34S                  -0.0663  0.0390  -1.7023  0.0887  -0.1426  0.0100
## del.13C                  -0.1305  0.1035  -1.2608  0.2074  -0.3335  0.0724
##                           
## intrcpt                 **
## OA                        
## BC.extinction.ratePBDB    
## d18OresidualMean          
## del.34S                  .
## del.13C                   
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

write.csv(coef(covModel.Broad.RMA), "./broadCoefTable.csv", row.names = T)

broadCoefPlot <- coefPlot(covModel.Broad.RMA, robust = F, std = T) + scale_x_discrete(labels = c("Extinction Rate", 
    expression(delta^18 * O ~ Residuals), expression(delta^13 * C), expression(delta^34 * 
        S), "Acidification")) + annotate("text", x = 5, y = -0.4, label = "A)") + 
    ylim(c(-0.5, 0.5)) + coord_flip()
```

```
## Error: incorrect number of dimensions
```


The Fitted Model for Epifauna v. Infauna

```r
habitDataGood <- habitData[which(!(is.na(habitData$BC.extinction.ratePBDB))), 
    ]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$lnorReg))), ]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$del.34S))), ]

covModel.Epifaunal.rma <- rma(yi = lnorReg, vi = vlnorReg, data = habitDataGood, 
    mods = ~OA + BC.extinction.ratePBDB + d18OresidualMean + del.34S)

covModel.Epifaunal.rma
```

```
## 
## Mixed-Effects Model (k = 46; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.3042 (SE = 0.1504)
## tau (square root of estimated tau^2 value):             0.5516
## I^2 (residual heterogeneity / unaccounted variability): 47.44%
## H^2 (unaccounted variability / sampling variability):   1.90
## 
## Test for Residual Heterogeneity: 
## QE(df = 41) = 74.7443, p-val = 0.0010
## 
## Test of Moderators (coefficient(s) 2,3,4,5): 
## QM(df = 4) = 10.8306, p-val = 0.0285
## 
## Model Results:
## 
##                         estimate      se     zval    pval    ci.lb
## intrcpt                   6.1354  2.3497   2.6112  0.0090   1.5302
## OA                       -0.3477  0.4210  -0.8259  0.4089  -1.1728
## BC.extinction.ratePBDB   -0.6114  0.9894  -0.6179  0.5366  -2.5506
## d18OresidualMean          0.7041  0.2358   2.9860  0.0028   0.2419
## del.34S                  -0.3048  0.1165  -2.6154  0.0089  -0.5333
##                           ci.ub    
## intrcpt                 10.7406  **
## OA                       0.4774    
## BC.extinction.ratePBDB   1.3278    
## d18OresidualMean         1.1663  **
## del.34S                 -0.0764  **
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
write.csv(coef(covModel.Epifaunal.rma), "./epiCoefTable.csv", row.names = T)


epiCoefPlot <- coefPlot(covModel.Epifaunal.rma, habitDataGood, robust = F, std = T) + 
    scale_x_discrete(labels = c("Extinction Rate", expression(delta^18 * O ~ 
        Residuals), expression(delta^34 * S), "Acidification")) + annotate("text", 
    x = 4, y = -1.5, label = "B)") + ylim(c(-1.75, 1.75)) + coord_flip()
```

```
## Error: incorrect number of dimensions
```


Figure 4
========================================================

```r
grid.arrange(broadCoefPlot + theme_bw(base_size = 18), epiCoefPlot + theme_bw(base_size = 18), 
    ncol = 2)
```

```
## Error: object 'broadCoefPlot' not found
```


Figure 5
========================================================

```r
#### What are the marginal effects from the model
del18marg <- marginalLine(covModel.Epifaunal.rma, "d18OresidualMean", habitDataGood, 
    robust = F) + xlab("\n Detrended Delta O18") + ylab("Component + Residual + Intercept Log Odds\n Ratios for Detrended Delta O18\n") + 
    annotate("text", x = -4, y = 8.75, label = "A)") + scale_color_discrete(guide = "none") + 
    theme_bw(base_size = 18)
```

```
## Error: incorrect number of dimensions
```

```r

del18MargData <- marginalData(covModel.Epifaunal.rma, "d18OresidualMean", habitDataGood)
```

```
## Error: incorrect number of dimensions
```

```r
write.csv(del18MargData, "./del18MargData.csv", row.names = F)
```

```
## Error: object 'del18MargData' not found
```

```r

del34marg <- marginalLine(covModel.Epifaunal.rma, "del.34S", habitDataGood, 
    robust = F) + xlab("\n Delta S34") + ylab("Component + Residual + Intercept Log Odds\n Ratios for Delta 34S\n") + 
    annotate("text", x = 13.75, y = 3.375, label = "B)") + theme_bw(base_size = 18)
```

```
## Error: incorrect number of dimensions
```

```r

del34margData <- marginalData(covModel.Epifaunal.rma, "del.34S", habitDataGood)
```

```
## Error: incorrect number of dimensions
```

```r
write.csv(del34marg, "./del34margData.csv", row.names = F)
```

```
## Error: object 'del34marg' not found
```

```r

# Extract Legend
g_legend <- function(a.gplot) {
    a.gplot <- a.gplot + scale_color_discrete("Study")
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

legend <- g_legend(del34marg)
```

```
## Error: object 'del34marg' not found
```

```r


grid.arrange(del18marg, del34marg + scale_color_discrete(guide = "none"), legend, 
    widths = c(3, 3, 1), nrow = 1)
```

```
## Error: object 'del18marg' not found
```



Appendix Jackknife Figures
========================================================

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'as.data.frame': Error in t(coefs$estimate) : error in evaluating
## the argument 'x' in selecting a method for function 't': Error in
## coefs$estimate : $ operator is invalid for atomic vectors
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'as.data.frame': Error in t(coefs$estimate) : error in evaluating
## the argument 'x' in selecting a method for function 't': Error in
## coefs$estimate : $ operator is invalid for atomic vectors
```



```r
funnel(broad.rma, main = "Funnel Plot for Broad v. Narrow Analysis")
```

![plot of chunk funnelPlots](figure/funnelPlots1.png) 

```r
funnel(meanModel.Epifaunal, main = "Funnel Plot for Epifauna v. Infauna Analysis")
```

![plot of chunk funnelPlots](figure/funnelPlots2.png) 
