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
## R^2 (amount of heterogeneity accounted for):            0.00%
## 
## Test for Residual Heterogeneity: 
## QE(df = 77) = 149.2280, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2,3): 
## QM(df = 2) = 1.4411, p-val = 0.4865
## 
## Model Results:
## 
##                                           se     zval    pval    ci.lb
## intrcpt                       1.1432  0.1731   6.6053  <.0001   0.8040
## Bivalve..GastropodGastropod   0.1126  0.2180   0.5163  0.6057  -0.3147
## Tax.levelSpecies             -0.2358  0.2104  -1.1204  0.2626  -0.6482
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
## R^2 (amount of heterogeneity accounted for):            0.00%
## 
## Test for Residual Heterogeneity: 
## QE(df = 78) = 151.8594, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.2337, p-val = 0.6288
## 
## Model Results:
## 
##                                 se    zval    pval    ci.lb   ci.ub     
## intrcpt             0.9790  0.1741  5.6224  <.0001   0.6377  1.3203  ***
## MultipleStagesTRUE  0.1052  0.2175  0.4835  0.6288  -0.3212  0.5315     
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
## R^2 (amount of heterogeneity accounted for):            0.00%
## 
## Test for Residual Heterogeneity: 
## QE(df = 78) = 151.9811, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.4515, p-val = 0.5016
## 
## Model Results:
## 
##                               se    zval    pval    ci.lb   ci.ub     
## intrcpt           0.9926  0.1311  7.5701  <.0001   0.7356  1.2495  ***
## Global.Regional1  0.1449  0.2156  0.6719  0.5016  -0.2777  0.5674     
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
## tau^2 (estimated amount of residual heterogeneity):     0 (SE = 1.0286)
## tau (square root of estimated tau^2 value):             0
## I^2 (residual heterogeneity / unaccounted variability): 0.00%
## H^2 (unaccounted variability / sampling variability):   1.00
## R^2 (amount of heterogeneity accounted for):            NA%
## 
## Test for Residual Heterogeneity: 
## QE(df = 78) = 5.4190, p-val = 1.0000
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 0.0065, p-val = 0.9356
## 
## Model Results:
## 
##                       se    zval    pval    ci.lb   ci.ub   
## intrcpt   0.9560  0.4591  2.0822  0.0373   0.0561  1.8558  *
## meanDate  0.0007  0.0091  0.0808  0.9356  -0.0172  0.0186   
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
pdf("figure/broad_narrow_rma.pdf", width = 3.7, height = 7.5)
plot_effect_sizes(broadData, rma.model = broad.rma, lab = c("narrow", "broad"))
dev.off()
```

```
## pdf 
##   2
```

```r
plot_effect_sizes(broadData, rma.model = broad.rma, lab = c("narrow", "broad"))
```

![plot of chunk Fig1](figure/Fig1.png) 

```r

habitData$In.text.Citation <- as.character(habitData$In.text.Citation)
habitData$In.text.Citation <- sub("McClure and Bohank", "McClure and Bohonak 1995", 
    habitData$In.text.Citation)
```




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
##                                           se     zval    pval    ci.lb
## Bivalve..GastropodBivalve    -0.1248  0.1468  -0.8507  0.3950  -0.4125
## Bivalve..GastropodGastropod   0.1098  0.3042   0.3609  0.7182  -0.4865
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
## R^2 (amount of heterogeneity accounted for):            0.00%
## 
## Test for Residual Heterogeneity: 
## QE(df = 47) = 119.4409, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 1.9268, p-val = 0.1651
## 
## Model Results:
## 
##                                se     zval    pval    ci.lb   ci.ub   
## intrcpt           -0.0067  0.1425  -0.0473  0.9622  -0.2861  0.2726   
## Global.Regional1  -0.5303  0.3821  -1.3881  0.1651  -1.2791  0.2185   
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
## tau^2 (estimated amount of residual heterogeneity):     0.4312 (SE = 0.1653)
## tau (square root of estimated tau^2 value):             0.6567
## I^2 (residual heterogeneity / unaccounted variability): 58.85%
## H^2 (unaccounted variability / sampling variability):   2.43
## R^2 (amount of heterogeneity accounted for):            0.63%
## 
## Test for Residual Heterogeneity: 
## QE(df = 47) = 114.0683, p-val < .0001
## 
## Test of Moderators (coefficient(s) 2): 
## QM(df = 1) = 1.0323, p-val = 0.3096
## 
## Model Results:
## 
##                        se     zval    pval    ci.lb   ci.ub   
## intrcpt   -0.2089  0.1821  -1.1473  0.2513  -0.5658  0.1480   
## meanDate   0.0015  0.0014   1.0160  0.3096  -0.0014  0.0043   
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
pdf("figure/inf_epi_rma.pdf", width = 3.7, height = 6)
plot_effect_sizes(habitData, meanModel.Epifaunal, lab = c("infauna", "epifauna"))
dev.off()
```

```
## pdf 
##   2
```

```r
plot_effect_sizes(habitData, meanModel.Epifaunal, lab = c("infauna", "epifauna"))
```

![plot of chunk Fig2](figure/Fig2.png) 

```r

# now fix up the labels (spacing and the et al.s)
```



Modeled Results
========================================================

Note, for both models, we fit using centered predictors. Doesn't make a difference to coefficient results, but by making the intercept 0, we get easier to interpret marginal plots
later on.

The Fitted Model for Broad v. Narrow

```r
broadDataExtinction <- broadData[which(!is.na(broadData$BC.extinction.ratePBDB)), 
    ]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$mean_d18O.prok)), 
    ]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$mean_d34S.prok)), 
    ]
broadDataExtinction <- broadDataExtinction[which(!is.na(broadDataExtinction$mean_d13C.prok)), 
    ]

# as we'll be using these predictors later
broadDataExtinction <- within(broadDataExtinction, {
    cent.extinction <- cent(BC.extinction.rate.PBDB3)
    cent.OA <- cent(OA)
    cent.d18O <- cent(mean_d18O.prok)
    cent.d34S <- cent(mean_d34S.prok)
    cent.d13C <- cent(mean_d13C.prok)
    detrend.cent.d18O <- cent(mean_d18O.detrended.prok)
    detrend.cent.d34S <- cent(mean_d34S.detrended.prok)
    detrend.cent.d13C <- cent(mean_d13C.detrended.prok)
    cent.meanDate <- cent(meanDate)
})

covModel.Broad.RMA <- rma(yi = lnorReg, vi = vlnorReg, data = broadDataExtinction, 
    mods = ~cent.extinction + cent.OA + cent.d18O + cent.d34S + cent.d13C)
```

```
## Warning: Studies with NAs omitted from model fitting.
```

```r

covModel.Broad.RMA
```

```
## 
## Mixed-Effects Model (k = 68; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.2717 (SE = 0.1202)
## tau (square root of estimated tau^2 value):             0.5212
## I^2 (residual heterogeneity / unaccounted variability): 42.69%
## H^2 (unaccounted variability / sampling variability):   1.74
## R^2 (amount of heterogeneity accounted for):            5.67%
## 
## Test for Residual Heterogeneity: 
## QE(df = 62) = 106.6651, p-val = 0.0004
## 
## Test of Moderators (coefficient(s) 2,3,4,5,6): 
## QM(df = 5) = 6.1964, p-val = 0.2876
## 
## Model Results:
## 
##                               se     zval    pval    ci.lb   ci.ub     
## intrcpt           1.0191  0.1073   9.4940  <.0001   0.8087  1.2295  ***
## cent.extinction   0.8116  1.2736   0.6373  0.5239  -1.6845  3.3078     
## cent.OA           0.0992  0.3068   0.3234  0.7464  -0.5022  0.7006     
## cent.d18O         0.0160  0.0623   0.2574  0.7968  -0.1060  0.1380     
## cent.d34S        -0.0669  0.0343  -1.9486  0.0513  -0.1341  0.0004    .
## cent.d13C        -0.0685  0.1038  -0.6599  0.5093  -0.2720  0.1350     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

# Also, the model with predictors detrended
covModel.Broad.RMA.detrended <- rma(yi = lnorReg, vi = vlnorReg, data = broadDataExtinction, 
    mods = ~cent.extinction + cent.OA + detrend.cent.d18O + detrend.cent.d34S + 
        detrend.cent.d13C)
```

```
## Warning: Studies with NAs omitted from model fitting.
```

```r

covModel.Broad.RMA.detrended
```

```
## 
## Mixed-Effects Model (k = 68; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.2328 (SE = 0.1110)
## tau (square root of estimated tau^2 value):             0.4825
## I^2 (residual heterogeneity / unaccounted variability): 38.93%
## H^2 (unaccounted variability / sampling variability):   1.64
## R^2 (amount of heterogeneity accounted for):            19.18%
## 
## Test for Residual Heterogeneity: 
## QE(df = 62) = 101.5340, p-val = 0.0011
## 
## Test of Moderators (coefficient(s) 2,3,4,5,6): 
## QM(df = 5) = 10.4940, p-val = 0.0624
## 
## Model Results:
## 
##                                 se     zval    pval    ci.lb   ci.ub     
## intrcpt             0.9923  0.1038   9.5618  <.0001   0.7889  1.1957  ***
## cent.extinction     0.9131  1.2212   0.7477  0.4546  -1.4804  3.3067     
## cent.OA             0.1129  0.2921   0.3864  0.6992  -0.4597  0.6854     
## detrend.cent.d18O   0.2345  0.1258   1.8639  0.0623  -0.0121  0.4812    .
## detrend.cent.d34S  -0.0264  0.0347  -0.7609  0.4467  -0.0945  0.0417     
## detrend.cent.d13C   0.0229  0.1107   0.2073  0.8358  -0.1940  0.2398     
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

write.csv(coef(covModel.Broad.RMA), "./broadCoefTable.csv", row.names = T)
write.csv(coef(covModel.Broad.RMA.detrended), "./broadCoefDetrendedTable.csv", 
    row.names = T)

broadCoefPlot <- coefPlot(covModel.Broad.RMA, robust = F, std = T, num_sds = 2) + 
    coord_flip() + scale_x_discrete(labels = c(expression(delta^13 * C), expression(delta^18 * 
    O), expression(delta^34 * S), "Extinction Rate", "Acidification"), expand = c(0.15, 
    0)) + annotate("text", x = 5, y = -0.2, label = "(a)") + ylim(c(-0.25, 0.25)) + 
    annotate("text", x = 5.6, y = -0.15, label = "Favours\nnarrow") + annotate("text", 
    x = 5.6, y = 0.15, label = "Favours\nbroad")
```


The Fitted Model for Epifauna v. Infauna

```r
habitDataGood <- habitData[which(!(is.na(habitData$BC.extinction.rate.PBDB3))), 
    ]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$lnorReg))), ]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$mean_d18O.prok))), 
    ]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$mean_d34S.prok))), 
    ]
habitDataGood <- habitDataGood[which(!(is.na(habitDataGood$vlnorReg))), ]

# get rid of
levels(habitDataGood$study.ID) <- gsub(" [A-Z]{3}", "", levels(habitDataGood$study.ID))
levels(habitDataGood$study.ID) <- gsub("[A-Z]{3}", "", levels(habitDataGood$study.ID))

# as we'll be using these predictors later
habitDataGood <- within(habitDataGood, {
    cent.extinction <- cent(BC.extinction.rate.PBDB3)
    cent.OA <- cent(OA)
    cent.d18O <- cent(mean_d18O.prok)
    cent.d34S <- cent(mean_d34S.prok)
    cent.meanDate <- cent(meanDate)
    detrend.cent.d18O <- cent(mean_d18O.detrended.prok)
    detrend.cent.d34S <- cent(mean_d34S.detrended.prok)
    detrend.cent.d13C <- cent(mean_d13C.detrended.prok)
})

covModel.Epifaunal.rma <- rma(yi = lnorReg, vi = vlnorReg, data = habitDataGood, 
    mods = ~cent.OA + cent.extinction + cent.d18O + cent.d34S)

covModel.Epifaunal.rma
```

```
## 
## Mixed-Effects Model (k = 41; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.3189 (SE = 0.1620)
## tau (square root of estimated tau^2 value):             0.5647
## I^2 (residual heterogeneity / unaccounted variability): 49.74%
## H^2 (unaccounted variability / sampling variability):   1.99
## R^2 (amount of heterogeneity accounted for):            19.26%
## 
## Test for Residual Heterogeneity: 
## QE(df = 36) = 70.2126, p-val = 0.0006
## 
## Test of Moderators (coefficient(s) 2,3,4,5): 
## QM(df = 4) = 6.0156, p-val = 0.1980
## 
## Model Results:
## 
##                               se     zval    pval    ci.lb   ci.ub   
## intrcpt          -0.0866  0.1357  -0.6380  0.5235  -0.3526  0.1794   
## cent.OA           0.3370  0.3676   0.9168  0.3593  -0.3834  1.0574   
## cent.extinction  -0.4786  1.2825  -0.3731  0.7090  -2.9922  2.0351   
## cent.d18O         0.4175  0.1993   2.0947  0.0362   0.0269  0.8081  *
## cent.d34S        -0.0700  0.0528  -1.3246  0.1853  -0.1735  0.0336   
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

# And now detrended
covModel.Epifaunal.rma.detrend <- rma(yi = lnorReg, vi = vlnorReg, data = habitDataGood, 
    mods = ~cent.OA + cent.extinction + detrend.cent.d18O + detrend.cent.d34S)

covModel.Epifaunal.rma.detrend
```

```
## 
## Mixed-Effects Model (k = 41; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.3078 (SE = 0.1588)
## tau (square root of estimated tau^2 value):             0.5548
## I^2 (residual heterogeneity / unaccounted variability): 48.85%
## H^2 (unaccounted variability / sampling variability):   1.96
## R^2 (amount of heterogeneity accounted for):            22.08%
## 
## Test for Residual Heterogeneity: 
## QE(df = 36) = 67.7970, p-val = 0.0011
## 
## Test of Moderators (coefficient(s) 2,3,4,5): 
## QM(df = 4) = 9.0577, p-val = 0.0597
## 
## Model Results:
## 
##                                 se     zval    pval    ci.lb   ci.ub    
## intrcpt            -0.0588  0.1346  -0.4371  0.6621  -0.3226  0.2049    
## cent.OA             0.0510  0.3819   0.1335  0.8938  -0.6975  0.7995    
## cent.extinction     0.9303  1.3731   0.6775  0.4981  -1.7610  3.6216    
## detrend.cent.d18O   0.5201  0.1937   2.6855  0.0072   0.1405  0.8996  **
## detrend.cent.d34S   0.0550  0.0497   1.1048  0.2693  -0.0425  0.1525    
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

write.csv(coef(covModel.Epifaunal.rma.detrend), "./epiCoefDetrendedTable.csv", 
    row.names = T)
write.csv(coef(covModel.Epifaunal.rma), "./epiCoefTable.csv", row.names = T)


epiCoefPlot <- coefPlot(covModel.Epifaunal.rma, habitDataGood, robust = F, std = T, 
    num_sds = 2) + coord_flip() + scale_x_discrete(labels = c(expression(delta^18 * 
    O), expression(delta^34 * S), "Extinction Rate", "Acidification"), expand = c(0.15, 
    0)) + annotate("text", x = 4, y = -0.4, label = "(b)") + ylim(c(-0.5, 0.5)) + 
    annotate("text", x = 4.6, y = -0.25, label = "Favours\ninfauna") + annotate("text", 
    x = 4.6, y = 0.25, label = "Favours\nepifauna")
```


Does adding Time change things?

```r
rma(yi = lnorReg, vi = vlnorReg, data = broadDataExtinction, mods = ~BC.extinction.ratePBDB + 
    cent.OA + cent.d18O + cent.d34S + cent.d13C + cent.meanDate)
```

```
## 
## Mixed-Effects Model (k = 71; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.2554 (SE = 0.1122)
## tau (square root of estimated tau^2 value):             0.5053
## I^2 (residual heterogeneity / unaccounted variability): 42.08%
## H^2 (unaccounted variability / sampling variability):   1.73
## R^2 (amount of heterogeneity accounted for):            6.24%
## 
## Test for Residual Heterogeneity: 
## QE(df = 64) = 109.9242, p-val = 0.0003
## 
## Test of Moderators (coefficient(s) 2,3,4,5,6,7): 
## QM(df = 6) = 7.8779, p-val = 0.2472
## 
## Model Results:
## 
##                                      se     zval    pval    ci.lb   ci.ub
## intrcpt                  0.9971  0.1345   7.4148  <.0001   0.7335  1.2606
## BC.extinction.ratePBDB   0.7317  1.2474   0.5866  0.5575  -1.7132  3.1766
## cent.OA                  0.0471  0.3225   0.1461  0.8838  -0.5850  0.6793
## cent.d18O                0.1945  0.1255   1.5498  0.1212  -0.0515  0.4405
## cent.d34S               -0.0262  0.0400  -0.6542  0.5130  -0.1047  0.0523
## cent.d13C                0.0048  0.1131   0.0423  0.9662  -0.2168  0.2264
## cent.meanDate            0.0024  0.0016   1.4962  0.1346  -0.0007  0.0055
##                            
## intrcpt                 ***
## BC.extinction.ratePBDB     
## cent.OA                    
## cent.d18O                  
## cent.d34S                  
## cent.d13C                  
## cent.meanDate              
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



```r
rma(yi = lnorReg, vi = vlnorReg, data = habitDataGood, mods = ~cent.OA + cent.extinction + 
    cent.d18O + cent.d34S + cent.meanDate)
```

```
## 
## Mixed-Effects Model (k = 41; tau^2 estimator: REML)
## 
## tau^2 (estimated amount of residual heterogeneity):     0.3419 (SE = 0.1710)
## tau (square root of estimated tau^2 value):             0.5847
## I^2 (residual heterogeneity / unaccounted variability): 51.44%
## H^2 (unaccounted variability / sampling variability):   2.06
## R^2 (amount of heterogeneity accounted for):            13.45%
## 
## Test for Residual Heterogeneity: 
## QE(df = 35) = 67.7996, p-val = 0.0007
## 
## Test of Moderators (coefficient(s) 2,3,4,5,6): 
## QM(df = 5) = 8.8667, p-val = 0.1145
## 
## Model Results:
## 
##                               se     zval    pval    ci.lb   ci.ub   
## intrcpt          -0.0423  0.1401  -0.3019  0.7628  -0.3169  0.2323   
## cent.OA          -0.0769  0.4388  -0.1753  0.8608  -0.9370  0.7832   
## cent.extinction   1.5142  1.7207   0.8800  0.3789  -1.8584  4.8868   
## cent.d18O         0.4808  0.2058   2.3359  0.0195   0.0774  0.8843  *
## cent.d34S         0.1167  0.1181   0.9879  0.3232  -0.1148  0.3483   
## cent.meanDate     0.0078  0.0044   1.7759  0.0758  -0.0008  0.0165  .
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Figure 4
========================================================

```r
grid.arrange(broadCoefPlot + theme_bw(base_size = 18), epiCoefPlot + theme_bw(base_size = 18), 
    ncol = 2)
```

```
## Warning: is.na() applied to non-(list or vector) of type 'expression'
## Warning: is.na() applied to non-(list or vector) of type 'expression'
```

![plot of chunk Fig4](figure/Fig4.png) 


Figure 5
========================================================

```r
#### What are the marginal effects from the model After adjusting for centering
#### the predictor
del18marg <- marginalLine(covModel.Epifaunal.rma, "cent.d18O", habitDataGood, 
    robust = F, xAdd = mean(habitDataGood$mean_d18O.prok)) + xlab("\n Delta O18") + 
    ylab("Component + Residual + Intercept Log Odds\n Ratios for Delta O18\n") + 
    annotate("text", x = -4, y = 2.75, label = "(a)") + theme_bw(base_size = 18) + 
    annotate("text", x = -1, y = -2, label = "Favours\ninfauna") + annotate("text", 
    x = -3.5, y = 2, label = "Favours\nepifauna")

del18MargData <- marginalData(covModel.Epifaunal.rma, "cent.d18O", habitDataGood)
write.csv(del18MargData, "./del18MargData.csv", row.names = F)

# Extract Legend
g_legend <- function(a.gplot) {
    a.gplot <- a.gplot + scale_color_discrete("Study")
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}


# The Figure
fig5a <- del18marg + scale_color_discrete(guide = "none")

legend <- g_legend(del18marg)

# The Detrended Figure
del18margDetrend <- marginalLine(covModel.Epifaunal.rma.detrend, "detrend.cent.d18O", 
    habitDataGood, robust = F, xAdd = mean(habitDataGood$mean_d18O.detrended.prok)) + 
    xlab("\n Detrended Delta O18") + ylab("Component + Residual + Intercept Log Odds\n Ratios for Detrended Delta O18\n") + 
    annotate("text", x = -2.5, y = 3, label = "(b)") + theme_bw(base_size = 18) + 
    annotate("text", x = 2, y = -1.5, label = "Favours\ninfauna") + annotate("text", 
    x = -1.5, y = 2, label = "Favours\nepifauna")

fig5b <- del18margDetrend + scale_color_discrete(guide = "none")

grid.arrange(fig5a, fig5b, legend, widths = c(3, 3, 1), ncol = 3)
```

![plot of chunk Fig5](figure/Fig5.png) 



Appendix Jackknife Figures
========================================================

```
## Error: arguments imply differing number of rows: 68, 71
```

```
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
```

```
## Error: 'data' must be of a vector type, was 'NULL'
```



```r
funnel(broad.rma, main = "Funnel Plot for Broad v. Narrow Analysis")
```

![plot of chunk funnelPlots](figure/funnelPlots1.png) 

```r
funnel(meanModel.Epifaunal, main = "Funnel Plot for Epifauna v. Infauna Analysis")
```

![plot of chunk funnelPlots](figure/funnelPlots2.png) 

```r


scaledat <- function(x) {
    x.scaled <- x/(2 * sd(x, na.rm = TRUE))
    x.scaled
}

broadDataExtinctionScaled <- broadDataExtinction
broadDataExtinctionScaled <- transform(broadDataExtinction, BC.extinction.ratePBDB = scaledat(BC.extinction.ratePBDB), 
    mean_d18O.prok = scaledat(mean_d18O.prok), mean_d34S.prok = scaledat(mean_d34S.prok), 
    mean_d13C.prok = scaledat(mean_d13C.prok))
# sd of OA should already be ~0.5. (actually around 0.42)

covModel.Broad.RMA2.scaled <- rma(yi = lnorReg, vi = vlnorReg, data = broadDataExtinctionScaled, 
    mods = ~BC.extinction.ratePBDB + OA + mean_d18O.prok + mean_d34S.prok + 
        mean_d13C.prok)
# Now for the habit model:
habitDataGoodScaled <- habitDataGood
habitDataGoodScaled <- transform(habitDataGood, BC.extinction.ratePBDB = scaledat(BC.extinction.ratePBDB), 
    mean_d18O.prok = scaledat(mean_d18O.prok), mean_d34S.prok = scaledat(mean_d34S.prok), 
    meanDate = scaledat(meanDate))

covModel.Epifaunal.rma3.scaled <- rma(yi = lnorReg, vi = vlnorReg, data = habitDataGoodScaled, 
    mods = ~OA + BC.extinction.ratePBDB + mean_d18O.prok + mean_d34S.prok + 
        meanDate)

pdf("figure/broad-jackknife.pdf", width = 4, height = 8)
jackknifed_coefs_fun(covModel.Broad.RMA2.scaled, broadDataExtinctionProk, robust = F) + 
    theme_bw() + scale_colour_grey(name = "Study Removed\n") + ylab("Scaled coefficient estimate")
```

```
## Error: object 'broadDataExtinctionProk' not found
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r

# TODO WARNING Error in rma(lnorReg, vi = vlnorReg, data = temp_dat, mods =
# temp_dat[, : Processing terminated since k = 0.
pdf("figure/habit-jackknife.pdf", width = 4, height = 8)
jackknifed_coefs_fun(covModel.Epifaunal.rma3.scaled, habitDataGood, robust = F) + 
    theme_bw() + scale_colour_grey(name = "Study Removed\n") + ylab("Scaled coefficient estimate")
```

```
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
## Using  as id variables
```

```
## Error: 'data' must be of a vector type, was 'NULL'
```

```r
dev.off()
```

```
## pdf 
##   2
```

