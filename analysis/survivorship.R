

## @knitr survivorshipPrep
habitDataGood$EpifaunalSurvived <- 100*with(habitDataGood, X..Surv/(Total...Ext + Total...Surv))

habitDataGood$InfaunalSurvived <- 100*with(habitDataGood, (Total...Surv-X..Surv)/(Total...Ext + Total...Surv))

habitDataGoodMelt <- melt.data.frame(habitDataGood, c("number", "study.ID","del.34S","d18OresidualMean","Total...Ext", "Total...Surv"), c("EpifaunalSurvived","InfaunalSurvived"  ))
habitDataGoodMelt <- transform(habitDataGoodMelt, prop_surv = value / 100, tot_obs = Total...Ext + Total...Surv)

#deltaO18Survivorship
qplot(d18OresidualMean, prop_surv, geom="point", data=habitDataGoodMelt, color=study.ID, group=variable) + 
  facet_wrap(~variable) +
  stat_smooth(method="glm", color="black", mapping=aes(weight = tot_obs), family = binomial(link = "logit")) +
  theme_bw() + ylab("Porportion Survived")


#delta 34SSurvivorship
qplot(del.34S, prop_surv, geom="point", data=habitDataGoodMelt, color=study.ID, group=variable) + 
  facet_wrap(~variable) +
  stat_smooth(method="glm", color="black", mapping=aes(weight = tot_obs), family = binomial(link = "logit")) +
  theme_bw() + ylab("Porportion Survived")


# logistic regression:
m_bin_epi_surv <- glm(prop_surv ~ del.34S, data = subset(habitDataGoodMelt, variable == "EpifaunalSurvived"), weights = tot_obs, family = binomial(link = "logit"))
summary(m_bin_epi_surv)

m_bin_inf_surv <- glm(prop_surv ~ del.34S, data = subset(habitDataGoodMelt, variable == "InfaunalSurvived"), weights = tot_obs, family = binomial(link = "logit"))
summary(m_bin_inf_surv)

m_bin_epi_surv_linear <- glm(prop_surv ~ del.34S, data = subset(habitDataGoodMelt, variable == "EpifaunalSurvived"), weights = tot_obs)
summary(m_bin_epi_surv_linear)

m_bin_inf_surv_linear <- glm(prop_surv ~ del.34S, data = subset(habitDataGoodMelt, variable == "InfaunalSurvived"), weights = tot_obs)
summary(m_bin_inf_surv_linear)

#library(arm)
#del.34S.pred <- data.frame(del.34S = seq(min(habitDataGoodMelt$del.34S), max(habitDataGoodMelt$del.34S), length.out = 100))
#epi_surv_pred <- predict(m_bin_epi_surv, se = TRUE, newdata = del.34S.pred)
#l <- invlogit(epi_surv_pred$fit - 1.96 * epi_surv_pred$se.fit)
#u <- invlogit(epi_surv_pred$fit + 1.96 * epi_surv_pred$se.fit)
#epi_surv_pred_df <- data.frame(del.34S = del.34S.pred, l = l, u = u, fit = epi_surv_pred$fit)
#ggplot(habitDataGoodMelt, aes(del.34S, value)) + geom_point(aes(color=study.ID, group=variable)) + 
#facet_wrap(~variable) +
#theme_bw() + ylab("% Survived")
