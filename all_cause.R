## Sub analyses between LTPA and all_cause mortality
library(ggplot2)
library(dosresmeta)
library(rms)

rm (list = ls())
# Read the data
data <- read.csv("data/09.06_COMBINED DATASET REDUCED.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

data$tot_personyrs <- as.numeric(data$tot_personyrs)
data[(is.na(data$tot_personyrs)),]$tot_personyrs <- 
   data[(is.na(data$tot_personyrs)),]$mean_followup * data[(is.na(data$tot_personyrs)),]$n_baseline
data[(is.na(data$mean_followup)),]$mean_followup <- 
   data[(is.na(data$mean_followup)),]$tot_personyrs / data[(is.na(data$mean_followup)),]$n_baseline
data$outcome <- trimws(data$outcome)

# Read all the functions
source("all-functions.R")
keep <- c("keep", ls())

## -----------------------------------------------------------------------------
## All-cause mortality (overall)
rm(list = setdiff(ls(), keep))

all_cause <- getDiseaseSpecificData(data, "all-cause mortality", 
                                    paexposure = "LTPA", overall1 = 1)
all_cause <- formatData(all_cause, kcases = T)
all_cause$n <- with(all_cause, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases,n
any(is.na(all_cause[, c("dose", "rr", "cases", "personyears")]))

# descriptive plot
ggplot(all_cause, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(all_cause$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.5, .75, 1))

# individual analyses
k_all_cause <- quantile(all_cause$dose, c(.25, .5, .75))
modi_spl_all_cause <- lapply(split(all_cause, all_cause$id), function(d)
   dosresmeta(logrr ~ rcs(dose, k_all_cause), id = id, cases = cases,
              n = n, type = type, se = se, data = d))

# graphical prediction with model data (only for a descriptive point of view)
par(mfrow = c(2, 2), bty = "n", las = 1)
mapply(function(spl, d){
   with(d, errbar(dose, rr, lci, uci, add = F, log = "y", lty = 1, 
                  xlab = "LTPA", ylab = "Hazard Ratio"))
   newdata <- data.frame(dose = seq(min(d$dose), max(d$dose), length.out = 50))
   with(predict(spl, newdata, xref = d$dose[is.na(d$se)], expo = T), {
      matlines(newdata$dose, cbind(pred, ci.lb, ci.ub),  
      col = "black", lty = c(1, 2, 2))
   })
   title(d$authors[1])
}, modi_spl_all_cause, split(all_cause, all_cause$id), SIMPLIFY = F)

# pooled analysis
spl_all_cause <- dosresmeta(logrr ~ rcs(dose, k_all_cause), id = id, cases = cases,
                            n = n, type = type, se = se, data = all_cause)
summary(spl_all_cause)

newdata <- data.frame(dose = seq(min(all_cause$dose), 50, length.out = 100))
ggplot(predict(spl_all_cause, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.6, .8, 1)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)

# Comment: From the (first) descriptive, the distribution of
#          the LPTA is very skewed (only 3 points after 40 LPTA), so fixing the last
#          knot at the 90th has an impact in this case. Using the 75th percentile
#          gives different results. I think this sensibility should be mentioned.
#          In addition, limit the pooled curve to a 'reasonable' value of LPTA (50 or lower).



## -----------------------------------------------------------------------------
## All-cause mortality (men)
rm(list = setdiff(ls(), keep))

all_cause_men <- getDiseaseSpecificData(data, "all-cause mortality", 
                                    paexposure = "LTPA", gender = 1)
all_cause_men <- formatData(all_cause_men, kcases = T)
all_cause_men$n <- with(all_cause_men, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases,n
any(is.na(all_cause_men[, c("dose", "rr", "cases", "personyears")]))

# descriptive plot
ggplot(all_cause_men, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(all_cause_men$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.6, .8, 1))

# individual analyses
k_all_cause_men <- quantile(all_cause_men$dose, c(.1, .5, .9))
modi_spl_all_cause_men <- lapply(split(all_cause_men, all_cause_men$id), function(d)
   dosresmeta(logrr ~ rcs(dose, k_all_cause_men), id = id, cases = cases,
              n = n, type = type, se = se, data = d))
# checking the covariance matrices (doesn't seem to be any 'problem')
#lapply(modi_spl_all_cause_men, function(m) cov2cor(m$Slist[[1]]))

# graphical prediction with model data (only for a descriptive point of view)
par(mfrow = c(2, 2), bty = "n", las = 1)
mapply(function(spl, d){
   with(d, errbar(dose, rr, lci, uci, add = F, log = "y", lty = 1, 
                  xlab = "LTPA", ylab = "Hazard Ratio"))
   newdata <- data.frame(dose = seq(min(d$dose), max(d$dose), length.out = 50))
   with(predict(spl, newdata, xref = d$dose[is.na(d$se)], expo = T), {
      matlines(newdata$dose, cbind(pred, ci.lb, ci.ub),  
               col = "black", lty = c(1, 2, 2))
   })
   title(d$authors[1])
}, modi_spl_all_cause_men, split(all_cause_men, all_cause_men$id), SIMPLIFY = F)

# pooled analysis
spl_all_cause_men <- dosresmeta(logrr ~ rcs(dose, k_all_cause_men), id = id, cases = cases,
                            n = n, type = type, se = se, data = all_cause_men,
                            method = "fixed")
summary(spl_all_cause_men)

newdata <- data.frame(dose = seq(min(all_cause_men$dose), max(all_cause_men$dose), length.out = 100))
ggplot(predict(spl_all_cause_men, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.75, .85, 1)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)


# Comment: The analysis is based on only 3 studies; hence the uncertainty in the estimates for
#          the between-studies heterogeneity is very high. This is the main reason for
#          the strange behavior (mainly related to correlation of the two spline coefficients).
#          A fixed-effects model does not have that 'problem'. Of course the heterogeneity
#          is substantial, and a fixed-effects model is hard to be supported.


## -----------------------------------------------------------------------------
## All-cause mortality (women)
rm(list = setdiff(ls(), keep))

all_cause_women <- getDiseaseSpecificData(data, "all-cause mortality", 
                                        paexposure = "LTPA", gender = 2)
all_cause_women <- formatData(all_cause_women, kcases = T)
all_cause_women$n <- with(all_cause_women, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases,n
any(is.na(all_cause_women[, c("dose", "rr", "cases", "personyears")]))

# descriptive plot
ggplot(all_cause_women, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(all_cause_women$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.6, .8, 1))

# individual analyses
k_all_cause_women <- quantile(all_cause_women$dose, c(.25, .5, .75))
modi_spl_all_cause_women <- lapply(split(all_cause_women, all_cause_women$id), function(d)
   dosresmeta(logrr ~ rcs(dose, k_all_cause_women), id = id, cases = cases,
              n = n, type = type, se = se, data = d))

# graphical prediction with model data (only for a descriptive point of view)
par(mfrow = c(2, 2), bty = "n", las = 1)
mapply(function(spl, d){
   with(d, errbar(dose, rr, lci, uci, add = F, log = "y", lty = 1, 
                  xlab = "LTPA", ylab = "Hazard Ratio"))
   newdata <- data.frame(dose = seq(min(d$dose), max(d$dose), length.out = 50))
   with(predict(spl, newdata, xref = d$dose[is.na(d$se)], expo = T), {
      matlines(newdata$dose, cbind(pred, ci.lb, ci.ub),  
               col = "black", lty = c(1, 2, 2))
   })
   title(d$authors[1])
}, modi_spl_all_cause_women, split(all_cause_women, all_cause_women$id), SIMPLIFY = F)

# pooled analysis
spl_all_cause_women <- dosresmeta(logrr ~ rcs(dose, k_all_cause_women), id = id, cases = cases,
                                n = n, type = type, se = se, data = all_cause_women)
summary(spl_all_cause_women)

newdata <- data.frame(dose = seq(min(all_cause_women$dose), max(all_cause_women$dose), length.out = 100))
ggplot(predict(spl_all_cause_women, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.6, .8, 1)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)

# Comment: In this case the knots location does not have an impact. There is a slight
#          increase in the risk, but also a lot of uncertainty (i.e. not significant)