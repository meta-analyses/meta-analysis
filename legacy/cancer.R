## Sub analyses between LTPA and cancer
library(ggplot2)
library(dosresmeta)
library(rms)

rm(list = ls())
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
## total cancer (overall)
rm(list = setdiff(ls(), keep))

cancer <- getDiseaseSpecificData(data, "total cancer", 
                                    paexposure = "LTPA", overall1 = 1)
cancer <- formatData(cancer, kcases = T)
cancer$n <- with(cancer, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases, n
any(is.na(cancer[, c("dose", "rr", "cases", "n")]))
cancer$n[is.na(cancer$n)] <- cancer$totalpersons[is.na(cancer$n)]

# descriptive plot
ggplot(cancer, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(cancer$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.6, .8, 1, 1.2))

# individual analyses
k_cancer <- quantile(cancer$dose, c(.1, .5, .9))
modi_spl_cancer <- lapply(split(cancer, cancer$id), function(d)
   dosresmeta(logrr ~ rcs(dose, k_cancer), id = id, cases = cases,
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
}, modi_spl_cancer, split(cancer, cancer$id), SIMPLIFY = F)

# pooled analysis
spl_cancer <- dosresmeta(logrr ~ rcs(dose, k_cancer), id = id, cases = cases,
                            n = n, type = type, se = se, data = cancer)
summary(spl_cancer)

newdata <- data.frame(dose = seq(min(cancer$dose), max(cancer$dose), length.out = 100))
ggplot(predict(spl_cancer, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.85, .9, .95, 1)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)

# Comment: There is not that much of an increase. It levels out if you use
#          25th, 50th, 75th percentiles. The differences, however, are not substantial


## -----------------------------------------------------------------------------
## total cancer (men)
rm(list = setdiff(ls(), keep))

cancer_men <- getDiseaseSpecificData(data, "total cancer", 
                                    paexposure = "LTPA", gender = 1)
cancer_men <- formatData(cancer_men, kcases = T)
cancer_men$n <- with(cancer_men, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases, n
any(is.na(cancer_men[, c("dose", "rr", "cases", "n")]))

# descriptive plot
ggplot(cancer_men, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(cancer_men$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.7, .8, .9, 1))

# individual analyses
k_cancer_men <- quantile(cancer_men$dose, c(.1, .5, .9))
modi_spl_cancer_men <- lapply(split(cancer_men, cancer_men$id), function(d)
   dosresmeta(logrr ~ rcs(dose, k_cancer_men), id = id, cases = cases,
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
}, modi_spl_cancer_men, split(cancer_men, cancer_men$id), SIMPLIFY = F)

# pooled analysis
spl_cancer_men <- dosresmeta(logrr ~ rcs(dose, k_cancer_men), id = id, cases = cases,
                            n = n, type = type, se = se, data = cancer_men,
                            method = "reml")
summary(spl_cancer_men)

newdata <- data.frame(dose = seq(min(cancer_men$dose), max(cancer_men$dose), length.out = 100))
ggplot(predict(spl_cancer_men, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.7, .8, 1)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)

# Comment: As for all-cause mortality overall, this behaviour disappears in a
#          fixed-effects model. But in this case it is not that evident.


## -----------------------------------------------------------------------------
## total cancer (women)
rm(list = setdiff(ls(), keep))

cancer_women <- getDiseaseSpecificData(data, "total cancer", 
                                        paexposure = "LTPA", gender = 2)
cancer_women <- formatData(cancer_women, kcases = T)
cancer_women$n <- with(cancer_women, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases,n
any(is.na(cancer_women[, c("dose", "rr", "cases", "n")]))

# descriptive plot
ggplot(cancer_women, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(cancer_women$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.6, .8, 1))

# individual analyses
k_cancer_women <- quantile(cancer_women$dose, c(.25, .5, .75))
modi_spl_cancer_women <- lapply(split(cancer_women, cancer_women$id), function(d)
   dosresmeta(logrr ~ rcs(dose, k_cancer_women), id = id, cases = cases,
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
}, modi_spl_cancer_women, split(cancer_women, cancer_women$id), SIMPLIFY = F)

# pooled analysis
spl_cancer_women <- dosresmeta(logrr ~ rcs(dose, k_cancer_women), id = id, cases = cases,
                                n = n, type = type, se = se, data = cancer_women)
summary(spl_cancer_women)

newdata <- data.frame(dose = seq(min(cancer_women$dose), max(cancer_women$dose), length.out = 100))
ggplot(predict(spl_cancer_women, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.8, 1, 1.2, 1.4)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)

# Comment: As in all-cause mortality female, the curve is not sensible to the knots location.
#         Apparently it's in the observed data. Actually there are some
#         J-shapes (individual graphs). Anyway there is a lot of uncertainty and not
#         so many studies. You may also limit the pooled curve to 20 LTPA, where
#         there is the majority of the data points.