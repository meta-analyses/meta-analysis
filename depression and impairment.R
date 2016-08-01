## Sub analyses between LTPA and depression / cognitive impairment
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
## depression (overall)
rm(list = setdiff(ls(), keep))

depression <- getDiseaseSpecificData(data, "depression", 
                                    paexposure = "LTPA", overall1 = 1)
depression <- formatData(depression, kcases = T)
depression$n <- with(depression, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases, n
any(is.na(depression[, c("dose", "rr", "cases", "n")]))

# descriptive plot
ggplot(depression, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(depression$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", breaks = c(.6, .8, 1, 1.2))

# individual analyses
k_depression <- quantile(depression$dose, c(.1, .5, .9))
modi_spl_depression <- lapply(split(depression, depression$id), function(d)
   dosresmeta(logrr ~ rcs(dose, k_depression), id = id, cases = cases,
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
}, modi_spl_depression, split(depression, depression$id), SIMPLIFY = F)

# pooled analysis
spl_depression <- dosresmeta(logrr ~ rcs(dose, k_depression), id = id, cases = cases,
                            n = n, type = type, se = se, data = depression,
                            method = "reml")
summary(spl_depression)

newdata <- data.frame(dose = seq(min(depression$dose), 30, length.out = 100))
ggplot(predict(spl_depression, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.7, .8, 1, 1.2)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)

# Comment: The shape is not that odd. Again it is related to the uncertainty in the
#          estimate of the between-study heterogeneity. It disappears when fitting a
#          fixed-effects model.


## -----------------------------------------------------------------------------
## Cognitive impairment (women)
rm(list = setdiff(ls(), keep))

cognimp_women <- getDiseaseSpecificData(data, "cognitive impairment", 
                                        paexposure = "LTPA", gender = 2)
cognimp_women <- formatData(cognimp_women, kcases = T)
cognimp_women$n <- with(cognimp_women, ifelse(effect_measure == "hr", personyears, totalpersons))


# checking NA for rr, dose, and cases,n
any(is.na(cognimp_women[, c("dose", "rr", "cases", "n")]))

# descriptive plot
ggplot(cognimp_women, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(cognimp_women$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.5, .75, 1))

# individual analyses
k_cognimp_women <- quantile(cognimp_women$dose, c(.1, .5, .9))
modi_spl_cognimp_women <- lapply(split(cognimp_women, cognimp_women$id), function(d)
   dosresmeta(logrr ~ rcs(dose, k_cognimp_women), id = id, cases = cases,
              n = n, type = type, se = se, data = d))

# graphical prediction with model data (only for a descriptive point of view)
par(mfrow = c(2, 1), bty = "n", las = 1)
mapply(function(spl, d){
   with(d, errbar(dose, rr, lci, uci, add = F, log = "y", lty = 1, 
                  xlab = "LTPA", ylab = "Hazard Ratio"))
   newdata <- data.frame(dose = seq(min(d$dose), max(d$dose), length.out = 50))
   with(predict(spl, newdata, xref = d$dose[is.na(d$se)], expo = T), {
      matlines(newdata$dose, cbind(pred, ci.lb, ci.ub),  
               col = "black", lty = c(1, 2, 2))
   })
   title(d$authors[1])
}, modi_spl_cognimp_women, split(cognimp_women, cognimp_women$id), SIMPLIFY = F)

# pooled analysis
spl_cognimp_women <- dosresmeta(logrr ~ rcs(dose, k_cognimp_women), id = id, cases = cases,
                                n = n, type = type, se = se, data = cognimp_women)
summary(spl_cognimp_women)

newdata <- data.frame(dose = seq(min(cognimp_women$dose), max(cognimp_women$dose), length.out = 100))
ggplot(predict(spl_cognimp_women, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.2, .5, 1, 2, 5)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)

# Comment: Only two studies with different LPTA range