## Sub analyses between LTPA and dementia
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
## dementia (overall)
rm(list = setdiff(ls(), keep))

dementia <- getDiseaseSpecificData(data, "dementia", 
                                    paexposure = "LTPA", overall1 = 1)
dementia <- formatData(dementia, kcases = T)
dementia$n <- with(dementia, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases, n
any(is.na(dementia[, c("dose", "rr", "cases", "n")]))

# descriptive plot
ggplot(dementia, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(dementia$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.6, .8, 1, 1.2))

# individual analyses
k_dementia <- quantile(dementia$dose, c(.1, .5, .9))
modi_spl_dementia <- lapply(split(dementia, dementia$id), function(d)
   dosresmeta(logrr ~ rcs(dose, k_dementia), id = id, cases = cases,
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
}, modi_spl_dementia, split(dementia, dementia$id), SIMPLIFY = F)

# pooled analysis
spl_dementia <- dosresmeta(logrr ~ rcs(dose, k_dementia), id = id, cases = cases,
                            n = n, type = type, se = se, data = dementia)
summary(spl_dementia)

newdata <- data.frame(dose = seq(min(dementia$dose), max(dementia$dose), length.out = 100))
ggplot(predict(spl_dementia, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.6, .8, 1)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)

# Comment: I don't observe that much of 'ballooning'.
#        In addition it is based only on 3 studies.


## -----------------------------------------------------------------------------
## dementia (men)
rm(list = setdiff(ls(), keep))

dementia_men <- getDiseaseSpecificData(data, "dementia", 
                                    paexposure = "LTPA", gender = 1)
dementia_men <- formatData(dementia_men, kcases = T)
dementia_men$n <- with(dementia_men, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases, n
any(is.na(dementia_men[, c("dose", "rr", "cases", "n")]))

# descriptive plot
ggplot(dementia_men, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(dementia_men$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.5, .75, 1))

k_dementia_men <- quantile(dementia_men$dose, c(.1, .5, .9))
spl_dementia_men <- dosresmeta(logrr ~ rcs(dose, k_dementia_men), id = id, cases = cases,
                            n = n, type = type, se = se, data = dementia_men)
summary(spl_dementia_men)

newdata <- data.frame(dose = seq(min(dementia_men$dose), max(dementia_men$dose), length.out = 100))
ggplot(predict(spl_dementia_men, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.5, .75, 1, 1.25)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)


# Comment: it's only one study.


## -----------------------------------------------------------------------------
## total dementia (women)
rm(list = setdiff(ls(), keep))

dementia_women <- getDiseaseSpecificData(data, "dementia", 
                                        paexposure = "LTPA", gender = 2)
dementia_women <- formatData(dementia_women, kcases = T)
dementia_women$n <- with(dementia_women, ifelse(effect_measure == "hr", personyears, totalpersons))

# checking NA for rr, dose, and cases,n
any(is.na(dementia_women[, c("dose", "rr", "cases", "n")]))

# descriptive plot
ggplot(dementia_women, aes(dose, rr, group = id, shape = authors)) + 
   scale_shape_manual(values = seq_along(unique(dementia_women$id))) +
   labs(shape = "Authors") +
   geom_line() + geom_point() + theme_classic() + ylab("LnRR") +
   xlab("LTPA") + 
   scale_y_continuous("Hazard Ratio" , trans = "log", 
                      breaks = c(.5, .75, 1))

k_dementia_women <- quantile(dementia_women$dose, c(.25, .5, .75))
spl_dementia_women <- dosresmeta(logrr ~ rcs(dose, k_dementia_women), id = id, cases = cases,
                                n = n, type = type, se = se, data = dementia_women)
summary(spl_dementia_women)

newdata <- data.frame(dose = seq(min(dementia_women$dose), max(dementia_women$dose), length.out = 100))
ggplot(predict(spl_dementia_women, newdata, expo = T), 
       aes(newdata$dose, y = pred, ymin = ci.lb, ymax = ci.ub)) + 
   geom_line() + scale_y_continuous("Hazard Ratio", trans = "log", breaks = c(.4, .6, .8, 1, 1.2)) +
   xlab("LTPA") + theme_classic() + geom_ribbon(alpha = .1)

## Comment: As for men, it's only one study. Changing knots location doesn't have an impact.