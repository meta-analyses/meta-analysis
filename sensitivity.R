library(dosresmeta)
library(rms)
library(tidyverse)
library(plotly)
library(directlabels)
library(gridExtra)


## Call init to load all data and functions
source("init.R")

# Remove all studies with less than 40k n_baseline for all-cause mortality, and 10k for the rest
raw_data <- subset(raw_data, (outcome == 'all-cause mortality' & n_baseline >= 40000) | (outcome != 'all-cause mortality' & n_baseline >= 1000))

## -----------------------------------------------------------------------------
## select data of interest

i <- 5
cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & (overall == 1 | sex_subgroups == 3))
acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
# Remove when totalperson is not available for hr, and personsyears for rr/or
acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                 (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))

## some descriptives
length(unique(acmfdata$id))
table(acmfdata$id)
group_by(acmfdata, id) %>% select(dose, se) %>%
  summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
ggplotly(
  ggplot(acmfdata, aes(dose, rr, col = factor(id))) + geom_point() +
    geom_line() +
    scale_y_continuous(trans = "log", breaks = c(.1, .25, .5, .75, 1, 1.25)) +
    theme_classic() + guides(col = FALSE)
)


## -----------------------------------------------------------------------------
## individual curves

# choose the knot you're interested
last_k <- .75
k <- quantile(acmfdata$dose, c(0, last_k/2, last_k))
spli <- lapply(split(acmfdata, acmfdata$id), function(d)
  dosresmeta(logrr ~ rcs(dose, k), id = id, type = type, se = se, cases = cases,
             n = ifelse(effect_measure == "hr", personyrs, totalpersons),
             data = d))
pi <- Map(function(d, m){
  data.frame(dose = c(d$dose[is.na(d$se)], seq(min(d$dose), max(d$dose), length.out = 100))) %>%
    bind_cols(predict(m, newdata = ., expo = T)) %>%
    ggplot(aes(x = dose)) + 
    geom_line(aes(y = pred)) + geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = .2) +
    geom_errorbar(data = d, aes(dose, ymin = lci_effect, ymax = uci_effect)) +
    scale_y_continuous(trans = "log", breaks = seq(.25, 4, .25)) +
    theme_classic() + labs(title = paste("Study ID", d$id[1])) +
    theme(plot.title = element_text(hjust = 0.5))
}, split(acmfdata, acmfdata$id), spli)
ml <- marrangeGrob(pi, ncol = 3, nrow = 3)
ml
#ggsave("individual_curve.pdf", ml, height = 12, width = 10)


## -----------------------------------------------------------------------------
## dose-response model

last_k <- .75
k <- quantile(acmfdata$dose, c(0, last_k/2, last_k))
spl <- dosresmeta(logrr ~ rcs(dose, k), id = id, type = type, se = se, 
                  cases = cases, n = ifelse(effect_measure == "hr", personyrs, totalpersons),
                  data = acmfdata)
#summary(spl)

newd <- data.frame(dose = seq(0, max(acmfdata$dose), length.out = 100))
newd %>%
  bind_cols(predict(spl, newdata = newd, exp = T)) %>%
  ggplot(aes(dose, pred, ymin = ci.lb, ymax = ci.ub)) + geom_line() +
  geom_ribbon(alpha = .1) +
  scale_y_continuous(trans = "log", breaks = seq(.25, 2, .25)) +
  theme_classic()


## -----------------------------------------------------------------------------
## sensitivity analysis: knots location

last_k <- seq(.5, .9, by = .05)
klist <- lapply(as.list(last_k), function(p)
  quantile(acmfdata$dose, c(0, p/2, p))
)
names(klist) <- paste0(100*last_k, "%")
spl_k <- lapply(klist, function(k)
  dosresmeta(logrr ~ rcs(dose, k), id = id, type = type, se = se, 
             cases = cases, n = ifelse(effect_measure == "hr", personyrs, totalpersons),
             data = acmfdata)
)
pred_k <- cbind(newd, do.call("cbind", lapply(spl_k, function(m)
  predict(m, newdata = newd, exp = T)$pred)))
colnames(pred_k)[-1] <- round(sapply(klist, function(k) k[3]), 0)
range <- 2:(length(last_k) + 1)
pred_k <- gather(pred_k, last_knot, pred, 2:10)

ggplot(pred_k, aes(dose, pred, label = last_knot, col = last_knot)) + 
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label = last_knot), method = list(dl.trans(x = x + .6), 'last.bumpup', hjust = 1)) +
  labs(x = "MET-minutes/week", y = "Hazard Ratio", title = "Different knots location") +   theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


## -----------------------------------------------------------------------------
## sensitivity analysis: excluding high exposures (doses > 40)

## study ID 2 needs to be rearranged with the referent being the first category
ps <- dosresmeta:::change_ref(logrr, se^2, cases, totalpersons, "ci", 
                              data = subset(acmfdata, id == 2), 1, method = "hamling",
                              expo = F)
ps$se <- ps$v.1^.5
acmfdata[acmfdata$id == 2, c("cases", "totalpersons", "logrr", "se")] <- ps[, c("A.1", "N.1", "logrr.1", "se")]

maxdose <- 40
table(acmfdata$dose >= maxdose)
acmfdata5 <- subset(acmfdata, dose < maxdose)
table(acmfdata5$id)

last_k5 <- .75
k5 <- quantile(acmfdata5$dose, c(0, last_k5/2, last_k5))
spl5 <- dosresmeta(logrr ~ rcs(dose, k5), id = id, type = type, se = se, 
                   cases = cases, n = ifelse(effect_measure == "hr", personyrs, totalpersons),
                   data = acmfdata)
#summary(spl5)

newd5 <- subset(newd, dose < maxdose)
newd5 %>%
  bind_cols(predict(spl5, newdata = newd5, exp = T)) %>%
  ggplot(aes(dose, pred, ymin = ci.lb, ymax = ci.ub)) + geom_line() +
  geom_ribbon(alpha = .1) +
  scale_y_continuous(trans = "log", breaks = seq(.25, 2, .25)) +
  theme_classic()



## -----------------------------------------------------------------------------
## Sensitivity: Leave-one-out

last_k <- .75
k <- quantile(acmfdata$dose, c(0, last_k/2, last_k))
spl_noi <- lapply(as.list(unique(acmfdata$id)), function(i)
  dosresmeta(logrr ~ rcs(dose, k), id = id, type = type, se = se, 
             cases = cases, n = ifelse(effect_measure == "hr", personyrs, totalpersons),
             data =  subset(acmfdata, id != i)))

pred_noi <- cbind(newd, do.call("cbind", lapply(spl_noi, function(m)
  predict(m, newdata = newd, exp = T)$pred)))
colnames(pred_noi)[-1] <- unique(acmfdata$id)
pred_noi <- gather(pred_noi, sensitivity, pred, `1`:`20`)

ggplot(pred_noi, aes(dose, pred, label = sensitivity, col = sensitivity)) + 
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label = sensitivity), method = list(dl.trans(x = x + .7), 'last.bumpup', hjust = 1)) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))
