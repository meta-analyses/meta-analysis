## Sub analyses between LTPA and all_cause mortality
library(ggplot2)
library(readxl)
library(dosresmeta)
library(rms)
library(tidyverse)

# rm (list = ls())
# Read the data
ndata <- read_excel("data/Reviewed_form_harmonised_2020.10.08.xlsx", sheet = 1, skip = 1)

data <- ndata %>% filter(outcome == "All-cause mortality") %>% 
  dplyr::select(ref_id...1, first_author, outcome, outcome_type, pa_domains, n_per_category, person_years_per_category, cases_per_category, 
                effect_measure, m_met_h_wk, most_adj_effect, most_adj_lci, most_adj_uci, mean_follow_up)

data$type <- ""

data[data$effect_measure == "OR",]$type <- "ir" # incidence rate
data[data$effect_measure == "RR",]$type <- "ir" # incidence rate
data[data$effect_measure == "HR",]$type <- "ci" # cumulative incidence
data$type <- as.character(data$type)


data$logrr <- log(data$m_met_h_wk)
data$most_adj_lci <- as.numeric(data$most_adj_lci)
data$se <- with(data, (log(most_adj_uci) - log(most_adj_lci))/(2*qnorm(.975)))


data <- data %>% rename(ref_number = ref_id...1, Author = first_author, 
                        n_baseline = n_per_category, 
                        totalpersons = n_per_category, tot_personyrs = person_years_per_category, 
                        personyrs = person_years_per_category, mean_followup = mean_follow_up, 
                        dose = m_met_h_wk, RR = most_adj_effect , 
                        effect = most_adj_effect, 
                        uci_effect = most_adj_uci, lci_effect = most_adj_lci, 
                        cases = cases_per_category)

source("all-functions.R")



total_population <- T
male_population <- T
female_population <- F
local_last_knot <- 0.75

if (total_population){
  for (i in 1:nrow(uoutcome)){
    i <- 1
    cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- data
    local_cov_method <- F
    if (i == 3 || i == 4) local_cov_method <- T
    if (nrow(acmfdata) > 0){
      # acmfdata <- filter(acmfdata, ref_number == '81')
      acmfdata$effect_measure <- tolower(acmfdata$effect_measure)
      acmfdata$RR <- acmfdata$effect
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
      last_knot <- get_last_knot(acmfdata, dose_pert = local_last_knot , personyrs_pert = local_last_knot)
      # cat("dose_pert and person_years_pert ", last_knot, "\n")
      last_knot <- last_knot[2]
      if (nrow(acmfdata) > 0){
        dataset <- acmfdata
        q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
        if (!is.null(dataset)){
          dataset$personyrs <- round(dataset$personyrs)
          group_by(dataset, id) %>% select(dose, se) %>%
            summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
          pa <- acmfdata
          
          dataset2 <- data.frame(metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = local_cov_method, minQuantile = 0, maxQuantile = last_knot, lout = 1000))
          colnames(dataset2) <- c("dose","RR", "lb", "ub")
          
          #obj <- metaAnalysis(acmfdata, returnval = T, ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
          #dataset2 <- data.frame(dose = obj[1], RR = as.data.frame(obj[2])[1], lb = as.data.frame(obj[2])[2], ub = as.data.frame(obj[2])[3])
          # colnames(dataset2) <- c("dose","RR", "lb", "ub")
          plotTitle <- paste0( uoutcome$outcome[i] ,  " - Total Population")
          plotTitle <-  paste0(simpleCap(plotTitle), 
                               ' \nDose Response Relationship between Physical Activity and All-Cause Mortality [30]',
                               ' \nNumber of people: ' , round(sum(acmfdata$totalpersons)))#, " ", local_last_knot)
          p <- ggplot() +
            #geom_line(data = dataset, aes(dose, RR, col = factor(ref_number), label = personyrs)) +
            #geom_point(data = dataset, aes(dose, RR, col = factor(ref_number)), size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
            geom_line(data = subset(dataset2, dose < 24.2), aes(x = dose, y = RR)) +
            geom_line(data = subset(dataset2, dose >= 24.2), aes(x = dose, y = RR), linetype = "dashed") +
            geom_ribbon(data = subset(dataset2, dose < 24.2), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
            geom_ribbon(data = subset(dataset2, dose >= 24.2), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.10) +
            #geom_vline(xintercept= q, linetype="dotted", alpha = 0.6) +
            scale_x_continuous(expand = c(0, 0),
                               breaks = seq(from = 0, to = 80, by = 10)) + 
            scale_y_continuous(expand = c(0, 0),
                               breaks = seq(from = 0, to = max(dataset2$ub), by = 0.2),
                               limits = c(0, NA)) +
            # coord_cartesian(ylim = c(min(dataset$RR) - 0.2, max(dataset$RR) + 0.2), 
            #                 xlim = c(0, max(dataset$dose) + 3)) + 
            theme(legend.position="none",
                  plot.title = element_text(hjust = 0.5)) +
            xlab("\nMarginal MET hours per week\n") +
            ylab("\nRelative Risk\n") +
            labs(title = paste(plotTitle))
          print(p)
          ggsave(paste0(uoutcome$outcome[i], "-Arem et al", ".png"), height=5, width=10, units='in', dpi=600, scale = 1)
        }
      }
    }
  }
}