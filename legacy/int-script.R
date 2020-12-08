## Sub analyses between LTPA and all_cause mortality
library(ggplot2)
library(readxl)
library(dosresmeta)
library(rms)
library(tidyverse)

rm (list = ls())
# Read the data
ndata <- read_excel("data/Reviewed_form_harmonised_2020.10.08.xlsx", sheet = 1, skip = 1)

data <- ndata %>% filter(outcome == "All-cause mortality")

data <- data %>% dplyr::select(ref_id...1, first_author, outcome, n_per_category, person_years_per_category, cases_per_category, effect_measure, m_met_h_wk, most_adj_effect, most_adj_lci, most_adj_uci)

data <- data %>% rename(ref_number = ref_id...1, Author = first_author)
raw_data <- subset(raw_data, select = c(ref_number, Author, outcome, outcome_type, pa_domain_subgroup, overall, sex_subgroups, 
                                        effect_measure, type, n_baseline, totalpersons, tot_personyrs, personyrs,
                                        mean_followup, dose, RR, effect, uci_effect, lci_effect, cases))