## Initialize global vars
local_last_knot <- 0.75
ALT <- FALSE
NO_BMI_EFFECT <- FALSE

# Initialize datasets
source("script/filter_studies.R")

# Load additional library
library(broom)

raw_data_gsp_ltpa %>% 
  mutate(sex = case_when(sex_subgroups == 2 ~ 'female', 
                         TRUE ~ 'male')) %>% group_by(sex) %>%
  do({x <- .$dose
  map_dfr(.x = c(0, 0.025, .05, 0.25, .5, .75, 0.95, 0.975, 1),
          .f = ~ data_frame(Quantile = .x,
                            Value = quantile(x, probs = .x)))
  }) %>% readr::write_csv('data/csv/distr/dose_distr_by_sex.csv')