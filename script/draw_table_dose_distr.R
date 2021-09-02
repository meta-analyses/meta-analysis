## Initialize global vars
local_last_knot <- 0.75
ALT <- FALSE
NO_BMI_EFFECT <- FALSE

# Initialize datasets
source("script/filter_studies.R")

# Load additional library
library(broom)

# Create weighted dose by using dose and total persons
raw_data_gsp_ltpa$weighted_dose <- (raw_data_gsp_ltpa$totalpersons * raw_data_gsp_ltpa$dose) / 
  max(raw_data_gsp_ltpa$totalpersons, na.rm = T)

# Remove rows with NA weighted_dose
raw_data_gsp_ltpa <- raw_data_gsp_ltpa %>% filter(!is.na(weighted_dose))

# Create quantiles and save as CSV
raw_data_gsp_ltpa %>% 
  mutate(sex = case_when(sex_subgroups == 2 ~ 'female', 
                         TRUE ~ 'male')) %>% group_by(sex) %>%
  do({x <- .$weighted_dose
  map_dfr(.x = c(0, 0.025, .05, 0.25, .5, .75, 0.95, 0.975, 1),
          .f = ~ data_frame(Quantile = .x,
                            Value = round(quantile(x, probs = .x), 2)))
  }) %>% 
  pivot_wider(names_from = Quantile, values_from = c(Value) ) %>% 
  readr::write_csv('data/csv/distr/dose_distr_by_sex.csv')