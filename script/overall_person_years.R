source("script/filter_studies.R")

local_physical_outcomes <- uoutcome %>% filter(!outcome %in% c("All-cause dementia", 
                                                               "Alzheimer's disease", 
                                                               "Major depression", 
                                                               "Parkinson's disease", 
                                                               "Vascular dementia"))
local_outcome_type <- 'Both'

dir_name <- "Fatal and non-fatal"

acmfdata <- subset(raw_data_tp_ltpa,
                     pa_domain_subgroup == local_pa_domain_subgroup &
                     outcome_type == local_outcome_type)

# Add additional "fatal" studies that had no "both" types
if (local_outcome_type == "Both") {
  # Subset fatal types
  add_fdata <- subset(raw_data_tp_ltpa,
                        pa_domain_subgroup == local_pa_domain_subgroup & 
                        outcome_type == "Fatal")
  # ONLY add those studies that have no "both" studies
  add_fdata <- subset(add_fdata, !id %in% acmfdata$id)
  # Add additional rows
  if (nrow(add_fdata) > 0) {
    acmfdata <- rbind(acmfdata, add_fdata)
  }
  
  # Subset Non-fatal types
  add_nfdata <- subset(raw_data_tp_ltpa,
                         pa_domain_subgroup == local_pa_domain_subgroup & 
                         outcome_type == "Non-fatal")
  
  # ONLY add those studies that have no "both" studies
  add_nfdata <- subset(add_nfdata, !id %in% acmfdata$id)
  # Add additional rows
  if (nrow(add_nfdata) > 0) {
    acmfdata <- rbind(acmfdata, add_nfdata)
  }
}

print(length(unique(acmfdata$ref_number)))

if (nrow(acmfdata) > 0) {
  # Fill missing values by inferring to useful columns
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = TRUE, kcases = FALSE)
  
  tpyears <- sum(acmfdata$personyrs, na.rm = T)
  
  lpyears <- acmfdata %>% filter(dose <= 17.5) %>% summarise(lp = sum(personyrs)) %>% mutate(percentage = round(lp/tpyears * 100, 1)) %>% dplyr::select(percentage) %>% as.numeric()
  hpyears <- acmfdata %>% filter(dose <= 35) %>% summarise(lp = sum(personyrs)) %>% mutate(percentage = round(lp/tpyears * 100, 1)) %>% dplyr::select(percentage) %>% as.numeric()
  
}
  
