source("filter_studies.R")
acmfdata <- subset(raw_data_tp_ltpa, outcome == "All-cause mortality" & pa_domain_subgroup == "LTPA")
acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
# Remove when totalperson is not available for hr, and personsyears for rr/or
acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) |
                                 (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
# Filter studies by study size
acmfdata <- subset(acmfdata, n_baseline >= 10000)
last_knot <- get_last_knot(acmfdata, personyrs_pert = 0, dose_pert = 0.75)
last_knot <- last_knot[2]

# Load dosresmeta library
require(dosresmeta)
# Create a dosresmeta obj
obj <- dosresmeta(logrr ~ dose, cases = cases, n = ifelse(effect_measure == "hr", personyrs, totalpersons), 
                  type = type, se = se, id = id,  
                  proc = "2stage",
                  data = acmfdata)

# Print summary
summary(obj)
