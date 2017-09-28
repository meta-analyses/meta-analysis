## Read raw data and run following tests:
# sum of totalpersons should be of order with n_baseline
# 
# sum of tot_personyrs should be of order with person years
# 
# same for cases
# 
# Referrant category should have the lowest dose
# if greater than 10, flag it up
# if greater than 80, flag it up

source("filter_studies.R")

for (i in 1:nrow(uoutcome)){
  acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  
  
  for (j in unique(acmfdata$id)){
    d <- subset(acmfdata, id == j)
    
    total_pyears <- sum(d$personyrs, na.rm = T)
    diff <- 100 - (total_pyears /  unique(d$tot_personyrs) * 100)
    if (!is.na(diff) && diff > 10){
      cat("PYRS ", uoutcome$outcome[i], " ", diff, " ", unique(d$ref_number), "\n")
    }
    
  }
}



for (i in 1:nrow(uoutcome)){
  acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  
  
  for (j in unique(acmfdata$id)){
    d <- subset(acmfdata, id == j)
    
    total_persons <- sum(d$totalpersons, na.rm = T)
    diff <- 100 - (total_persons /  unique(d$n_baseline) * 100)
    if (!is.na(diff) && diff > 10){
      cat("TPRS ", uoutcome$outcome[i], " ", diff, " ", unique(d$ref_number), "\n")
    }
  }
}


for (i in 1:nrow(uoutcome)){
  acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  
  
  for (j in unique(acmfdata$id)){
    d <- subset(acmfdata, id == j)
    
    total_cases <- sum(d$cases, na.rm = T)
    diff <- 100 - (total_cases /  unique(d$tot_cases) * 100)
    if (!is.na(diff) && diff > 10){
      cat("Cases ", uoutcome$outcome[i], " ", diff, " ", unique(d$ref_number), "\n")
    }
  }
}



for (i in 1:nrow(uoutcome)){
  acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  
  
  for (j in unique(acmfdata$id)){
    d <- subset(acmfdata, id == j & rr == 1)
    
    if (!is.na(d) && nrow(d) == 1 && (d$dose > 10)){
      cat("Unexpected high referent  dose ", uoutcome$outcome[i], " ", d$dose, " ", unique(d$ref_number), "\n")
    }
    
    u <- subset(acmfdata, id == j)
    if (max(u$dose) > 80){
      cat("Unexpected high dose ", uoutcome$outcome[i], " ", max(u$dose), " ", unique(d$ref_number), "\n")
    }
  }
}