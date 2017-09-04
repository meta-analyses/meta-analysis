source("init.R")

total_population <- T
male_population <- T
female_population <- T

if (total_population){
  for (i in 1:nrow(uoutcome)){
    i <- 5
    cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & (overall == 1 | sex_subgroups == 3))
    if (nrow(acmfdata) > 0){

      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Subset to selected columns
      acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      if (local_pa_domain_subgroup == "TPA" && (i == 4 || i == 10)){
        acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$uci_effect <- acmfdata[acmfdata$logrr == 0,]$lci_effect <- 0
      }
      
      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste0( uoutcome$outcome[i] , " (", local_pa_domain_subgroup,") ", " - Total Population"), covMethed = T, minQuantile = 0, maxQuantile = 0.75)
      }
    }
  }
  
}

if(male_population){
  for (i in 1:nrow(uoutcome)){
    #i <- 11
    cat("Male Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 1)
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      if (local_pa_domain_subgroup == "TPA" && (i == 11) )# Remove duplicate rows for ref_number 73
        acmfdata <- acmfdata[!duplicated(acmfdata[c("id", "dose")]),]
      
      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Male Population"), covMethed = T, minQuantile = 0, maxQuantile = 0.75)
      }
    }
  }
}

if(female_population){
  for (i in 1:nrow(uoutcome)){
    i <- 4
    cat("Female Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 2)
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))      
      
      
      if (local_pa_domain_subgroup == "TPA" && (i == 4 || i == 10))
        acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$uci_effect <- acmfdata[acmfdata$logrr == 0,]$lci_effect <- 0
      
      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Female Population"), covMethed = T, minQuantile = 0, maxQuantile = 0.65)
      }
    }
  }
}