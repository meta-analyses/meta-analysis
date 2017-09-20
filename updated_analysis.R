source("filter_studies.R")

total_population <- T
male_population <- T
female_population <- T

if (total_population){
  for (i in 1:nrow(uoutcome)){
    i <- 11
    #cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
    
    # Filter studies by study size
    if (i == 1){
      acmfdata <- subset(acmfdata, n_baseline >= 40000)
    }else{
      acmfdata <- subset(acmfdata, n_baseline >= 10000)
    }
    
    local_cov_method <- F
    if (i == 3 || i == 4) local_cov_method <- T
      
    if (nrow(acmfdata) > 0){

      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Subset to selected columns
      acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      top_df <- subset(acmfdata, dose >= (as.numeric(quantile(acmfdata$dose)[4])))
      bottom_df <- setdiff(acmfdata, top_df)
      personyrs_pert <- 0.25
      max_pert <- 0.25
      
      continue <- TRUE
      
      percentile <- ecdf(acmfdata$dose)
      
      while(continue)
      {
        # cat(" sum ", sum(top_df$personyrs, na.rm = T), "\n" )
        if (sum(top_df$totalpersons, na.rm = T) >= (personyrs_pert * sum(acmfdata$totalpersons, na.rm = T)) ){
          
          continue <- FALSE
        }else{
          max_dose <- max(bottom_df$dose)
          row_df <- subset(bottom_df, dose == max_dose)
          # Rmeove the maximum point
          top_df <- rbind(top_df, row_df)
          bottom_df <- subset(bottom_df, dose != max_dose)
          # max_pert <- (min(top_df$dose) / max(acmfdata$dose) * 100)
          #personyrs_pert <- max(bottom_df$dose) / max(acmfdata$dose)
          
          #cat(personyrs_pert, " - ", nrow(top_df), " - ", nrow(bottom_df), "\n")
          
        }
      }
      
      
      cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, " with quantiles ", percentile(max_dose), "\n")
      
      
      #acmfdata <- subset(acmfdata, !is.na(rr))
      
      if (i == 5)
        acmfdata <- subset(acmfdata, !is.na(rr))
      
      if (local_pa_domain_subgroup == "TPA" && (i == 4 || i == 10)){
        acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$uci_effect <- acmfdata[acmfdata$logrr == 0,]$lci_effect <- 0
      }

      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste0( uoutcome$outcome[i] , " (", local_pa_domain_subgroup,") ", " - Total Population"),
                     covMethed = local_cov_method, maxQuantile = percentile(max_dose))
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
