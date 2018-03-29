source("filter_studies.R")
# Load dosresmeta library
require(dosresmeta)
# Load rms library
require(rms)

local_cov <- F
local_last_knot <- 0.75
otype <- c("mortality", "incidence")
for (i in 1:nrow(uoutcome)){
  for (ot in otype){
    # i <- 5
    acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == "LTPA" & outcome_type == ot) #"All-cause mortality"
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) |
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      # Filter studies by study size
      acmfdata <- subset(acmfdata, n_baseline >= 10000)
      if (uoutcome$outcome[i] == "Coronary heart disease")
        local_last_knot <- 0.77
      else
        local_last_knot <- 0.75
      last_knot <- get_last_knot(acmfdata, personyrs_pert = local_last_knot, dose_pert = local_last_knot)
      last_knot <- last_knot[2]
      
      if (uoutcome$outcome[i] == "Coronary heart disease" || uoutcome$outcome[i] == "Cardiovascular disease" || uoutcome$outcome[i] == "Stroke")
        local_cov <- T
      else
        local_cov <- F
      
      k <- quantile(acmfdata$dose, c(0, (last_knot) / 2, last_knot))
      # Create a dosresmeta obj
      obj <- dosresmeta(logrr ~ rcs(dose,k), cases = cases, n = ifelse(effect_measure == "hr", personyrs, totalpersons), 
                        type = type, se = se, id = id,  
                        # proc = "2stage",
                        covariance = ifelse(local_cov, "h", "gl"),
                        data = acmfdata)
      if (obj$proc == "2stage"){
        cat("Outcome: ", uoutcome$outcome[i], " - ", ot,  "\n")
        qt <- qtest(obj)
        Q <- formatC(qt$Q, digits = 3, format = "f")
        pvalue <- formatC(qt$pvalue, digits = 3, format = "f")
        i2 <- formatC(pmax((qt$Q - qt$df)/qt$Q * 100,
                           0), digits = 1, format = "f")
        cat("Univariate ", "Cochran Q-test for ", "residual ",
            "heterogeneity:", "\n", sep = "")
        cat("Q = ", Q[1], " (df = ", qt$df[1], "), p-value = ",
            pvalue[1], "\n", sep = "")
        cat("I-square statistic = ", i2[1], "%", "\n\n", sep = "")
      }
    }
    
  }
  
}


local_cov <- F
local_last_knot <- 0.75
gender <- c(1, 2)

for (i in 1:nrow(uoutcome)){
  for (ot in otype){
    for (g in gender){
      acmfdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & 
                           pa_domain_subgroup == "LTPA" & 
                           outcome_type == ot & sex_subgroups == g)
      if (nrow(acmfdata) > 0){
        acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
        # Remove when totalperson is not available for hr, and personsyears for rr/or
        acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) |
                                         (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
        if (uoutcome$outcome[i] == "Coronary heart disease")
          local_last_knot <- 0.77
        else
          local_last_knot <- 0.75
        
        
        last_knot <- get_last_knot(acmfdata, personyrs_pert = local_last_knot, dose_pert = local_last_knot)
        last_knot <- last_knot[2]
        
        if (uoutcome$outcome[i] == "Coronary heart disease" || uoutcome$outcome[i] == "Cardiovascular disease" || uoutcome$outcome[i] == "Stroke")
          local_cov <- T
        else
          local_cov <- F
        
        k <- quantile(acmfdata$dose, c(0, (last_knot) / 2, last_knot))
        # Create a dosresmeta obj
        obj <- dosresmeta(logrr ~ rcs(dose,k), cases = cases, n = ifelse(effect_measure == "hr", personyrs, totalpersons), 
                          type = type, 
                          lb = lci_effect,
                          ub = uci_effect,
                          id = id,  
                          # proc = "2stage",
                          covariance = ifelse(local_cov, "h", "gl"),
                          data = acmfdata)
        cat(ifelse(g  == 1 ,"Males - ", "Females - "), "Outcome: ", uoutcome$outcome[i], " - ", ot,  "\n")
        if (obj$proc == "2stage"){
          # cat(ifelse(g  == 1 ,"Males - ", "Females - "), "Outcome: ", uoutcome$outcome[i], " - ", ot,  "\n")
          qt <- qtest(obj)
          Q <- formatC(qt$Q, digits = 3, format = "f")
          pvalue <- formatC(qt$pvalue, digits = 3, format = "f")
          i2 <- formatC(pmax((qt$Q - qt$df)/qt$Q * 100,
                             0), digits = 1, format = "f")
          cat("Univariate ", "Cochran Q-test for ", "residual ",
              "heterogeneity:", "\n", sep = "")
          cat("Q = ", Q[1], " (df = ", qt$df[1], "), p-value = ",
              pvalue[1], "\n", sep = "")
          cat("I-square statistic = ", i2[1], "%", "\n\n", sep = "")
        }
      }
      
    }
  }
}
