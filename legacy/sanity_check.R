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
  acmfdata$ref_number <- sapply(strsplit(acmfdata$ref_number," "), `[`, 1)
  cat(length(unique(acmfdata$id)), " ", length(unique(acmfdata$Study)), "\n")
  td <- acmfdata %>% select(ref_number, Study) %>% distinct()
  #
  td <- td %>% group_by(Study) %>% filter( n() > 1 )
  if (nrow(td) > 0){
    td$outcome <- uoutcome$outcome[i]
    td$population <- "total population"

    write.table(td, "test.csv", sep = ",", col.names = F, row.names = F, append = T)
  }
  
  # i <- 1
  acmfdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & 
                       pa_domain_subgroup == local_pa_domain_subgroup & 
                       sex_subgroups == 1)
  
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  acmfdata[acmfdata$ref_number == "18 -1",]$Study <- "EPIC-Spain"
  if (!is.null(acmfdata) && nrow(acmfdata) > 0){
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
    acmfdata$ref_number <- sapply(strsplit(acmfdata$ref_number," "), `[`, 1)
    
    cat(uoutcome$outcome[i], " " , length(unique(acmfdata$id)), " ", length(unique(acmfdata$Study)), "\n")
    td <- acmfdata %>% select(ref_number, Study) %>% distinct()
    
    td <- td %>% group_by(Study) %>% filter( n() > 1 )
    if (nrow(td) > 0){
      cat("male population for ", uoutcome$outcome[i], "\n")
      td$outcome <- uoutcome$outcome[i]
      td$population <- "male population"
    }
  }
  
  acmfdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 2)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  if (!is.null(acmfdata) && nrow(acmfdata) > 0){
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) |
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    acmfdata$ref_number <- sapply(strsplit(acmfdata$ref_number," "), `[`, 1)
    cat(length(unique(acmfdata$id)), " ", length(unique(acmfdata$Study)), "\n")
    td <- acmfdata %>% select(ref_number, Study) %>% distinct()

    td <- td %>% group_by(Study) %>% filter( n() > 1 )
    if (nrow(td) > 0){
      td$outcome <- uoutcome$outcome[i]
      td$population <- "female population"
      write.table(td, "test.csv", sep = ",", col.names = F, row.names = F, append = T)
    }
  }
}

## Identify studies with 10 or greater than 10 referent dose

for (i in 1:nrow(uoutcome)){
  # i <- 1
  require(dplyr)
  acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  
  
  for (j in unique(acmfdata$id)){
    # j <- 1
    d <- subset(acmfdata, id == j & (is.na(se))) # %>% select(dose) %>% as.numeric()
    # cat("Referent dose: ", uoutcome$outcome[i], " ", d, " ", unique(d$ref_number), "\n")
    
    if (as.numeric(d$dose) > 8){
      cat("Referent dose: ", d$dose, " for ", uoutcome$outcome[i], " with ref_number ", unique(d$ref_number), "\n")
    }
    
  }
}


for (i in 1:nrow(uoutcome)){
  # i <- 1
  acmfdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  # acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  if (!is.null(acmfdata) && nrow(acmfdata) > 0){
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    #acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) |
    #                                 (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    acmfdata$ref_number <- sapply(strsplit(acmfdata$ref_number," "), `[`, 1)
    m_pop <- filter(acmfdata, sex_subgroups == 1)
    w_pop <- filter(acmfdata, sex_subgroups == 2)
    
    for (j in unique(m_pop$ref_number)){
      # j <- "13"
      m_pop_nb <- filter(m_pop, ref_number == j) %>% select(n_baseline) %>% distinct()
      w_pop_nb <- filter(m_pop, ref_number == j) %>% select(n_baseline) %>% distinct()
      
      if ( (m_pop_nb + m_pop_nb) < 1000){
        cat(uoutcome$outcome[i], " - ", j , " \n")
      }
      
    }
    
    # # cat(length(unique(acmfdata$id)), " ", length(unique(acmfdata$Study)), "\n")
    # for (j in unique(acmfdata$ref_number)){
    #   td <- filter(acmfdata, ref_number == j)
    # 
    #   if (length(unique(td$sex_subgroups)) != 2){
    #     cat(uoutcome$outcome[i], " with sex group ", unique(td$sex_subgroups), "\n")
    # 
    #   }
    # }
  }
}

if (T){
  for (i in 3){#1:nrow(uoutcome)){
    for (j in seq(from = 0.75, to = 1, by = 0.01)){
      # i <- 3
      # j <- 0.85
      cat("Total Population - Outcome: ", uoutcome$outcome[i], " and j ", j, "\n")
      acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup &
                           outcome_type == "mortality")
      
      acmfdata <- subset(acmfdata, n_baseline >= 10000)
      
      local_cov_method <- F
      if (nrow(acmfdata) > 0){
        acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
        # Remove when totalperson is not available for hr, and personsyears for rr/or
        acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
        # Subset to selected columns
        acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
        last_knot <- get_last_knot(acmfdata, dose_pert = j , personyrs_pert = j)
        cat("dose_pert and person_years_pert ", last_knot, "\n")
        last_knot <- last_knot[2]
        if (nrow(acmfdata) > 0){
          dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot))
          if (nrow(dataset) > 0){
            colnames(dataset) <- c("dose","RR", "lb", "ub")
            q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
            
            plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Total Population")
            
            plotTitle <-  paste0(simpleCap(plotTitle), ' \n Number of samples: ', 
                                 length(unique(acmfdata$id)), 
                                 ' \n Number of people: ' , round(sum(acmfdata$totalpersons)), " ", j)
            
            cat(plotTitle, "\n")
            
            # xlab = "Marginal MET hours per week" 
            # print(
            #   ggplot(dataset, aes(dose, RR)) +
            #   geom_line(data = dataset) +
            #   geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
            #   scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
            #   xlab(paste("\n", xlab, "\n")) +
            #   ylab("\nRelative Risk\n") +
            #   geom_vline(xintercept= q, linetype="dotted", alpha=0.4) +
            #   labs(title = paste(plotTitle))
            # )
          }
        }
      }
      
    }
  }
}

## 0.77 for CHD total population with outcome_type = mortality




for (g in c(1,2)){# male and female
  for (i in 9){#stroke
    for (j in seq(from = 0.75, to = 1, by = 0.01)){
      gender <- "Male"
      if (g == 2)
        gender <- "Female"
      
      cat(gender, " Population - Outcome: ", uoutcome$outcome[i], " and j ", j, "\n")
      acmfdata <- subset(raw_data_gsp_ltpa, 
                         outcome == uoutcome$outcome[i] & 
                         pa_domain_subgroup == "LTPA" & 
                         sex_subgroups == g)
      
      local_cov_method <- F
      if (nrow(acmfdata) > 0){
        acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
        # Remove when totalperson is not available for hr, and personsyears for rr/or
        acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                         (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
        # Subset to selected columns
        acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
        last_knot <- get_last_knot(acmfdata, dose_pert = j , personyrs_pert = j)
        cat("dose_pert and person_years_pert ", last_knot, "\n")
        last_knot <- last_knot[2]
        if (nrow(acmfdata) > 0){
          dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot))
          if (nrow(dataset) > 0){
            colnames(dataset) <- c("dose","RR", "lb", "ub")
            q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
            gender <- "Male"
            if (g == 2)
              gender <- "Female"
            
            plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - ", gender, "Population")
            
            plotTitle <-  paste0(simpleCap(plotTitle), ' \n Number of samples: ', 
                                 length(unique(acmfdata$id)), 
                                 ' \n Number of people: ' , round(sum(acmfdata$totalpersons)), " ", j)
            
            cat(plotTitle, "\n")
            
            # xlab = "Marginal MET hours per week" 
            # print(
            #   ggplot(dataset, aes(dose, RR)) +
            #   geom_line(data = dataset) +
            #   geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
            #   scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
            #   xlab(paste("\n", xlab, "\n")) +
            #   ylab("\nRelative Risk\n") +
            #   geom_vline(xintercept= q, linetype="dotted", alpha=0.4) +
            #   labs(title = paste(plotTitle))
            # )
          }
        }
      }
      
    }
  }
}

# .83 for male population and .85 for female population



for (g in c(2)){# female
  for (ut in c("all", "incidence")){
    for (j in seq(from = 0.75, to = 1, by = 0.01)){
      i <- 4 #colon cancer
      gender <- "Male"
      if (g == 2)
        gender <- "Female"
      cat(gender, " Population - Outcome: ", uoutcome$outcome[i], " type ", ut , " and j ", j, "\n")
      if (ut == "all"){
        
        
        acmfdata <- subset(raw_data_gsp_ltpa, 
                           outcome == uoutcome$outcome[i] & 
                             pa_domain_subgroup == "LTPA" & 
                             sex_subgroups == g)
        
        
      }else{
        
        acmfdata <- subset(raw_data_gsp_ltpa, 
                           outcome == uoutcome$outcome[i] & 
                             pa_domain_subgroup == "LTPA" & 
                             sex_subgroups == g &
                             outcome_type == "incidence")
        
        
      }
      
      local_cov_method <- F
      if (nrow(acmfdata) > 0){
        acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
        # Remove when totalperson is not available for hr, and personsyears for rr/or
        acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                         (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
        # Subset to selected columns
        acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
        last_knot <- get_last_knot(acmfdata, dose_pert = j , personyrs_pert = j)
        
        last_knot <- last_knot[2]
        if (nrow(acmfdata) > 0){
          dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot))
          if (nrow(dataset) > 0){
            colnames(dataset) <- c("dose","RR", "lb", "ub")
            q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
            gender <- "Male"
            if (g == 2)
              gender <- "Female"
            
            plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - ", gender, "Population")
            
            plotTitle <-  paste0(simpleCap(plotTitle), ' \n Number of samples: ', 
                                 length(unique(acmfdata$id)), 
                                 ' \n Number of people: ' , round(sum(acmfdata$totalpersons)), " ", j)
            
            # cat("dose_pert and person_years_pert ", last_knot, "\n")
            
            # cat(plotTitle, "\n")
            
            # xlab = "Marginal MET hours per week" 
            # print(
            #   ggplot(dataset, aes(dose, RR)) +
            #   geom_line(data = dataset) +
            #   geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
            #   scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
            #   xlab(paste("\n", xlab, "\n")) +
            #   ylab("\nRelative Risk\n") +
            #   geom_vline(xintercept= q, linetype="dotted", alpha=0.4) +
            #   labs(title = paste(plotTitle))
            # )
          }
        }
      }
      
    }
  }
}

# Female  Population - Outcome:  colon cancer  type  all  and j  0.78 
# Female  Population - Outcome:  colon cancer  type  incidence  and j  0.78






