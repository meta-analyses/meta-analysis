source("filter_studies.R")

total_population <- F
male_population <- F
female_population <- T

local_last_knot <- 0.75

if (total_population){
  for (i in 1:nrow(uoutcome)){
    # i <- 1
    cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
    
    acmfdata <- subset(acmfdata, n_baseline >= 10000)
    
    # # Filter studies by study size
    # if (i == 1){
    #   acmfdata <- subset(acmfdata, n_baseline >= 40000)
    # }else{
    #   acmfdata <- subset(acmfdata, n_baseline >= 10000)
    # }
    
    local_cov_method <- F
    if (i == 3 || i == 4) local_cov_method <- T
      
    if (nrow(acmfdata) > 0){

      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Subset to selected columns
      acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      last_knot <- get_last_knot(acmfdata, dose_pert = local_last_knot , personyrs_pert = local_last_knot)
      cat("dose_pert and person_years_pert ", last_knot, "\n")
      
      last_knot <- last_knot[2]
      
      #windows()
      #acmfdata <- subset(acmfdata, !is.na(rr))
      
      if (i == 5)
        acmfdata <- subset(acmfdata, !is.na(rr))
      
      if (local_pa_domain_subgroup == "TPA" && (i == 4 || i == 10)){
        acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$uci_effect <- acmfdata[acmfdata$logrr == 0,]$lci_effect <- 0
      }
      
      # Remove ref_number 146 from i == 1
      if (i == 1){
        acmfdata <- filter(acmfdata, ref_number != 146)
      }
      
      # acmfdata[acmfdata$id == 23 & acmfdata$logrr == 0, ]$lci_effect <- NA
      # acmfdata[acmfdata$id == 23 & acmfdata$logrr == 0, ]$uci_effect <- NA
      # acmfdata[acmfdata$id == 23 & acmfdata$logrr == 0, ]$se <- NA

      if (nrow(acmfdata) > 0){
        
        dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, 
                                           ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot))
        colnames(dataset) <- c("dose","RR", "lb", "ub")
        
        plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Total Population")
        
        
        plotTitle <-  paste0(simpleCap(plotTitle), ' \n Number of samples: ', 
              length(unique(acmfdata$id)), 
              ' \n Number of people: ' , round(sum(acmfdata$totalpersons)))
        
        q<- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
        
        xlab = "Marginal MET hours per week" 
        #windows()
        print(
          ggplot(dataset, aes(dose, RR)) + 
            geom_line(data = dataset) + 
            geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
            #coord_cartesian(ylim = c(0, 1), xlim = c(0, xMax)) +
            scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
            # coord_cartesian(xlim = c(0, 70)) +
            xlab(paste("\n", xlab, "\n")) +
            ylab("\nRelative Risk\n") +
            geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
            
            theme(
              plot.margin = unit(c(2, 1, 1, 1), "cm"), 
              plot.title = element_text(size = 12, colour = "black", vjust = 7),
              plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
              legend.direction = "horizontal",
              legend.position = c(0.1, 1.05)) + 
            labs(title = paste(plotTitle)) #+ labs(fill = "")
        )
        
        # metaAnalysis(acmfdata, ptitle = paste0( uoutcome$outcome[i] , " (", local_pa_domain_subgroup,") ", " - Total Population"),
        #              covMethed = T, maxQuantile = last_knot)
      }
    }
  }
  
}

if(male_population){
  for (i in 1:nrow(uoutcome)){
    # i <- 9
    cat("Male Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 1)
    
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      last_knot <- get_last_knot(acmfdata, dose_pert = local_last_knot, personyrs_pert = local_last_knot)
      cat("dose_pert and person_years_pert ", last_knot, "\n")
      
      # local_personyrs_pert <- 0.25
      # if (i == 4 || i == 9)
      #   local_personyrs_pert <- 0.1
      # 
      # last_knot <- get_last_knot(acmfdata, personyrs_pert = local_personyrs_pert)
      # 
      # cat(last_knot, "\n")
      
      last_knot <- last_knot[2]
      
      
      if (local_pa_domain_subgroup == "TPA" && (i == 10) )# Remove duplicate rows for ref_number 73
        acmfdata <- acmfdata[!duplicated(acmfdata[c("id", "dose")]),]
      
      if (nrow(acmfdata) > 0){
        
        dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, 
                                                        ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot))
        colnames(dataset) <- c("dose","RR", "lb", "ub")
        
        plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Male Population")
        
        plotTitle <-  paste0(simpleCap(plotTitle), ' \n Number of samples: ', 
              length(unique(acmfdata$id)), 
              ' \n Number of people: ' , round(sum(acmfdata$totalpersons)))
        
        
        
        q<- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
        
        xlab = "Marginal MET hours per week" 
        #windows()
        print(
        ggplot(dataset, aes(dose, RR)) + 
          geom_line(data = dataset) + 
          geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
          #coord_cartesian(ylim = c(0, 1), xlim = c(0, xMax)) +
          scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
          # coord_cartesian(xlim = c(0, 70)) +
          xlab(paste("\n", xlab, "\n")) +
          ylab("\nRelative Risk\n") +
          geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
          
          theme(
            plot.margin = unit(c(2, 1, 1, 1), "cm"), 
            plot.title = element_text(size = 12, colour = "black", vjust = 7),
            plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
            legend.direction = "horizontal",
            legend.position = c(0.1, 1.05)) + 
          labs(title = paste(plotTitle)) #+ labs(fill = "")
        )
        
      }
    }
  }
}

if(female_population){
  for (i in c(1:10)){#nrow(uoutcome)){
    i <- 4
    cat("Female Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 2)
    
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      last_knot <- get_last_knot(acmfdata, dose_pert = local_last_knot, personyrs_pert = local_last_knot)
      cat("dose_pert and person_years_pert ", last_knot, "\n")
      
      last_knot <- last_knot[2]
      
      
      if (local_pa_domain_subgroup == "TPA" && (i == 4 || i == 10))
        acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$uci_effect <- acmfdata[acmfdata$logrr == 0,]$lci_effect <- 0
      
      if (nrow(acmfdata) > 0){
        dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, 
                                           ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot))
        colnames(dataset) <- c("dose","RR", "lb", "ub")
        
        plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Female Population")
        
        plotTitle <-  paste0(simpleCap(plotTitle), ' \n Number of samples: ', 
              length(unique(acmfdata$id)), 
              ' \n Number of people: ' , round(sum(acmfdata$totalpersons)))
        
        q<- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
        
        xlab = "Marginal MET hours per week" 
        #windows()
        print(
          ggplot(dataset, aes(dose, RR)) + 
            geom_line(data = dataset) + 
            geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
            #coord_cartesian(ylim = c(0, 1), xlim = c(0, xMax)) +
            scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
            # coord_cartesian(xlim = c(0, 70)) +
            xlab(paste("\n", xlab, "\n")) +
            ylab("\nRelative Risk\n") +
            geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
            
            theme(
              plot.margin = unit(c(2, 1, 1, 1), "cm"), 
              plot.title = element_text(size = 12, colour = "black", vjust = 7),
              plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
              legend.direction = "horizontal",
              legend.position = c(0.1, 1.05)) + 
            labs(title = paste(plotTitle)) #+ labs(fill = "")
        )
        
      }
    }
  }
}
