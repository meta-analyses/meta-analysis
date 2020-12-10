source("script/filter_studies.R")

total_population <- T
male_population <- F
female_population <- F
local_last_knot <- 0.75

if (total_population){
  for (i in 1:nrow(uoutcome)){
    for (local_outcome_type in c('Fatal', 'Non-fatal')){
      if (!i %in% c(1:3)){
        dir_name <- ifelse(local_outcome_type == 'Fatal', 'mortality', 'incidence')
        # dir_name <- 'Fatal'
        # i <- 3
        cat("Total Population - Outcome: ", uoutcome$outcome[i], " , outcome type ", 
            dir_name, " and index ", i, "\n")
        acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & outcome_type == "Fatal")
        acmfdata <- subset(acmfdata, n_baseline >= 10000)
        local_cov_method <- T
        # if (i == 3 || i == 4) local_cov_method <- T
        if (nrow(acmfdata) > 0){
          acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
          acmfdata <- subset(acmfdata, !((effect == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                           (effect != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
          acmfdata <- subset(acmfdata, select = c(id, first_author, effect_measure, outcome_type, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
          last_knot <- get_last_knot(acmfdata, dose_pert = local_last_knot , personyrs_pert = local_last_knot)
          # cat("dose_pert and person_years_pert ", last_knot, "\n")
          last_knot <- last_knot[2]
          if (nrow(acmfdata) > 0){
            dataset <- acmfdata
            
            q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
            if (!is.null(dataset)){
              dataset$personyrs <- round(dataset$personyrs)
              group_by(dataset, id) %>% select(dose, se) %>%
                summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
              pa <- acmfdata
              
              local_filter <- dataset %>% group_by(id) %>% summarise(c = sum(is.na(se))) %>% filter(c > 1) %>% dplyr::select(id)
              # print(dataset %>% group_by(id) %>% summarise(c = sum(is.na(se))) %>% filter(c > 1) %>% dplyr::select(id))
              ld <- NULL
              if (nrow(local_filter) > 0){
                ld <- dataset %>% filter(!id %in% local_filter)
              }else{
                ld <- dataset
              }
              
              dataset2 <- data.frame(metaAnalysis(ld, ptitle = "", returnval = T, covMethed = T, minQuantile = 0, maxQuantile = last_knot, lout = 1000))
              
              # for (local_id in unique(dataset$id)){
              #   # problematic id for i = 1 is 19
              #   # problematic id for i = 2 is 24
              #   ld <- dataset %>% filter(id != 40)
              #   print(local_id)
              #   
              # }
              colnames(dataset2) <- c("dose","RR", "lb", "ub")
              
              plotTitle <- paste0( uoutcome$outcome[i] ,  " - ", simpleCap(dir_name), " - Total Population")
              plotTitle <-  paste0(simpleCap(plotTitle), ' \nNumber of entries: ',
                                   length(unique(acmfdata$id)),
                                   ' \nNumber of people: ' , round(sum(acmfdata$totalpersons, na.rm = T)))#, " ", local_last_knot)
              p <- ggplot() +
                geom_line(data = dataset, aes(dose, RR, col = factor(id), label = personyrs)) +
                geom_point(data = dataset, aes(dose, RR, col = factor(id)), size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
                geom_line(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, y = RR)) +
                geom_line(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, y = RR), linetype = "dashed") +
                geom_ribbon(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
                geom_ribbon(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.10) +
                geom_vline(xintercept= q, linetype="dotted", alpha = 0.6) +
                scale_x_continuous(expand = c(0, 0),
                                   breaks = seq(from = 0, to = 80, by = 10)) + 
                scale_y_continuous(expand = c(0, 0),
                                   breaks = seq(from = 0, to = max(dataset2$ub), by = 0.2),
                                   limits = c(0, NA)) +
                theme(legend.position="none",
                      plot.title = element_text(hjust = 0.5)) +
                xlab("\nMarginal MET hours per week\n") +
                ylab("\nRelative Risk\n") +
                labs(title = paste(plotTitle))
              print(p)
              ggsave(paste0('plots/', dir_name, '/', uoutcome$outcome[i], "-", dir_name, ".png"), height=5, width=10, units='in', dpi=600, scale = 1)
            }
          }
        }
      }
    }
  }
}

if(male_population){
  for (i in 1:nrow(uoutcome)){
    # i <- 5
    cat("Male Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    mdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 1)
    wdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 2)
    
    
    if (nrow(mdata) > 0 && nrow(wdata) > 0){
      mdata <- getMissingVariables(mdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      mdata <- subset(mdata, !((effect == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      mdata <- subset(mdata, !is.na(RR) & !is.na(dose))
      
      mdata <- subset(mdata, select = c(id, ref_number, effect, outcome_type, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
      
      m_last_knot <- get_last_knot(mdata, dose_pert = local_last_knot, personyrs_pert = local_last_knot)
      cat("dose_pert and person_years_pert ", m_last_knot, "\n")
      
      # local_personyrs_pert <- 0.25
      # if (i == 4 || i == 9)
      #   local_personyrs_pert <- 0.1
      # 
      # last_knot <- get_last_knot(mdata, personyrs_pert = local_personyrs_pert)
      # 
      # cat(last_knot, "\n")
      
      m_last_knot <- m_last_knot[2]
      
      
      
      wdata <- getMissingVariables(wdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      wdata <- subset(wdata, !((effect == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                 (effect != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      wdata <- subset(wdata, !is.na(RR) & !is.na(dose))
      
      wdata <- subset(wdata, select = c(id, ref_number, effect, outcome_type, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
      
      w_last_knot <- get_last_knot(wdata, dose_pert = local_last_knot, personyrs_pert = local_last_knot)
      cat("dose_pert and person_years_pert ", w_last_knot, "\n")
      
      
      # if (local_pa_domain_subgroup == "TPA" && (i == 10) )# Remove duplicate rows for ref_number 73
      #   mdata <- mdata[!duplicated(mdata[c("id", "dose")]),]
      
      if (nrow(mdata) > 0){
        
        dataset <- data.frame(metaAnalysis(mdata, returnval = T, 
                                                        ptitle = "", covMethed = F, minQuantile = 0, maxQuantile = last_knot))
        colnames(dataset) <- c("dose","RR", "lb", "ub")
        
        plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Male Population")
        
        plotTitle <-  paste0(simpleCap(plotTitle), ' \n Number of samples: ', 
              length(unique(mdata$id)), 
              ' \n Number of people: ' , round(sum(mdata$totalpersons)))
        
        
        
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
    # i <- 1
    gender <- 1
    gender_title <- "Male"
    
    if (gender == 2){
      gender_title <- "Female"
      
    }
    
    cat(gender_title, " Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    
    # acmfdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & 
    #                      pa_domain_subgroup == local_pa_domain_subgroup & 
    #                      sex_subgroups == 2 &
    #                      outcome_type == "incidence")
    
    
    acmfdata <- subset(raw_data_gsp_ltpa, 
                       outcome == uoutcome$outcome[i] & 
                       pa_domain_subgroup == "LTPA" & 
                       sex_subgroups == gender)
    
    # acmfdata <- subset(acmfdata, n_baseline >= 10000)
    
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      acmfdata <- subset(acmfdata, !is.na(RR) & !is.na(dose))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect, outcome_type, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
      
      last_knot <- get_last_knot(acmfdata, dose_pert = local_last_knot, personyrs_pert = local_last_knot)
      cat("dose_pert and person_years_pert ", last_knot, "\n")
      
      last_knot <- last_knot[2]
      
      
      if (local_pa_domain_subgroup == "TPA" && (i == 4 || i == 10))
        acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$uci_effect <- acmfdata[acmfdata$logrr == 0,]$lci_effect <- 0
      
      if (nrow(acmfdata) > 0){
        dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, 
                                           ptitle = "", covMethed = F, minQuantile = 0, maxQuantile = last_knot))
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
