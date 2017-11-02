source("filter_studies.R")

total_population <- T
male_population <- F
female_population <- F
local_last_knot <- 0.75

if (total_population){
  for (i in 1:nrow(uoutcome)){
    # for (local_last_knot in c(0.85, 0.85)){
    i <- 1
    cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup) # &
                         # outcome_type == "mortality")
    
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
      
      if (nrow(acmfdata) > 0){
        
        dataset <- acmfdata
        q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
        
        
        if (!is.null(dataset)){
          dataset$personyrs <- round(dataset$personyrs)
          
          group_by(dataset, id) %>% select(dose, se) %>%
            summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
          
          obj <- metaAnalysis(acmfdata, returnval = T, ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot)
          dataset2 <- data.frame(dose = obj[1], RR = as.data.frame(obj[2])[1], lb = as.data.frame(obj[2])[2], ub = as.data.frame(obj[2])[3])
          colnames(dataset2) <- c("dose","RR", "lb", "ub")
          
          
          plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Total Population")

          plotTitle <-  paste0(simpleCap(plotTitle), ' \n Number of samples: ',
                length(unique(acmfdata$id)),
                ' \n Number of people: ' , round(sum(acmfdata$totalpersons)))#, " ", local_last_knot)
          
          
          # gg <- plotly::ggplotly(
          #   ggplot(dataset, aes(dose, rr, col = ref_number, label = personyrs)) + geom_point(size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
          #     geom_line() +
          #     geom_line(data = dataset2) + 
          #     geom_ribbon(data = dataset2, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
          #     #scale_y_continuous(trans = "log", breaks = c(.1, .25, .5, .75, 1, 1.25)) +
          #     scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
          #     coord_cartesian(xlim = c(0, max(dataset$dose))) +
          #     geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
          #     theme_classic() + guides(col = FALSE) + 
          #     xlab("\nMarginal MET hours per week\n") +
          #     ylab("\nRelative Risk\n") +
          #     labs(title = paste("")) +
          #     theme(
          #       plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          #       plot.title = element_text(size = 12, colour = "black", vjust = 7),
          #       plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
          #       legend.direction = "horizontal",
          #       legend.position = c(0.1, 1.05))
          # )
          
          
          p <- ggplot() +
            geom_line(data = dataset, aes(dose, rr, col = factor(ref_number), label = personyrs)) +
            geom_point(data = dataset, aes(dose, rr, col = factor(ref_number)), size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
            # geom_point(data = dataset, aes(dose, rr, col = ref_number, label = personyrs)) + 
            # red plot
            geom_line(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, y = RR)) +
            geom_line(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, y = RR), linetype = "dashed") +
            geom_ribbon(data = dataset2, aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
            geom_vline(xintercept= q, linetype="dotted", alpha = 0.6) +
            scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
            theme(legend.position="none") +
            xlab("\nMarginal MET hours per week\n") +
            ylab("\nRelative Risk\n") +
            labs(title = paste(plotTitle))
          
          print(p)
          
          # ggsave(paste0(uoutcome$outcome[i], ".png"), height=5, width=10, units='in', dpi=600)
          #plotly::ggplotly(p)
          
        }
        
        
        
        # obj <- metaAnalysis(acmfdata, returnval = T, ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot)
        # dataset <- data.frame(dose = obj[1], RR = as.data.frame(obj[2])[1], lb = as.data.frame(obj[2])[2], ub = as.data.frame(obj[2])[3])
        # colnames(dataset) <- c("dose","RR", "lb", "ub")
        # 
        # #print(obj[3], "\n")
        # 
        # plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Total Population")
        # 
        # plotTitle <-  paste0(simpleCap(plotTitle), ' \n Number of samples: ', 
        #       length(unique(acmfdata$id)), 
        #       ' \n Number of people: ' , round(sum(acmfdata$totalpersons)), " ", local_last_knot)
        # q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
        # xlab = "Marginal MET hours per week" 
        # print(
        #   ggplot(dataset, aes(dose, RR)) + 
        #     geom_line(data = dataset) + 
        #     geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
        #     #coord_cartesian(ylim = c(0, 1), xlim = c(0, xMax)) +
        #     scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
        #     # coord_cartesian(xlim = c(0, 70)) +
        #     xlab(paste("\n", xlab, "\n")) +
        #     ylab("\nRelative Risk\n") +
        #     geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
        #     
        #     theme(
        #       plot.margin = unit(c(2, 1, 1, 1), "cm"), 
        #       plot.title = element_text(size = 12, colour = "black", vjust = 7),
        #       plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
        #       legend.direction = "horizontal",
        #       legend.position = c(0.1, 1.05)) + 
        #     labs(title = paste(plotTitle)) #+ labs(fill = "")
        # )
      }
    }
    }
  # }
  
}

if(male_population){
  for (i in 1:nrow(uoutcome)){
    i <- 5
    cat("Male Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    mdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 1)
    wdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 2)
    
    
    if (nrow(mdata) > 0 && nrow(wdata) > 0){
      mdata <- getMissingVariables(mdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      mdata <- subset(mdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      mdata <- subset(mdata, !is.na(rr) & !is.na(dose))
      
      mdata <- subset(mdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
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
      wdata <- subset(wdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                 (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      wdata <- subset(wdata, !is.na(rr) & !is.na(dose))
      
      wdata <- subset(wdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
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
    i <- 1
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
