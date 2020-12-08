source("filter_studies.R")
## Not using any self functions
mdata <- subset(raw_data_gsp_ltpa, outcome == "stroke" & pa_domain_subgroup == "LTPA" & sex_subgroups == 1)
mdata <- getMissingVariables(mdata, infertotalpersons = T, kcases = T)

# Remove when totalperson is not available for hr, and personsyears for rr/or
mdata <- subset(mdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                 (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))

# Remove where both dose and response are null
mdata <- subset(mdata, !is.na(rr) & !is.na(dose))

mdata <- subset(mdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))



for (k in seq(from = 0.85, to = 1, by = 0.05)){
  for (i in c(1,2)){
    g <- "Male"
    if (i == 2)
      g <- "Female"
    acmfdata <- subset(raw_data_gsp_ltpa, outcome == "stroke" & pa_domain_subgroup == "LTPA" & sex_subgroups == i)
    
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      last_knot <- get_last_knot(acmfdata, dose_pert = k, personyrs_pert = k)
      cat("dose_pert and person_years_pert ", last_knot, "\n")
      
      last_knot <- last_knot[2]
      
      if (nrow(acmfdata) > 0){
        
        dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, 
                                           ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot))
        colnames(dataset) <- c("dose","RR", "lb", "ub")
        
        plotTitle <- paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - ", g, " Population")
        
        
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
    