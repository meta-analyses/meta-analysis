source("script/filter_studies.R")

total_population <- T
local_last_knot <- 0.75

if (total_population){
  for (i in 1:nrow(uoutcome)){
    # for (local_outcome_type in c('Fatal')){
    for (local_outcome_type in c('Fatal', 'Non-fatal', 'Both')){
      if (i %in% c(3) & local_outcome_type == 'Fatal'){
        next
      }
      if (i %in% c(3, 18, 19, 20) & local_outcome_type == 'Both')
        next
      
      if (local_outcome_type == 'Fatal'){
        dir_name <- 'mortality'
      }
      else if (local_outcome_type == 'Non-fatal'){
        dir_name <- 'incidence'
      }
      else{
        dir_name <- 'both'
        
      }
      # dir_name <- 'Fatal'
      # i <- 1
      cat("Total Population - Outcome: ", uoutcome$outcome[i], " , outcome type ", 
          dir_name, " and index ", i, "\n")
      
      acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & outcome_type == local_outcome_type)
      
      if (local_outcome_type == 'Both'){
        add_fdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & outcome_type == 'Fatal')
        add_fdata <- subset(add_fdata, !id %in% acmfdata$id)
        
        if (nrow(add_fdata) > 0){
          acmfdata <- rbind(acmfdata, add_fdata)
        }
      }
      acmfdata <- subset(acmfdata, n_baseline >= 10000)
      local_cov_method <- T
      # if (i == 3 || i == 4) local_cov_method <- T
      if (nrow(acmfdata) > 0){
        acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
        acmfdata <- subset(acmfdata, !((effect == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                         (effect != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
        acmfdata <- subset(acmfdata, select = c(id, ref_number, first_author, effect_measure, outcome_type, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
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
            
            res <- metaAnalysis(ld, ptitle = "", returnval = T, covMethed = T, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
            
            if (is.null(res) || is.na(res)){
              res <- metaAnalysis(ld, ptitle = "", returnval = T, covMethed = F, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
            }
            
            if (is.null(res) || is.na(res)){
              res <- metaAnalysis(ld, ptitle = "", returnval = T, covMethed = F, minQuantile = 0, maxQuantile = 0.9, lout = 1000)
            }
            
            dataset2 <- data.frame(res)
            
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
            # browser()
            p <- ggplot() +
              geom_line(data = dataset, aes(dose, RR, col = factor(id), group = ref_number)) +
              geom_point(data = dataset, aes(dose, RR, col = factor(id), label = first_author), size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
              # geom_dl(data = dataset, aes(dose, RR, label = ref_number), method=list(
              #   cex=0.8,
              #   directlabels::polygon.method(
              #     "top",
              #     offset.cm=0.5,
              #     padding.cm=0.05)))  + 
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