source("script/filter_studies.R")
options(warn=-1)

total_population <- T

# Set fixed last knot to 75th of person years
local_last_knot <- 0.75

# Set log file
record_removed_entries <- 'missing_entries.csv'

#Check its existence
if (file.exists(record_removed_entries)) {
  #Delete file if it exists
  file.remove(record_removed_entries)
}

fold <- 'plots/'

# get all files in the directories, recursively
f <- list.files(fold, include.dirs = F, full.names = T, recursive = T)
# remove the files
file.remove(f)

if (total_population){
  for (i in 1:nrow(uoutcome)){
    #Loop through all three outcome types
    for (local_outcome_type in c('Fatal', 'Non-fatal', 'Both')){
      # local_outcome_type <- 'Fatal'; i <- 1
      
      # Select output directory according to outcome type
      if (local_outcome_type == 'Fatal'){
        dir_name <- 'Fatal'
      } else if (local_outcome_type == 'Non-fatal'){
        dir_name <- 'Non-fatal'
      } else {
        dir_name <- 'Fatal and non-fatal'
      }
      
      # Print basic info re outcome, outcome type and index
      cat("Total Population - Outcome: ", uoutcome$outcome[i], " , outcome type ", 
          dir_name, " and index ", i, "\n")
      
      # Subset according to outcome, domain and outcome type
      acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & 
                           pa_domain_subgroup == local_pa_domain_subgroup & 
                           outcome_type == local_outcome_type)
      
      # Add additional 'fatal' studies that had no 'both' types
      if (local_outcome_type == 'Both'){
        # Subset fatal types
        add_fdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & outcome_type == 'Fatal')
        # ONLY add those studies that have no 'both' studies
        add_fdata <- subset(add_fdata, !id %in% acmfdata$id)
        # Add additional rows
        if (nrow(add_fdata) > 0){
          #if (nrow(acmfdata) == 0)
          #  next()
          acmfdata <- rbind(acmfdata, add_fdata)
        }
        
        # Subset Non-fatal types
        add_nfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & outcome_type == 'Non-fatal')
        
        # ONLY add those studies that have no 'both' studies
        add_nfdata <- subset(add_nfdata, !id %in% acmfdata$id)
        # Add additional rows
        if (nrow(add_nfdata) > 0){
          acmfdata <- rbind(acmfdata, add_nfdata)
        }
      }
      
      print(length(unique(acmfdata$ref_number)))
      
      # Use default covariance method
      local_cov_method <- T
      if (nrow(acmfdata) > 0){
        # Fill missing values by inferring to useful columns
        acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = F)
        
        acmfdata$analysis_outcome_type <- local_outcome_type
        
        # Before removing any lines with n requirement less than 10k
        missing_cases <- acmfdata
        
        # Keep only those studies with cases present
        acmfdata <- subset(acmfdata, !is.na(cases))
        
        # Keep only those studies with n_baseline greater than 10k
        missing_cases <- setdiff(missing_cases, acmfdata )
        if (nrow(missing_cases) > 0){
          missing_cases$reason <- 'missing cases'
          readr::write_csv(missing_cases, record_removed_entries, append = T)
        }
        
        # Before removing any lines with n requirement less than 10k
        n_subset <- acmfdata
        
        # Keep only those studies with n_baseline greater than 10k
        acmfdata <- subset(acmfdata, n_baseline >= 10000)
        
        # Keep only those studies with n_baseline greater than 10k
        n_subset <- setdiff(n_subset, acmfdata )
        if (nrow(n_subset) > 0){
          n_subset$reason <- 'n_baseline < 10k'
          readr::write_csv(n_subset, record_removed_entries, append = T)
        }
        
        # Remove all studies with missing RRs
        missing_RR_ids <- subset(acmfdata, is.na(RR)) %>% select(id)
        if (nrow(missing_RR_ids) > 0){
          temp <- subset(acmfdata, id %in% missing_RR_ids)
          temp$reason <- 'missing RRs'
          readr::write_csv(temp, record_removed_entries, append = T)
          acmfdata <- subset(acmfdata, !id %in% missing_RR_ids)
        }
        
        # Remove all studies with negative standard error (SE)
        negative_SE_ids <- subset(acmfdata, se < 0) %>% select(id)
        if (nrow(negative_SE_ids) > 0){
          temp <- subset(acmfdata, id %in% negative_SE_ids)
          temp$reason <- 'negative error'
          readr::write_csv(temp, record_removed_entries, append = T)
          acmfdata <- subset(acmfdata, !id %in% negative_SE_ids)
        }
        
        # Before removing any lines with n requirement less than 10k
        n_missing <- acmfdata
        
        # Remove all studies with mandatory info
        acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                         (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
        n_missing <- setdiff(n_missing, acmfdata)
        if (nrow(n_missing) > 0){
          n_missing$reason <- 'missing either person years or total persons'
          readr::write_csv(n_missing, record_removed_entries, append = T)
        }
        
        # NOTE TO MATT/LEANDRO
        # This removes all studies with repeating rows such as studies with both sex and ethnicity entries
        # Won't need it if we remove all such rows from the dataset
        # Identify all studies with repeating IDs
        local_filter <- acmfdata %>% group_by(id) %>% summarise(c = sum(is.na(se))) %>% filter(c > 1) %>% dplyr::select(id)
        
        # Remove all such studies altogether - which is a temp fix
        if (nrow(local_filter) > 0){
          
          temp <- subset(acmfdata, id %in% local_filter)
          temp$reason <- 'multiple stratification'
          readr::write_csv(temp, record_removed_entries, append = T)
          acmfdata <- acmfdata %>% filter(!id %in% local_filter)
        }
        
        orig_col_names <- colnames(acmfdata)
        
        # Select subset of columns
        acmfdata <- subset(acmfdata, select = c(id, ref_number, first_author, effect_measure, outcome_type, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
        # Get last knot based on 75% of person years
        last_knot <- get_last_knot(acmfdata, dose_pert = local_last_knot , personyrs_pert = local_last_knot)
        last_knot <- last_knot[2]
        
        if (nrow(acmfdata) > 0){
          dataset <- acmfdata
          # Get quantiles (0th, 37.5th and 75th)
          q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
          last_quintile <- gsub("%", "", names(q)[3]) %>% as.numeric() %>% round(1)
          
          last_knot_title <- paste0(last_quintile, '% dose (using ', (local_last_knot * 100), '% person years)')
          if (!is.null(dataset)){
            dataset$personyrs <- round(dataset$personyrs)
            group_by(dataset, id) %>% select(dose, se) %>%
              summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
            pa <- acmfdata
            
            # By default run the analysis with Hamling method to approximate covariance
            res <- metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = T, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
            
            # If it fails, use the default by Greenland and Longnecker (gl)
            if (is.null(res) || is.na(res)){
              res <- metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = F, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
            }
            
            # If this too fails, increase last_knot by 5% until it converges
            if (is.null(res) || is.na(res)){
              
              for (nq in seq(from = round(last_quintile + 5), 100, 5)){
                nq <- nq / 100
                q <- quantile(dataset$dose, c(0, nq / 2, nq))
                res <- metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = F, minQuantile = 0, maxQuantile = nq, lout = 1000)
                if (!is.null(res)){
                  last_quintile <- gsub("%", "", names(q)[3]) %>% as.numeric() %>% round(1)
                  last_knot_title <- paste0(last_quintile, '% dose')
                  break
                }
              }            
            }
            
            # Save results as data frame
            dataset2 <- data.frame(res)
            
            # Assign names
            colnames(dataset2) <- c("dose","RR", "lb", "ub")
            
            # if (length(unique(dataset$id)) > 5){
            #   # 20% + median
            #   med_val <- (median(dataset2$dose, na.rm = T) * (20/100 * last_knot + last_knot))
            #   
            #   print(med_val)
            #   
            #   dataset2 <- subset(dataset2, dose < med_val)
            #   
            #   dataset <- subset(dataset, dose < med_val)
            # }
            
            # Create plot title 
            plotTitle <- paste0( uoutcome$outcome[i] ,  " - ", simpleCap(dir_name), " - Total Population")
            plotTitle <-  paste0(simpleCap(plotTitle), ' \nNumber of entries: ',
                                 length(unique(acmfdata$id)),
                                 ' \nNumber of people: ' , round(sum(acmfdata$totalpersons, na.rm = T)), 
                                 "\n Last knot: ", last_knot_title)
            # Create plot
            p <- ggplot() +
              geom_line(data = dataset, aes(dose, RR, col = factor(id), group = ref_number)) +
              geom_point(data = dataset, aes(dose, RR, col = factor(id), label = first_author), size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
              geom_line(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, y = RR)) +
              geom_line(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, y = RR), linetype = "dashed") +
              geom_ribbon(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
              geom_ribbon(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.10) +
              geom_vline(xintercept= q, linetype="dotted", alpha = 0.6) +
              coord_fixed(ylim = c(0, 1.5), x = c(0,40), ratio = 10) + 
              theme(legend.position="none",
                    plot.title = element_text(hjust = 0.5)) +
              xlab("\nMarginal MET hours per week\n") +
              ylab("\nRelative Risk\n") +
              labs(title = paste(plotTitle))
            
            # Print plot
            print(p)
            
            # Save plot
            ggsave(paste0('plots/', dir_name, '/', uoutcome$outcome[i], "-", dir_name, ".png"), height=5, width=10, units='in', dpi=600, scale = 1)
          }
        }
      }
    }
  }
}

# Read csv file and append column name
temp <- read_csv('missing_entries.csv', col_names = F)
colnames(temp) <- append(orig_col_names, 'reason')
temp <- temp[!duplicated(temp),]
readr::write_csv(temp, 'missing_entries.csv')

