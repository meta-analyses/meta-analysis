source("script/filter_studies.R")
options(warn = -1)

total_population <- TRUE

# Set fixed last knot to 75th of person years
local_last_knot <- 0.75

# Set log file
record_removed_entries <- "missing_entries.csv"

# Check its existence
if (file.exists(record_removed_entries)) {
  # Delete file if it exists
  file.remove(record_removed_entries)
}

fold <- ifelse(ALT, "plots/alt_analysis/", "plots/main_analysis/")

# get all png files in the directories, recursively
f <- list.files(fold, pattern = ".png", include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
# remove the files
file.remove(f)

fatal_plots <- htmltools::tagList()
non_fatal_plots <- htmltools::tagList()
fatal_non_fatal_plots <- htmltools::tagList()

PIF_df <- NULL
test_df <- NULL
rr_conf_df <- NULL

if (total_population) {
  for (i in 1:nrow(uoutcome)) {
    # Loop through all three outcome types
    for (local_outcome_type in c("Fatal", "Non-fatal", "Both")) {
      # local_outcome_type <- "Fatal"; i <- 1
      
      # Select output directory according to outcome type
      if (local_outcome_type == "Fatal") {
        dir_name <- "Fatal"
      } else if (local_outcome_type == "Non-fatal") {
        dir_name <- "Non-fatal"
      } else {
        dir_name <- "Fatal and non-fatal"
      }
      
      # Print basic info re outcome, outcome type and index
      cat(
        "Total Population - Outcome: ", uoutcome$outcom[i], " , outcome type ",
        dir_name, " and index ", i, "\n"
      )
      
      # Subset according to outcome, domain and outcome type
      acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] &
                           pa_domain_subgroup == local_pa_domain_subgroup &
                           outcome_type == local_outcome_type)
      
      # Add additional "fatal" studies that had no "both" types
      if (local_outcome_type == "Both") {
        # Subset fatal types
        add_fdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & 
                              pa_domain_subgroup == local_pa_domain_subgroup & 
                              outcome_type == "Fatal")
        # ONLY add those studies that have no "both" studies
        add_fdata <- subset(add_fdata, !id %in% acmfdata$id)
        # Add additional rows
        if (nrow(add_fdata) > 0) {
          # if (nrow(acmfdata) == 0)
          #  next()
          acmfdata <- rbind(acmfdata, add_fdata)
        }
        
        # Subset Non-fatal types
        add_nfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & 
                               pa_domain_subgroup == local_pa_domain_subgroup & 
                               outcome_type == "Non-fatal")
        
        # ONLY add those studies that have no "both" studies
        add_nfdata <- subset(add_nfdata, !id %in% acmfdata$id)
        # Add additional rows
        if (nrow(add_nfdata) > 0) {
          acmfdata <- rbind(acmfdata, add_nfdata)
        }
      }
      
      print(length(unique(acmfdata$ref_number)))
      
      # Use default covariance method
      local_cov_method <- TRUE
      if (nrow(acmfdata) > 0) {
        # Fill missing values by inferring to useful columns
        acmfdata <- getMissingVariables(acmfdata, infertotalpersons = TRUE, kcases = FALSE)
        
        acmfdata$analysis_outcome_type <- local_outcome_type
        
        # Before removing any lines with n requirement less than 10k
        missing_cases <- acmfdata
        
        # Keep only those studies with cases present
        acmfdata <- subset(acmfdata, !is.na(cases))
        
        # Keep only those studies with n_baseline greater than 10k
        missing_cases <- setdiff(missing_cases, acmfdata)
        if (nrow(missing_cases) > 0) {
          missing_cases$reason <- "missing cases"
          readr::write_csv(missing_cases, record_removed_entries, append = TRUE)
        }
        
        # Before removing any lines with n requirement less than 10k
        n_subset <- acmfdata
        
        # Remove all studies with missing RRs
        missing_RR_ids <- subset(acmfdata, is.na(RR)) %>% select(id)
        if (nrow(missing_RR_ids) > 0) {
          temp <- subset(acmfdata, id %in% missing_RR_ids)
          temp$reason <- "missing RRs"
          readr::write_csv(temp, record_removed_entries, append = TRUE)
          acmfdata <- subset(acmfdata, !id %in% missing_RR_ids)
        }
        
        # Remove all studies with negative standard error (SE)
        negative_SE_ids <- subset(acmfdata, se < 0) %>% select(id)
        if (nrow(negative_SE_ids) > 0) {
          temp <- subset(acmfdata, id %in% negative_SE_ids)
          temp$reason <- "negative error"
          readr::write_csv(temp, record_removed_entries, append = TRUE)
          acmfdata <- subset(acmfdata, !id %in% negative_SE_ids)
        }
        
        # Before removing any lines with n requirement less than 10k
        n_missing <- acmfdata
        
        # Remove all studies with mandatory info
        acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0)) |
                                         (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0)))))
        n_missing <- setdiff(n_missing, acmfdata)
        if (nrow(n_missing) > 0) {
          n_missing$reason <- "missing either person years or total persons"
          readr::write_csv(n_missing, record_removed_entries, append = TRUE)
        }
        
        # NOTE TO MATT/LEANDRO
        # This removes all studies with repeating rows such as studies with both sex and ethnicity entries
        # Won'TRUE need it if we remove all such rows from the dataset
        # Identify all studies with repeating IDs
        local_filter <- acmfdata %>%
          group_by(id) %>%
          summarise(c = sum(is.na(se))) %>%
          filter(c > 1) %>%
          dplyr::select(id)
        
        # Remove all such studies altogether - which is a temp fix
        if (nrow(local_filter) > 0) {
          temp <- subset(acmfdata, id %in% local_filter)
          temp$reason <- "multiple stratification"
          readr::write_csv(temp, record_removed_entries, append = TRUE)
          acmfdata <- acmfdata %>% filter(!id %in% local_filter)
        }
        
        orig_col_names <- colnames(acmfdata)
        
        # Select subset of columns
        acmfdata <- subset(acmfdata, select = c(id, ref_number, first_author, effect_measure, outcome_type, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
        # Get last knot based on 75% of person years
        last_knot <- get_last_knot(acmfdata, dose_pert = local_last_knot, personyrs_pert = local_last_knot)
        last_knot <- last_knot[2]
        
        if (nrow(acmfdata) > 0) {
          dataset <- acmfdata
          # Get quantiles (0th, 37.5th and 75th)
          q <- quantile(dataset$dose, prob = last_knot)
          last_quintile <- gsub("%", "", names(q)) %>%
            as.numeric() %>%
            round(1)
          
          last_knot_title <- paste0(last_quintile, "% dose (using ", (local_last_knot * 100), "% person years)")
          if (!is.null(dataset)) {
            dataset$personyrs <- round(dataset$personyrs)
            group_by(dataset, id) %>%
              select(dose, se) %>%
              summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
            pa <- acmfdata
            
            # By default run the analysis with Hamling method to approximate covariance
            res <- metaAnalysis(dataset, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
            
            # If it fails, use the default by Greenland and Longnecker (gl)
            if (is.null(res) || is.na(res)) {
              res <- metaAnalysis(dataset, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
            }
            
            # If this too fails, increase last_knot by 5% until it converges
            if (is.null(res) || is.na(res)) {
              for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
                print(nq)
                last_knot <- get_last_knot(acmfdata, dose_pert = nq, personyrs_pert = nq)
                q <- quantile(dataset$dose, prob = last_knot[2])
                res <- metaAnalysis(dataset, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
                if (!is.null(res)) {
                  last_quintile <- gsub("%", "", names(q)) %>%
                    as.numeric() %>%
                    round(1)
                  last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
                  break
                }
              }
            }
            
            # if (!is.null(res))
            #   next()
            
            if (length(unique(dataset$id)) < 4) {
              next
            }
            
            stats_Q_lbl <- ""
            stats_I_lbl <- ""
            
            if (res[[3]]$proc == "2stage"){
              qt <- qtest(res[[3]])
              Q <- formatC(qt$Q, digits = 3, format = "f")
              pvalue <- formatC(qt$pvalue, digits = 3, format = "f")
              i2 <- formatC(pmax((qt$Q - qt$df)/qt$Q * 100,
                                 0), digits = 1, format = "f")
              stats_Q_lbl <- paste0("Q-test = ", Q[1], 
                                    " (df = ", qt$df[1], "), p-value = ", 
                                    pvalue[1])
              stats_I_lbl <- paste0("I-square statistic = ", i2[1], "%")
            }
            
            # Save results as data frame
            dataset2 <- data.frame(cbind(res[[1]], res[[2]]))
            
            # Assign names
            colnames(dataset2) <- c("dose", "RR", "lb", "ub")
            
            # Identify personyrs above 40
            clipped_personyrs <- filter(dataset, dose > 40) %>% summarise(v = sum(personyrs) / sum(dataset$personyrs, na.rm = T) * 100) %>% dplyr::select(v) %>% round(1)
            
            clipped_personyrs_title <- ""
            
            if (clipped_personyrs$v > 5){
              clipped_personyrs_title <- paste0(" clipped personyrs (", clipped_personyrs$v, "%)")
            }
            
            # Create plot title
            plot_title <- paste0(uoutcome$outcome[i], " - ", simpleCap(dir_name), " - Total Population")
            plot_title <- paste0(
              simpleCap(plot_title), " \nNumber of entries: ",
              length(unique(acmfdata$id)),
              " \nPerson-years: ", format(round(sum(dataset$personyrs, na.rm = TRUE)), scientific = FALSE, big.mark = ','), 
              clipped_personyrs_title, 
              "\n Last knot: ", last_knot_title
            )
            
            dataset$ref_number <- as.factor(dataset$ref_number)
            # Create plot
            p <- ggplot() +
              geom_line(data = dataset, aes(dose, RR, col = ref_number, group = ref_number)) +
              annotate("text",  x = 30, y = 0.2, hjust = 0, vjust = 0, label = stats_Q_lbl, size = 3) +
              annotate("text",  x = 30, y = 0, hjust = 0, vjust = 0, label = stats_I_lbl, size = 3) +
              geom_point(data = dataset, aes(dose, RR, col = ref_number, label = first_author, group = personyrs), size = 4 * (dataset$personyrs - min(dataset$personyrs)) / diff(range(dataset$personyrs))) +
              geom_line(data = subset(dataset2, dose < as.numeric(q)), aes(x = dose, y = RR)) +
              geom_line(data = subset(dataset2, dose >= as.numeric(q)), aes(x = dose, y = RR), linetype = "dashed") +
              geom_ribbon(data = subset(dataset2, dose < as.numeric(q)), aes(x = dose, ymin = `lb`, ymax = `ub`), alpha = 0.25) +
              geom_ribbon(data = subset(dataset2, dose >= as.numeric(q)), aes(x = dose, ymin = `lb`, ymax = `ub`), alpha = 0.10) +
              geom_vline(xintercept = q, linetype = "dotted", alpha = 0.6) +
              coord_fixed(ylim = c(0, 1.5), x = c(0, 40), ratio = 10) +
              theme(
                legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 9)
              ) +
              xlab("\nMarginal MET hours per week\n") +
              ylab("\nRelative Risk\n") +
              labs(title = paste(plot_title))
            
            # Print plot
            print(p)
            
            # Save the plot as a .Rds file
            # saveRDS(p, paste0("plots/", dir_name, "/", uoutcome$outcome[i], "-", dir_name, ".Rds"), version = 2)
            
            if (local_outcome_type == "Fatal") {
              fatal_plots[[length(fatal_plots) + 1]] <- as.widget(plotly::ggplotly(p))
            } else if (local_outcome_type == "Non-fatal") {
              non_fatal_plots[[length(non_fatal_plots) + 1]] <- as.widget(plotly::ggplotly(p))
            } else {
              fatal_non_fatal_plots[[length(fatal_non_fatal_plots) + 1]] <- as.widget(plotly::ggplotly(p))
            }
            
            
            # Save plot
            ggsave(paste0(fold, dir_name, "/", uoutcome$outcome[i], "-", dir_name, ".png"), height = 5, width = 10, units = "in", dpi = 600, scale = 1)
            
            q <- ggplot(subset(dataset, se != 0),
                        aes(dose, vpc(res[[3]]))) +
              geom_point() + 
              geom_smooth(method = "loess", se = F, col = "black") +
              labs(y = "VPC", x = "Dose")
            
            ggsave(paste0(fold, "vpc/", uoutcome$outcome[i], "-", dir_name, ".png"), height = 5, width = 10, units = "in", dpi = 600, scale = 1)
            
            # at 4.375, 8.750, and 17.500 
            
            lowest_PIF <- round(get_pif_values(acmfdata, dataset2, 4.375), 2)
            mid_PIF <- round(get_pif_values(acmfdata, dataset2, 8.75), 2)
            highest_PIF <- round(get_pif_values(acmfdata, dataset2, 17.5), 2)
            
            temp_df <- data.frame(outcome = uoutcome$outcome[i], outcome_type = local_outcome_type, lowest_guideline = lowest_PIF[1],
                                  lowest_CFI = paste0("(",lowest_PIF[2], " - ", lowest_PIF[3], ")"), mid_guideline = mid_PIF[1],
                                  mid_CFI = paste0("(", mid_PIF[2], " - ", mid_PIF[3], ")"), highest_guideline = highest_PIF[1],
                                  highest_CFI = paste0("(", highest_PIF[2], " - ", highest_PIF[3], ")"))
            
            PIF_df <- rbind(PIF_df,temp_df)
            
            rr_df <- get_ma_table(dataset2, "RR")
            lb_df <- get_ma_table(dataset2, "lb")
            ub_df <- get_ma_table(dataset2, "ub")
            
            rr_conf <- data.frame(outcome = uoutcome$outcome[i], outcome_type = local_outcome_type, lowest_guideline = rr_df[1],
                                  lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                                  mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), highest_guideline = rr_df[3],
                                  highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
            
            rr_conf_df <- rbind(rr_conf, rr_conf_df)
            
            test_labels <- data.frame(outcome = uoutcome$outcome[i], outcome_type = local_outcome_type, 
                                      q_test = stats_Q_lbl, i_test = stats_I_lbl)
            
            test_df <- rbind(test_df, test_labels)
            
            
          }
        }
      }
    }
  }
}

# Adjust subtitle acc. to analysis
sub_title <- ifelse(ALT, "alt", "main")

# Save rr_conf_df table
write_csv(rr_conf_df, paste0("data/output/rr-", sub_title, "-analysis-total-pop.csv"))

# Save PIF table
write_csv(PIF_df, paste0("data/output/PIF-", sub_title, "-analysis-total-pop.csv"))

# Save test table
write_csv(test_df, paste0("data/output/statistical-tests-", sub_title, "-analysis-total-pop.csv"))

save(fatal_plots, file = paste0(fold, "html_widgets/fatal_plots.RData"))
save(non_fatal_plots, file = paste0(fold, "html_widgets/non_fatal_plots.RData"))
save(fatal_non_fatal_plots, file = paste0(fold, "html_widgets/fatal_non_fatal_plots.RData"))

# Read csv file and append column name
if (file.exists("missing_entries.csv")) {
  temp <- read_csv("missing_entries.csv", col_names = FALSE)
  if (!any(colnames(temp) == "reason")) {
    colnames(temp) <- append(orig_col_names, "reason")
    temp <- temp[!duplicated(temp), ]
    readr::write_csv(temp, "missing_entries.csv")
  }
}