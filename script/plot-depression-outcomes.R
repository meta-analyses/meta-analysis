#### Plot depression, major depression and depressive symptoms

# ASSUMPTION: the main script has been run, which is available at script/master_script. This runs 
# the meta-analyses, and saves the output in CSV files. The current list of outcomes (causes and diseases) are:
# All-cause cancer
# All-cause cvd
# All-cause dementia
# All-cause mortality
# Alzheimer's disease
# Bladder cancer
# Breast cancer
# Colon cancer
# Coronary heart disease
# Depression
# Elevated depressive symptoms
# Endometrial cancer
# Esophageal cancer
# Gastric cardia cancer
# Head and neck cancer
# Heart failure
# Kidney cancer
# Liver cancer
# Lung cancer
# Major depression
# Myeloid leukemia
# Myeloma
# Parkinson's disease
# Prostate cancer
# Rectum cancer
# Stroke
# Vascular dementia

# Declare global variables
ALT <- FALSE
NO_BMI_EFFECT <- FALSE

# Load all required datasets and functions
source("script/filter_studies.R")

# Select specific outcomes
uoutcome_depression <- uoutcome %>% filter(outcome %in% c("Depression", "Elevated depressive symptoms", "Major depression"))

plot_RR <- function(dataset, q, plot_title){
  
  # Create plot
  p <- ggplot() +
    geom_line(data = subset(dataset, dose < as.numeric(q)), aes(x = dose, y = RR)) +
    geom_line(data = subset(dataset, dose >= as.numeric(q)), aes(x = dose, y = RR), linetype = "dashed") +
    geom_ribbon(data = subset(dataset, dose < as.numeric(q)), aes(x = dose, ymin = `lb`, ymax = `ub`), alpha = 0.25) +
    geom_ribbon(data = subset(dataset, dose >= as.numeric(q)), aes(x = dose, ymin = `lb`, ymax = `ub`), alpha = 0.10) +
    coord_fixed(ylim = c(0, 1.5), x = c(0, 40), ratio = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 9)
    ) +
    xlab("\nMarginal MET hours per week\n") +
    ylab("\nRelative Risk\n") + 
    labs(title = plot_title)
  p
}

# Read total population's RR
prefix_path <- "data/csv/MA-DR/"

# List to store all DR
df_list <- list()
# List to store last knots - disease and outcome type specific
last_knot_df <- list()
# Loop all three last knot position
for(local_last_knot in c(0.75, 0.85, 0.95)){
  last_knot_df[[paste0("lastknot-", local_last_knot)]] <- read_csv(paste0(prefix_path, local_last_knot,"/", "75p_diseases.csv"))
  # Loop for outcomes
  for (i in 1:nrow(uoutcome_depression)){
    # Loop through all three outcome types
    for (local_outcome_type in c("Fatal", "Non-fatal", "Both")) {
      
      # Select output directory according to outcome type
      if (local_outcome_type == "Fatal") {
        dir_name <- "Fatal"
      } else if (local_outcome_type == "Non-fatal") {
        dir_name <- "Non-fatal"
      } else {
        dir_name <- "Fatal and non-fatal"
      }
      
      snake_case_outcome <- gsub(x = uoutcome_depression$outcome[i], pattern = " ", replacement = "-") %>% tolower()
      snake_case_outcome_type <- gsub(x = dir_name, pattern = " ", replacement = "-") %>% tolower()
      ma_filename <- paste0(snake_case_outcome, "-", snake_case_outcome_type)
      
      fname <- print(paste0(prefix_path, local_last_knot,"/", ma_filename, ".csv"))
      if (file.exists(fname)){
        df_list[[paste0(ma_filename, "-last-knot-", local_last_knot)]] <- read_csv(fname)
        
        plot_title <- paste0(uoutcome_depression$outcome[i], " - ", simpleCap(dir_name))
        q <- last_knot_df[[paste0("lastknot-", local_last_knot)]] %>% filter(disease == snake_case_outcome) %>% 
          dplyr::select(snake_case_outcome_type) %>% 
          as.numeric() %>% round(1)
        
        print(plot_RR(df_list[[paste0(ma_filename, "-last-knot-", local_last_knot)]], q, plot_title))
        
      }
      
    }
  }
}

dataset <- df_list$`major-depression-fatal-and-non-fatal-last-knot-0.75`

q <- last_knot_df$`lastknot-0.75` %>% filter(disease == "major-depression") %>% dplyr::select("fatal-and-non-fatal") %>% as.numeric() %>%
  round(1)



plot_RR(dataset, q)




    