# Remove all elements from the memory
rm (list = ls())

# Set log file
record_removed_entries <- "missing_entries.csv"

# Check its existence
if (file.exists(record_removed_entries)) {
  # Delete file if it exists
  file.remove(record_removed_entries)
}

# Loop through three global variables
# 1. Last knot which has 0.75 and 0.85 as values
# 2. Alternate assumptions for MMETs - on or off
# 3. NO_BMI_EFFECT - on or off
# Set these variables for total and gendered analysis

for(local_last_knot in c(0.75, 0.85))
  for(ALT in c(TRUE, FALSE))
    for (NO_BMI_EFFECT in c(TRUE, FALSE)){
      print(paste(local_last_knot, ALT, NO_BMI_EFFECT, sep = "-"))
      source("script/analysis.R")
      source("script/gender_analysis.R")
      
    }

# Once these lines are executed, the markdown documents can be knitted to produce plots


