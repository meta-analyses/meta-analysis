# Remove all elements from the memory
rm (list = ls())

# Set log file
record_removed_entries <- "missing_entries.csv"

# Check its existence
if (file.exists(record_removed_entries)) {
  # Delete file if it exists
  file.remove(record_removed_entries)
}


for(local_last_knot in c(0.75, 0.85))
  for(ALT in c(TRUE, FALSE))
    for (NO_BMI_EFFECT in c(TRUE, FALSE)){
      print(paste(local_last_knot, ALT, NO_BMI_EFFECT, sep = "-"))
      source("script/analysis.R")
      source("script/gender_analysis.R")
      
    }


