# Remove all elements from the memory
rm (list = ls())

for(local_last_knot in c(0.75, 0.85))
  for(ALT in c(TRUE, FALSE))
    for (NO_BMI_EFFECT in c(TRUE, FALSE)){
      print(paste(local_last_knot, ALT, NO_BMI_EFFECT, sep = "-"))
      source("script/analysis.R")
      source("script/gender_analysis.R")
      
    }


