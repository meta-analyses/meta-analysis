# Remove all elements from the memory
rm (list = ls())

for(local_last_knot in c(0.85, 0.75))
  for(ALT in c(TRUE, FALSE))
    for (NO_BMI_EFFECT in c(TRUE, FALSE)){
      source("script/analysis.R")
      
    }


