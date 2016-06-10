# Read the data
data <- read.csv("data/09.06_COMBINED DATASET REDUCED.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

# Read all the functions
source("all-functions.R")

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(data$outcome)))
