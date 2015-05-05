# Data Sources
# ************

# Features from Dongxu
# --------------------
  dta11_f <- read.csv(paste0(getwd(),"/","DTA_11_features.csv"))
  # trip length - total distance traveled (m) 
  # distance covered - euclidean distance between start and stop (m)
  # acc - acceleration 
  # aveacc_pos - average of positive acceleration 
  # aveacc_neg - average of negative acceleration (deceleration)
  # std - standard deviation
