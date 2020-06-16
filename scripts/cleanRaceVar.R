### Recoding of race variable from raw data
###
### Ellyn Butler
### June 16, 2020

library('dplyr')

race_df <- read.csv("~/Documents/traumaCOVID/data/COVID-19Resilience_May_6_2020_14.22.csv")

race_df <- race_df[3:nrow(race_df),]
