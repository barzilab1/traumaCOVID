### This script finds the date of the first and last administration of the
### survey in the subsample of participants included in this paper.
###
### Ellyn Butler
### September 3, 2020

date_df <- read.csv('~/Documents/traumaCOVID/data/ACE_dataset_pulled_05062020.csv')

final_df <- read.csv('~/Documents/traumaCOVID/data/cleandata_2020-08-07.csv')

df$StartDate <- as.Date(df$StartDate, "%m/%d/%Y")

first_people <- df[df$StartDate == min(df$StartDate), 'randomID']
last_people <- df[df$StartDate == max(df$StartDate), 'randomID']

any(first_people %in% final_df$randomID)
any(last_people %in% final_df$randomID)

min(df$StartDate)
max(df$StartDate)
