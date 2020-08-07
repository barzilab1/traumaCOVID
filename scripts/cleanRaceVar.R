### Recoding of race variable from raw data
###
### Ellyn Butler
### June 16, 2020 - August 7, 2020


race_df <- read.csv('~/Documents/traumaCOVID/data/COVID-19Resilience_May_6_2020_14.22.csv')
given_df <- read.csv('~/Documents/traumaCOVID/data/ACE_dataset_pulled_05062020.csv')
given_df <- given_df[, c('randomID', 'race')]
names(given_df) <- c('randomID', 'race_givendata')
id_df <- read.csv('~/Documents/traumaCOVID/data/RaceData_CovidSilience_20200616.csv')

final_df <- merge(race_df[3:nrow(race_df), c('ResponseId', 'race')], id_df)
final_df <- merge(final_df, given_df)

# Checked two races - Looks good
final_df[final_df$race %in% c('6,5', '2,5', '1,5', '3,5', '1,2', '5,-2', '2,6',
  '2,3', '5,-1', '1,-2', '4,5', '3,-2'),]

# Checked three races - All races from given_df are NA
final_df[final_df$race %in% c('1,3,5', '2,6,5', '3,6,5', '2,3,5', '3,5,-2', '2,4,5',
  '3,4,5'),]

# Checked four races - All races from given_df are NA
final_df[final_df$race == '1,2,4,5',]
