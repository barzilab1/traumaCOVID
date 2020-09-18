### This script creates a demographics table
### https://www.r-bloggers.com/table-1-and-the-characteristics-of-study-population/
### https://davidgohel.github.io/officer/
### https://github.com/noamross/redoc
###
### Ellyn Butler
### September 3, 2020

library('devtools')
library('tableone')
library('officer')
library('magrittr')

full_df <- read.csv('~/Documents/traumaCOVID/data/cleandata_2020-08-07.csv')

names(full_df)[names(full_df) == 'sex'] <- 'Gender'
names(full_df)[names(full_df) == 'age'] <- 'Age'
names(full_df)[names(full_df) == 'occu_health'] <- 'Healthcare_Provider'
names(full_df)[names(full_df) == 'exp_test'] <- 'Receive_Coronavirus_Test'
names(full_df)[names(full_df) == 'exp_job_reduce'] <- 'Reduced_Job'

getRace <- function(i) {
  if (full_df[i, 'race_white'] == 1) { 'White'
  } else if (full_df[i, 'race_black'] == 1) { 'Black'
  } else { 'Other' }
}
full_df$Race <- sapply(1:nrow(full_df), getRace)

listVar <- c('Age', 'Gender', 'Race', 'Healthcare_Provider',
  'Receive_Coronavirus_Test', 'Reduced_Job', 'ACE_sum', paste0('ACE_', 1:10))
catVar <- c('Gender', 'Race', paste0('ACE_', 1:10))

table1 <- CreateTableOne(vars = listVar, data = full_df, factorVars = catVar)
table1 <- print(table1)

#print(table1, target = "~/Documents/traumaCOVID/tables/table1.docx")
