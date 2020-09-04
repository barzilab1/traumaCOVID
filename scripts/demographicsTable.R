### This script creates a demographics table
### https://www.r-bloggers.com/table-1-and-the-characteristics-of-study-population/
### https://davidgohel.github.io/officer/
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
names(full_df)[names(full_df) == 'sex'] <- 'Gender'

getRace <- function(i) {
  if (full_df[i, 'race_white'] == 1) { 'White'
  } else if (full_df[i, 'race_black'] == 1) { 'Black'
  } else { 'Other' }
}
full_df$Race <- sapply(1:nrow(full_df), getRace)

listVar <- c('Age', 'Gender', 'Race', 'ACE_sum', paste0('ACE_', 1:10))
catVar <- c('Gender', 'Race', paste0('ACE_', 1:10))

table1 <- CreateTableOne(vars = listVar, data = full_df, factorVars = catVar)
table1 <- print(table1)

print(table1, target = "~/Documents/traumaCOVID/tables/table1.docx")
