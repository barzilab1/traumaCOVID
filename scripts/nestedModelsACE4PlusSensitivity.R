### Hypothesized models, with Null Model containing demographics
###
### Ellyn Butler
### June 20, 2020 - October 4, 2020

# Load libraries
library('ggplot2')
library('gridExtra')
library('dplyr')
library('gt')
library('sjPlot')


# Load data
full_df <- read.csv('~/Documents/traumaCOVID/data/cleandata_2020-09-17.csv')
full_df$Finished_College <- recode(full_df$edu, 'LessThanHS'=0, 'HS'=0,
  'SomeCollege'=0, 'College'=1, 'Masters'=1, 'Doctorate'=1)
names(full_df)[names(full_df) == 'race_white'] <- 'White'
full_df$Female <- recode(full_df$sex, 'Female'=1, 'Male'=0)
full_df$Health <- recode(full_df$occu_health, 'Yes'=1, 'No'=0)
names(full_df)[names(full_df) == 'age'] <- 'Age'
full_df$Threat <- recode(full_df$exp_threat, 'Yes'=1, 'No'=0)
full_df$Deprivation <- recode(full_df$exp_deprivation, 'Yes'=1, 'No'=0)
full_df$Instability <- recode(full_df$exp_undepend, 'Yes'=1, 'No'=0)
full_df$Job_Reduced <- recode(full_df$exp_job_reduce, 'Yes'=1, 'No'=0)
full_df$COVID_Test <- recode(full_df$exp_test, 'Yes'=1, 'No'=0)
full_df$ACE_4Plus <- recode(full_df$ACE_sum, `0`=0, `1`=0, `2`=0, `3`=0, `4`=1,
  `5`=1, `6`=1, `7`=1, `8`=1, `9`=1, `10`=1)

# Scale variables (mean 0, variance 1)
full_df$Internalizing_Symptom_Load <- scale(full_df$Overall_Anxious_Misery)
full_df$Self_Reliance <- scale(full_df$Self_Reliance)
full_df$Emotion_Regulation <- scale(full_df$Emotion_Regulation)
full_df$Confidence_in_Relationship <- scale(full_df$Confidence_in_Relationship)
full_df$Harmony_in_Relationship <- scale(full_df$Harmony_in_Relationship)
full_df$Positive_Neighborhood <- scale(full_df$Positive_Neighborhood)

# Null model
mod0 <- lm(Internalizing_Symptom_Load ~ Age + Female + White + Finished_College + Health,
  data=full_df)

# Adversity model
mod1 <- lm(Internalizing_Symptom_Load ~ Age + Female + White + Finished_College + Health +
  ACE_4Plus, data=full_df)

comp_mod0_mod1 <- anova(mod0, mod1)

# Recent Stressors model
mod2 <- lm(Internalizing_Symptom_Load ~ Age + Female + White + Finished_College + Health +
  ACE_4Plus + Job_Reduced + COVID_Test, data=full_df)

comp_mod1_mod2 <- anova(mod1, mod2)

# Resilience model
mod3 <- lm(Internalizing_Symptom_Load ~ Age + Female + White + Finished_College + Health +
  ACE_4Plus + Job_Reduced + COVID_Test + Self_Reliance + Emotion_Regulation +
  Confidence_in_Relationship + Harmony_in_Relationship + Positive_Neighborhood, data=full_df)

comp_mod2_mod3 <- anova(mod2, mod3)

# Interaction model
mod4 <- lm(Internalizing_Symptom_Load ~ Age + Female + White + Finished_College + Health +
  ACE_4Plus + Job_Reduced + COVID_Test + Self_Reliance + Emotion_Regulation +
  Confidence_in_Relationship + Harmony_in_Relationship + Positive_Neighborhood +
  ACE_4Plus:Emotion_Regulation, data=full_df)

comp_mod3_mod4 <- anova(mod3, mod4)


tab_model(mod0, mod1, mod2, mod3, mod4, file='~/Documents/traumaCOVID/tables/nestedModelsACE4PlusSensitivity.doc')
