### Hypothesized models
###
### Ellyn Butler
### May 31, 2020

# Load libraries
library('ggplot2')
library('gridExtra')
library('gt')

# Load data
full_df <- read.csv("~/Documents/traumaCOVID/data/cleandata_2020-05-31.csv")


# Adversity model
mod1 <- lm(Overall_Anxious_Misery ~ I(exp_threat) + I(exp_deprivation) + I(exp_undepend),
  data=full_df)

# Recent Stressors model
mod2 <- lm(Overall_Anxious_Misery ~ I(exp_threat) + I(exp_deprivation) + I(exp_undepend) +
  exp_job_reduce + exp_test, data=full_df)

comp_mod1_mod2 <- anova(mod1, mod2)

# Resilience model
mod3 <- lm(Overall_Anxious_Misery ~ I(exp_threat) + I(exp_deprivation) + I(exp_undepend) +
  exp_job_reduce + exp_test + Self_Reliance + Emotion_Dysregulation +
  Positive_Relationships + Negative_Relationships + Neighborhood_Fears, data=full_df)

comp_mod2_mod3 <- anova(mod2, mod3)

# Interaction model
mod4 <- lm(Overall_Anxious_Misery ~ I(exp_threat) + I(exp_deprivation) + I(exp_undepend) +
  exp_job_reduce + exp_test + Self_Reliance + Emotion_Dysregulation +
  Positive_Relationships + Negative_Relationships + Neighborhood_Fears +
  I(exp_threat)*Emotion_Dysregulation + I(exp_deprivation)*Emotion_Dysregulation,
  I(exp_undepend)*Emotion_Dysregulation, data=full_df)

comp_mod3_mod4 <- anova(mod3, mod4)
