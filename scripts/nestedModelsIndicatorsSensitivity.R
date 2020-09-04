### Hypothesized models, with Null Model containing demographics
###
### Ellyn Butler
### June 16, 2020

# Load libraries
library('ggplot2')
library('gridExtra')
library('dplyr')
library('gt')
library('sjPlot')


# Load data
full_df <- read.csv("~/Documents/traumaCOVID/data/cleandata_2020-08-07.csv")
full_df$Finished_College <- recode(full_df$edu, "LessThanHS"=0, "HS"=0,
  "SomeCollege"=0, "College"=1, "Masters"=1, "Doctorate"=1)
names(full_df)[names(full_df) == "race_white"] <- "White"
full_df$Female <- recode(full_df$sex, "Female"=1, "Male"=0)
full_df$Health <- recode(full_df$occu_health, "Yes"=1, "No"=0)
names(full_df)[names(full_df) == "age"] <- "Age"
full_df$Threat <- recode(full_df$exp_threat, "Yes"=1, "No"=0)
full_df$Deprivation <- recode(full_df$exp_deprivation, "Yes"=1, "No"=0)
full_df$Instability <- recode(full_df$exp_undepend, "Yes"=1, "No"=0)
full_df$Job_Reduced <- recode(full_df$exp_job_reduce, "Yes"=1, "No"=0)
full_df$COVID_Test <- recode(full_df$exp_test, "Yes"=1, "No"=0)

# Scale variables (mean 0, variance 1)
full_df$Overall_Anxious_Misery <- scale(full_df$Overall_Anxious_Misery)
full_df$Self_Reliance <- scale(full_df$Self_Reliance)
full_df$Emotion_Dysregulation <- scale(full_df$Emotion_Dysregulation)
full_df$Positive_Relationships <- scale(full_df$Positive_Relationships)
full_df$Negative_Relationships <- scale(full_df$Negative_Relationships)
full_df$Neighborhood_Fears <- scale(full_df$Neighborhood_Fears)

# Null model
mod0 <- lm(Overall_Anxious_Misery ~ Age + Female + White + Finished_College + Health,
  data=full_df)

# Adversity model
mod1 <- lm(Overall_Anxious_Misery ~ Age + Female + White + Finished_College + Health +
  Threat + Deprivation + Instability, data=full_df)

comp_mod0_mod1 <- anova(mod0, mod1)

# Recent Stressors model
mod2 <- lm(Overall_Anxious_Misery ~ Age + Female + White + Finished_College + Health +
  Threat + Deprivation + Instability + Job_Reduced  + COVID_Test,
  data=full_df)

comp_mod1_mod2 <- anova(mod1, mod2)

# Resilience model
mod3 <- lm(Overall_Anxious_Misery ~ Age + Female + White + Finished_College + Health +
  Threat + Deprivation + Instability + Job_Reduced  +
  COVID_Test + Self_Reliance + Emotion_Dysregulation + Positive_Relationships +
  Negative_Relationships + Neighborhood_Fears, data=full_df)

comp_mod2_mod3 <- anova(mod2, mod3)

# Interaction model
mod4 <- lm(Overall_Anxious_Misery ~ Age + Female + White + Finished_College + Health +
  Threat + Deprivation + Instability +
  Job_Reduced  + COVID_Test + Self_Reliance + Emotion_Dysregulation +
  Positive_Relationships + Negative_Relationships + Neighborhood_Fears +
  Threat:Emotion_Dysregulation + Deprivation:Emotion_Dysregulation +
  Instability:Emotion_Dysregulation, data=full_df)

comp_mod3_mod4 <- anova(mod3, mod4)

mod_table <- tab_model(mod0, mod1, mod2, mod3, mod4)
#page.complete
#http://www.sussex.ac.uk/its/help/faq?faqid=630
