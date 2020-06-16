### Hypothesized models
###
### Ellyn Butler
### May 31, 2020 - June 1, 2020

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
  I(exp_job_reduce) + I(exp_test), data=full_df)

comp_mod1_mod2 <- anova(mod1, mod2)

# Resilience model
mod3 <- lm(Overall_Anxious_Misery ~ I(exp_threat) + I(exp_deprivation) + I(exp_undepend) +
  I(exp_job_reduce) + I(exp_test) + Self_Reliance + Emotion_Dysregulation +
  Positive_Relationships + Negative_Relationships + Neighborhood_Fears, data=full_df)

comp_mod2_mod3 <- anova(mod2, mod3)

# Interaction model
mod4 <- lm(Overall_Anxious_Misery ~ I(exp_threat) + I(exp_deprivation) + I(exp_undepend) +
  I(exp_job_reduce) + I(exp_test) + Self_Reliance + Emotion_Dysregulation +
  Positive_Relationships + Negative_Relationships + Neighborhood_Fears +
  I(exp_threat):Emotion_Dysregulation + I(exp_deprivation):Emotion_Dysregulation +
  I(exp_undepend):Emotion_Dysregulation, data=full_df)

comp_mod3_mod4 <- anova(mod3, mod4)


sum_df <- data.frame(Variables=c("Threat", "Deprivation", "Undependabililty",
  "Job reduction", "COVID test", "Self Reliance", "Emotion Dysregulation",
  "Positive Relationships", "Negative Relationships", "Neighborhood Fears",
  "Threat X Emotion Dysregulation", "Deprivation X Emotion Dysregulation",
  "Undependability X Emotion Dysregulation", "Model R^2"))

for (i in c(1, 4, 6, 11)) {
  if (i == 1) {
    for (modnum in 1:4) {
      sum_df[, paste0("Model", modnum, "_T")] <- ""
      for (thiseffect in 2:4) {
        mod <- get(paste0("mod", modnum))

        if (summary(mod)$coefficients[thiseffect, 4] <= .001) { pstar <- "***"
        } else if (summary(mod)$coefficients[thiseffect, 4] <= .01) { pstar <- "**"
        } else if (summary(mod)$coefficients[thiseffect, 4] <= .05) { pstar <- "*"
        } else if (summary(mod)$coefficients[thiseffect, 4] > .05) { pstar <- "" }
        sum_df[thiseffect-1, paste0("Model", modnum, "_T")] <- paste0(round(summary(mod)$coefficients[thiseffect, 3], digits=3), pstar)
      }
      sum_df[14, paste0("Model", modnum, "_T")] <- round(summary(mod)$r.squared, digits=3)
    }
  } else if (i == 4) {
    for (modnum in 2:4) {
      for (thiseffect in 5:6) {
        mod <- get(paste0("mod", modnum))

        if (summary(mod)$coefficients[thiseffect, 4] <= .001) { pstar <- "***"
        } else if (summary(mod)$coefficients[thiseffect, 4] <= .01) { pstar <- "**"
        } else if (summary(mod)$coefficients[thiseffect, 4] <= .05) { pstar <- "*"
        } else if (summary(mod)$coefficients[thiseffect, 4] > .05) { pstar <- "" }
        sum_df[thiseffect-1, paste0("Model", modnum, "_T")] <- paste0(round(summary(mod)$coefficients[thiseffect, 3], digits=3), pstar)
      }
    }
  } else if (i == 6) {
    for (modnum in 3:4) {
      for (thiseffect in 7:11) {
        mod <- get(paste0("mod", modnum))

        if (summary(mod)$coefficients[thiseffect, 4] <= .001) { pstar <- "***"
        } else if (summary(mod)$coefficients[thiseffect, 4] <= .01) { pstar <- "**"
        } else if (summary(mod)$coefficients[thiseffect, 4] <= .05) { pstar <- "*"
        } else if (summary(mod)$coefficients[thiseffect, 4] > .05) { pstar <- "" }
        sum_df[thiseffect-1, paste0("Model", modnum, "_T")] <- paste0(round(summary(mod)$coefficients[thiseffect, 3], digits=3), pstar)
      }
    }
  } else {
    modnum <- 4
    for (thiseffect in 12:14) {
      mod <- get(paste0("mod", modnum))

      if (summary(mod)$coefficients[thiseffect, 4] <= .001) { pstar <- "***"
      } else if (summary(mod)$coefficients[thiseffect, 4] <= .01) { pstar <- "**"
      } else if (summary(mod)$coefficients[thiseffect, 4] <= .05) { pstar <- "*"
      } else if (summary(mod)$coefficients[thiseffect, 4] > .05) { pstar <- "" }
      sum_df[thiseffect-1, paste0("Model", modnum, "_T")] <- paste0(round(summary(mod)$coefficients[thiseffect, 3], digits=3), pstar)
    }
  }
}

sum_table <- gt(sum_df)

gtsave(sum_table, "~/Documents/traumaCOVID/plots/nestedModelsTable.pdf")
