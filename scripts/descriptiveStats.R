### This script creates an N table by subgroups and examines collinearity
### in proposed predictors. It also outputs a cleaned data.frame.
###
### Ellyn Butler
### May 7, 2020 - May 18, 2020

# Load libraries
library('dplyr')

# Load data
full_df <- read.csv("~/Documents/traumaCOVID/data/ACE_dataset_pulled_05062020.csv")
full_df$sex <- recode(full_df$sex, "1"="Female", "2"="Male", .default=NA_character_)

# Recode job labels

#(Probably):
# !!! ACE_2 (physical assault): Did a parent or other adult in the household often;  Push, grab, slap, or throw something at you?  or  Ever hit you so hard that you had marks or were injured?
# !!! ACE_3 (sexual assault): Did an adult or person at least 5 years older than you ever;  Touch or fondle you or have you touch their body in a sexual way?  or  Try to or actually have oral, anal, or vaginal sex with you?
# !!! ACE_5 (material deprivation): Did you often feel that; You didn't have enough to eat, had to wear dirty clothes, and had no one to protect you? or  Your parents were too drunk or high to take care of you or take you to the doctor if you needed it?
# !!! ACE_7 (witness physical assault): 7. Was your mother or stepmother:  Often pushed, grabbed, slapped, or had something thrown at her?  or  Sometimes or often kicked, bitten, hit with a fist, or hit with something hard?  or  Ever repeatedly hit over at least a few minutes or threatened with a gun or knife?


###########################  N tables ###########################

ace_df <- full_df[full_df$ACE_2 %in% c(0,1) & full_df$ACE_3 %in% c(0,1) &
  full_df$ACE_5 %in% c(0,1) & full_df$ACE_7 %in% c(0,1), c("randomID", "age", "sex",
  "race", "occupation", paste0("GAD_", 1:7), paste0("ISI_", 1:7), paste0("ACE_", 1:10))]

# ------------------- Co-occurrence of ACEs ------------------- #

ace_df$physical <- ace_df$ACE_2
ace_df$sexual <- ace_df$ACE_3
ace_df$material <- ace_df$ACE_5
ace_df$witness <- ace_df$ACE_7
combos <- c("none", "physical", "sexual", "material", "witness", "physical_sexual",
  "physical_material", "physical_witness", "sexual_material", "sexual_witness",
  "material_witness", "physical_sexual_material", "physical_sexual_witness",
  "physical_material_witness", "physical_sexual_material_witness")
allvars <- c("physical", "sexual", "material", "witness")

aceSummary <- function(ace_df) {
  ace_summary <- rep(NA, length(combos))
  names(ace_summary) <- combos
  ace_summary["none"] <- nrow(ace_df[ace_df$physical == 0 & ace_df$sexual == 0 &
    ace_df$material == 0 & ace_df$witness == 0,])
  for (combo in combos[2:length(combos)]) {
    thesevars <- strsplit(combo, "_")[[1]]
    tmp_df <- ace_df
    for (thisvar in thesevars) {
      tmp_df <- tmp_df[tmp_df[,thisvar] == 1,]
    }
    ace_summary[combo] <- nrow(tmp_df)
  }
  return(ace_summary)
}

# Before excluding healthcare workers
ace_summary <- aceSummary(ace_df)

# After excluding healthcare workers
ace_nohealth_df <- ace_df[
ace_summary_nohealth <- aceSummary(




##################### IVs correlation table #####################
