### This script creates an N table by subgroups and examines collinearity
### in proposed predictors. It also outputs a cleaned data.frame.
###
### Ellyn Butler
### May 7, 2020 - May 19, 2020

# Load libraries
library('dplyr')
library('ggcorrplot')
library('ggplot2')
library('ggpubr')

# Load data
full_df <- read.csv("~/Documents/traumaCOVID/data/ACE_dataset_pulled_05062020.csv")
full_df$sex <- recode(full_df$sex, "1"="Female", "2"="Male", .default=NA_character_)

# Recode job labels
full_df$occu_health <- recode(full_df$occupation, "2"="0", "3"="0", "4"="0",
  "5"="0", "6"="1", "9"="0", "10"="0", "11"="0", "12"="0", "13"="0",
  "14"="0", "15"="0", "16"="0", "17"="0", "18"="0", "19"="0", "20"="0",
  "1"="0", "7"="0", "21"="0", "22"="0", "8"="0", .default=NA_character_)
full_df$occu_health <- as.numeric(full_df$occu_health)

full_df$job_reduced <- recode(full_df$exp_job_loss, "0"="0", "4"="1", "2"="1",
  "1"="0", "3"="1", .default=NA_character_)
full_df <- full_df[!is.na(full_df$job_reduced),]
row.names(full_df) <- 1:nrow(full_df)
full_df$job_reduced <- as.numeric(full_df$job_reduced)

full_df$relationship_last <- full_df$pr_2

# !!! ACE_2 (physical assault): Did a parent or other adult in the household often;  Push, grab, slap, or throw something at you?  or  Ever hit you so hard that you had marks or were injured?
# !!! ACE_3 (sexual assault): Did an adult or person at least 5 years older than you ever;  Touch or fondle you or have you touch their body in a sexual way?  or  Try to or actually have oral, anal, or vaginal sex with you?
# !!! ACE_5 (material deprivation): Did you often feel that; You didn't have enough to eat, had to wear dirty clothes, and had no one to protect you? or  Your parents were too drunk or high to take care of you or take you to the doctor if you needed it?
# !!! ACE_7 (witness physical assault): 7. Was your mother or stepmother:  Often pushed, grabbed, slapped, or had something thrown at her?  or  Sometimes or often kicked, bitten, hit with a fist, or hit with something hard?  or  Ever repeatedly hit over at least a few minutes or threatened with a gun or knife?


###########################  N tables ###########################

ace_df <- full_df[full_df$ACE_2 %in% c(0,1) & full_df$ACE_3 %in% c(0,1) &
  full_df$ACE_5 %in% c(0,1) & full_df$ACE_7 %in% c(0,1), c("randomID", "age", "sex",
  "race", "relationship_last", "job_reduced", "occu_health", paste0("GAD_", 1:7),
  paste0("ISI_", 1:7), paste0("ACE_", 1:10))]
row.names(ace_df) <- 1:nrow(ace_df)

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

# After excluding healthcare workers -> Will definitely need to keep
ace_nohealth_df <- ace_df[ace_df$occu_health == 0 & !is.na(ace_df$occu_health),]
row.names(ace_nohealth_df) <- 1:nrow(ace_nohealth_df)
ace_summary_nohealth <- aceSummary(ace_nohealth_df)

# Exclude subjects from ace_df without occupation information
ace_df <- ace_df[!is.na(ace_df$occu_health),]
row.names(ace_df) <- 1:nrow(ace_df)

# Plots
n_df <- data.frame(Adversity=rep(c("physical", "sexual", "witness", "material"), 2),
  Happened=c(rep("Yes", 4), rep("No", 4)), N=c(nrow(ace_df[ace_df$physical == 1,]),
  nrow(ace_df[ace_df$sexual == 1,]), nrow(ace_df[ace_df$witness == 1,]),
  nrow(ace_df[ace_df$material == 1,]), nrow(ace_df[ace_df$physical == 0,]),
  nrow(ace_df[ace_df$sexual == 0,]), nrow(ace_df[ace_df$witness == 0,]),
  nrow(ace_df[ace_df$material == 0,])))
n_plot <- ggplot(n_df, aes(x=Happened, y=N)) + theme_linedraw() +
  geom_bar(stat="identity") + facet_grid(. ~ Adversity)

bar_plot <- ggplot(ace_df, aes(relationship_last)) + theme_linedraw() +
  geom_bar() + ggtitle("Relationship will last no matter what?") + xlab("Response")

##################### IVs correlation table #####################

corr <- round(cor(ace_df[, c("relationship_last", "job_reduced", "physical",
  "sexual", "witness", "material", "occu_health")]), digits=5)

corr_plot <- ggcorrplot(corr)


pdf(file="~/Documents/traumaCOVID/plots/corrIVs.pdf", width=10, height=8)
ggarrange(n_plot,
          ggarrange(corr_plot, bar_plot, ncol = 2, labels = c("B", "C")),
          nrow = 2,
          labels = "A"
          )
dev.off()

################## Resilience correlation table ##################

res_df <- full_df[full_df$ACE_2 %in% c(0,1) & full_df$ACE_3 %in% c(0,1) &
  full_df$ACE_5 %in% c(0,1) & full_df$ACE_7 %in% c(0,1),
  c("randomID", grep("sr_", names(full_df), value=TRUE),
  paste0("er_", 1:5), grep("pr_", names(full_df), value=TRUE),
  paste0("nr_", 1:5), paste0("nsc_", 1:4), paste0("ACE_", 1:10))]
row.names(res_df) <- 1:nrow(res_df)

corr_res <- round(cor(res_df[,!(names(res_df) %in% "randomID")]), digits=4)

corr_res_plot <- ggcorrplot(corr_res)

pdf(file="~/Documents/traumaCOVID/plots/aceRes.pdf", width=10, height=8)
corr_res_plot
dev.off()
