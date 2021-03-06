### This script creates the first figure for the manuscript
###
### Ellyn Butler
### August 9, 2020 - August 22, 2020

library('dplyr')
library('ggplot2')
library('ggpubr')
library('reshape2')
library('ggcorrplot')
library('fastDummies')

full_df <- read.csv('~/Documents/traumaCOVID/data/cleandata_2020-08-07.csv')

full_df$Overall_Anxious_Misery <- scale(full_df$Overall_Anxious_Misery)

##################### Panel A #####################

meanInt <- function(i) mean(full_df[full_df[, i] == 'Yes', 'Overall_Anxious_Misery'])
ses2Int <- function(i) (sd(full_df[full_df[, i] == 'Yes',
  'Overall_Anxious_Misery'])/sqrt(nrow(full_df[full_df[, i] == 'Yes', ])))*2

intmeans <- sapply(paste0('ACE_', 1:10), meanInt)
intses <- sapply(paste0('ACE_', 1:10), ses2Int)
ace_df <- data.frame(ACEs=ordered(paste0('ACE_', 1:10)), IntMean=intmeans, IntSE2=intses)
ace_df$ACEs <- ordered(ace_df$ACEs, c(paste0('ACE_', 1:9), 'ACE_10'))

panel_a <- ggplot(ace_df, aes(x=ACEs, y=IntMean, fill=IntMean)) + theme_linedraw() +
  geom_bar(stat='identity') + scale_fill_gradient(low='yellow', high='red') +
  geom_errorbar(aes(ymin=IntMean-IntSE2, ymax=IntMean+IntSE2), width=.1) +
  scale_x_discrete(labels=c('ACE_1'='1: Verbal', 'ACE_2'='2: Physical',
    'ACE_3'='3: Sexual', 'ACE_4'='4: No love', 'ACE_5'='5: Neglect',
    'ACE_6'='6: Divorce', 'ACE_7'='7: Witness', 'ACE_8'='8: Substances',
    'ACE_9'='9: Mental illness', 'ACE_10'='10: Prison')) +
  labs(title='Internalizing Symptom Load by Adverse Childhood Experiences', y='Internalizing Symptom Load (95% CI)') +
  theme(legend.position='none', axis.text.x = element_text(angle = 45, hjust = 1, size=12),
    axis.text.y=element_text(size=12), axis.title=element_text(size=14))


##################### Panel B #####################

panel_b <- ggplot(full_df, aes(x=age, y=Overall_Anxious_Misery, colour=exp_job_reduce)) +
  theme_linedraw() + geom_smooth(method = "lm", fill = NA) + geom_point(alpha=.2) +
  scale_color_manual(values=c('plum2', 'red')) + theme(legend.position='top') +
  labs(title='Age by Internalizing Symptom Load', x='Age', y='Internalizing Symptom Load', colour='Job hours reduced')


##################### Panel C #####################

recodeAce <- function(i) { recode(full_df[, i], 'Yes'=1, 'No'=0) }
full_df[, paste0('ACE_', 1:10)] <- sapply(paste0('ACE_', 1:10), recodeAce)

corr <- cor(full_df[, paste0('ACE_', 1:10)])
panel_c <- ggcorrplot(corr, type='lower', lab=TRUE) +
  scale_x_discrete(labels=c('ACE_1'='1: Verbal', 'ACE_2'='2: Physical',
    'ACE_3'='3: Sexual', 'ACE_4'='4: No love', 'ACE_5'='5: Neglect',
    'ACE_6'='6: Divorce', 'ACE_7'='7: Witness', 'ACE_8'='8: Substances',
    'ACE_9'='9: Mental illness', 'ACE_10'='10: Prison')) +
  scale_y_discrete(labels=c('ACE_1'='1: Verbal', 'ACE_2'='2: Physical',
    'ACE_3'='3: Sexual', 'ACE_4'='4: No love', 'ACE_5'='5: Neglect',
    'ACE_6'='6: Divorce', 'ACE_7'='7: Witness', 'ACE_8'='8: Substances',
    'ACE_9'='9: Mental illness', 'ACE_10'='10: Prison')) +
  theme(legend.position='none')


###################################################


pdf(file='~/Documents/traumaCOVID/plots/figure1.pdf', width=10, height=10)
ggarrange(panel_a, ggarrange(panel_b, panel_c, ncol = 2, labels = c('B', 'C')),
          nrow = 2,  labels = 'A')
dev.off()


###################################################
###################################################
###################################################

######## Potential replacement for Panel A ########
meanInt <- function(i) mean(full_df[full_df[, i] == 1, 'Overall_Anxious_Misery'])
ses2Int <- function(i) (sd(full_df[full_df[, i] == 1,
  'Overall_Anxious_Misery'])/sqrt(nrow(full_df[full_df[, i] == 1, ])))*2

full_df <- fastDummies::dummy_cols(full_df, select_columns='ACE_sum')
full_df$ACE_sum_4 <- rowSums(full_df[, paste0('ACE_sum_', 5:10)])

intmeans <- sapply(paste0('ACE_sum_', 0:4), meanInt)
intses <- sapply(paste0('ACE_sum_', 0:4), ses2Int)
ace2_df <- data.frame(ACEs=ordered(paste0('ACE_sum_', 0:4)), IntMean=intmeans, IntSE2=intses)
#ace2_df$ACEs <- ordered(ace_df$ACEs, paste0('ACE_sum_', 0:4))

panel_a1 <- ggplot(ace2_df, aes(x=ACEs, y=IntMean, fill=IntMean)) + theme_linedraw() +
  geom_bar(stat='identity') + scale_fill_gradient(low='yellow', high='red') +
  geom_errorbar(aes(ymin=IntMean-IntSE2, ymax=IntMean+IntSE2), width=.1) +
  scale_x_discrete(labels=c('ACE_sum_0'='0', 'ACE_sum_1'='1', 'ACE_sum_2'='2',
    'ACE_sum_3'='3', 'ACE_sum_4'='4+')) +
  labs(title='Internalizing Symptom Load by Number of Types of ACEs', y='Internalizing Symptom Load (95% CI)') +
  theme(legend.position='none', axis.text.x = element_text(size=12),
    axis.text.y=element_text(size=12), axis.title=element_text(size=14))



meanInt <- function(i) mean(full_df[full_df[, i] == 'Yes', 'Overall_Anxious_Misery'])
ses2Int <- function(i) (sd(full_df[full_df[, i] == 'Yes',
  'Overall_Anxious_Misery'])/sqrt(nrow(full_df[full_df[, i] == 'Yes', ])))*2
types <- c('exp_threat', 'exp_deprivation', 'exp_undepend')
intmeans <- sapply(types, meanInt)
intses <- sapply(types, ses2Int)
ace3_df <- data.frame(ACEs=types, IntMean=intmeans, IntSE2=intses)
ace3_df$ACEs <- ordered(ace3_df$ACEs, types)

panel_a2 <- ggplot(ace3_df, aes(x=ACEs, y=IntMean, fill=IntMean)) + theme_linedraw() +
  geom_bar(stat='identity') + scale_fill_gradient(low='yellow', high='red') +
  geom_errorbar(aes(ymin=IntMean-IntSE2, ymax=IntMean+IntSE2), width=.1) +
  scale_x_discrete(labels=c('exp_threat'='Threat', 'exp_deprivation'='Deprivation',
    'exp_undepend'='Instability')) +
  labs(title='Internalizing Symptom Load by Types of ACEs', y='Internalizing Symptom Load (95% CI)') +
  theme(legend.position='none', axis.text.x = element_text(size=12),
    axis.text.y=element_text(size=12), axis.title=element_text(size=14))


pdf(file='~/Documents/traumaCOVID/plots/figure1_altA.pdf', width=10, height=5)
ggarrange(panel_a1, panel_a2, ncol=2)
dev.off()
