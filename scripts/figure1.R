### This script creates the first figure for the manuscript
###
### Ellyn Butler
### August 9, 2020

library('dplyr')
library('ggplot2')
library('ggpubr')
library('reshape2')
library('ggcorrplot')

full_df <- read.csv('~/Documents/traumaCOVID/data/cleandata_2020-08-07.csv')

full_df$Overall_Anxious_Misery <- scale(full_df$Overall_Anxious_Misery)

##################### Panel A #####################

meanInt <- function(i) mean(full_df[full_df[, i] == 'Yes', 'Overall_Anxious_Misery'])
ses2Int <- function(i) (sd(full_df[full_df[, i] == 'Yes',
  'Overall_Anxious_Misery'])/sqrt(nrow(full_df[full_df[, i] == 'Yes', 'Overall_Anxious_Misery'])))*2

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
  labs(title='Anxious Misery by Adverse Childhood Experiences', y='Anxious Misery (95% CI)') +
  theme(legend.position='none', axis.text.x = element_text(angle = 45, hjust = 1, size=12),
    axis.text.y=element_text(size=12), axis.title=element_text(size=14))


##################### Panel B #####################

panel_b <- ggplot(full_df, aes(x=age, y=Overall_Anxious_Misery, colour=exp_job_reduce)) +
  theme_linedraw() + geom_smooth(method = "lm", fill = NA) + geom_point(alpha=.2) +
  scale_color_manual(values=c('plum2', 'red')) + theme(legend.position='top') +
  labs(title='Age by Anxious Misery', x='Age', y='Anxious Misery', colour='Job hours reduced')


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
