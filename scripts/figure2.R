### Anxious misery by resilience factor scores
###
### Ellyn Butler
### August 9, 2020

library('dplyr')
library('ggplot2')
library('ggpubr')
library('reshape2')

full_df <- read.csv('~/Documents/traumaCOVID/data/cleandata_2020-08-07.csv')

resilience <- c('Self_Reliance', 'Emotion_Dysregulation', 'Positive_Relationships',
  'Negative_Relationships', 'Neighborhood_Fears')

full_df[,c('Overall_Anxious_Misery', resilience)] <-
  sapply(full_df[,c('Overall_Anxious_Misery', resilience)], scale)

full_df$ACE <- recode(full_df$ACE_sum, `0`='No', `1`='Yes', `2`='Yes', `3`='Yes',
 `4`='Yes', `5`='Yes', `6`='Yes', `7`='Yes', `8`='Yes', `9`='Yes', `10`='Yes')

for (res in resilience) {
  tmp_df <- full_df
  names(tmp_df)[names(tmp_df) == res] <- 'resvar'
  res_plot <- ggplot(tmp_df, aes(x=resvar, y=Overall_Anxious_Misery, colour=ACE)) +
    theme_linedraw() + geom_point(alpha=.2) + geom_smooth(method = "lm", fill = NA) +
    scale_color_manual(values=c('plum2', 'red'))+
    labs(y='Anxious Misery', x=gsub('_', ' ', res))
  assign(paste0(res, '_plot'), res_plot)
}

pdf(file='~/Documents/traumaCOVID/plots/figure2.pdf', width=12, height=6)
ggarrange(Emotion_Dysregulation_plot, Self_Reliance_plot,
  Positive_Relationships_plot, Negative_Relationships_plot, Neighborhood_Fears_plot,
  nrow = 2, ncol = 3,  labels = c('A', 'B', 'C', 'D', 'E'))
dev.off()
