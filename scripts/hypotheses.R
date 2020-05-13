### This script produces a bar plot that demonstrates what we expect to find
###
### Ellyn Butler
### May 7, 2020

library(ggplot2)

H_df <- data.frame(Adversity=c(rep("Abuse", 4), rep("Neglect", 4),
  rep("AbuseNeglect", 4), rep("Neither", 4)),
  Stress=rep(c("JobLoss", "Conflict", "JobLossConflict", "Neither"), 4),
  Value=c(9, 9, 10, 8, 6, 6, 7, 4, 9, 9, 10, 8, 5, 5, 6, 3))

H_df$Stress <- ordered(H_df$Stress, c("Neither", "JobLoss", "Conflict",
  "JobLossConflict"))
H_df$Adversity <- ordered(H_df$Adversity, c("Neither", "Neglect", "Abuse",
  "AbuseNeglect"))

p <- ggplot(H_df, aes(x=Adversity, y=Value, fill=Stress)) +
  theme_linedraw() + geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c("snow2", "thistle2", "turquoise3", "darkmagenta"))

pdf(file="~/Documents/traumaCOVID/plots/hypothesis.pdf", width=5, height=4)
p
dev.off()
