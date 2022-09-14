library(dplyr)
library(ggplot2)
library(ggpubr)
library(lme4)
library(simr)
library(pwr)

######## IMPORT DATA  ####

# crossover trial AB|BA
# csv: https://github.com/michaelfeurstein/sbl-experiment-pretest/blob/master/dataset.csv 

mydata <- read.csv("data_experiment.csv", sep = ";", dec = ",", header = TRUE)
mydata$time <- as.POSIXct(mydata$time, format="%M:%S")
mydata$duration.r <- as.numeric(format(mydata$time, "%M")) + as.numeric(format(mydata$time, "%S"))/60

# preparations for testing
# setup mapping and as.factor
syntaxMapping <- c("nl" = 1, "kv" = 2)
rankingMapping <- c("don't know" = 0, "2nd place" = 1, "1st place" = 2)
groupMapping <- c("beginner, student" = "bs", "advanced, student" = "as", "beginner, professional" = "bp", "advanced, professional" = "ap")

mydata$notation.r <- syntaxMapping[mydata$notation]
mydata$rank.r <- rankingMapping[mydata$rank]
mydata$group.r <- groupMapping[mydata$group.internal]

mydata$notation.r <- as.factor(mydata$notation.r)
mydata$notation.r <- factor(mydata$notation.r, levels = c("1", "2"), labels = c("natural language", "key-value"))
mydata$sequence <- as.factor(mydata$sequence)
mydata$sequence <- factor(mydata$sequence, levels = c("1", "2"), labels = c("NL-KV", "KV-NL"))
mydata$rank.r <- as.factor(mydata$rank.r)
mydata$rank.r <- factor(mydata$rank.r, levels = c("0", "1", "2"), labels = c("don't know", "2nd place", "1st place"))
mydata$period <- as.factor(mydata$period)
mydata$group.r <- as.factor(mydata$group.r)
mydata$group.r <- factor(mydata$group.r, levels = c("bs", "as", "bp", "ap"), labels = c("beginner-student", "advanced-student", "beginner-professional", "advanced-professional"))

# log transform duration
mydata$duration.log = log(mydata$duration.r)

# the actual dataframe we'll be working with
df <- subset(mydata, select = c("subject", "sequence", "period", "group.r", "notation.r", "duration.r", "duration.log", "accuracy", "sus", "rank.r"))

# write to csv so we don't need to run above lines too often
write.csv(df, "data_prepared.csv")

######## CROSS-OVER ANALYSIS ####
# Testing carry-over and treatment effect with t-tests
# Based on: https://www.lexjansen.com/pharmasug/2006/Posters/PO16.pdf

# calculate and plot sum of means for sequence,period
sp <- df %>%
  group_by(sequence, period) %>%
  summarize(mean_duration = mean(duration.log))

pd = position_dodge(0)
sp.plot <- ggplot(sp, aes(x = period,
               y = mean_duration,
               color = sequence,
               group = sequence)) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  geom_line() + theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4),
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  ) +
  ylab("Duration mean") +
  xlab("Period") +
  labs(color = "Sequence")

# calculate and plot sum of means for notation,period
np <- df %>%
  group_by(notation.r, period) %>%
  summarize(mean_duration = mean(duration.log))

pd = position_dodge(0)
np.plot <- ggplot(np, aes(x = period,
               y = mean_duration,
               color = notation.r,
               group = notation.r)) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  geom_line() + theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4),
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  ) +
  ylab("Duration mean") +
  xlab("Period") +
  labs(color = "Notation")

# arrange both plots in one figure
ggarrange(sp.plot, np.plot, nrow = 1, ncol = 2)

# estimate carry-over effect using sum values by t-test
# Null Hypothesis: there is a significant difference between NL-KV / KV-NL sequences --> this means there is a carry over effect (use only period 1)
# Alternative Hypothesis: there is no significant difference between NL-KV / KV-NL sequences --> this means there is NO carry over effect (use both periods)
# p-value > 0.05 shows possible carry-over effect is not significantly different between NL-KV / KV-NL sequences
t.test(mean_duration ~ sequence, data = sp)
t.test(mean_duration ~ notation.r, data = np)

# based on the result of the test update the dataframe
# option 1: leave it as it is because there is no carry over effect
# option 2: use only period 1 of dataframe, ergo remove period 2