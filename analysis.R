library(dplyr)
library(ggplot2)
library(ggpubr)
library(lme4)
library(lmerTest)
library(simr)
library(pwr)
library(effectsize)
library(MASS)

### IMPORT DATA  ####

# crossover trial AB|BA
# csv: https://github.com/michaelfeurstein/sbl-experiment-pretest/blob/master/dataset.csv 

mydata <- read.csv("data_export.csv", sep = ";", dec = ",", header = TRUE)
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
mydata$rank.r <- factor(mydata$rank.r, levels = c("0", "1", "2"), labels = c("don't know", "2nd place", "1st place"))
mydata$rank.r <- as.integer(mydata$rank.r)
mydata$period <- as.factor(mydata$period)
mydata$group.r <- as.factor(mydata$group.r)
mydata$group.r <- factor(mydata$group.r, levels = c("bs", "as", "bp", "ap"), labels = c("beginner-student", "advanced-student", "beginner-professional", "advanced-professional"))

# log transform duration
mydata$duration.log = log(mydata$duration.r)

# the actual dataframe we'll be working with
df <- subset(mydata, select = c("subject", "sequence", "period", "group.r", "notation.r", "duration.r", "duration.log", "accuracy", "sus", "rank.r"))

# write to csv so we don't need to run above lines too often
write.csv(df, "data_prepared.csv")

################################# #
################################# #

### SUMMARY OF DATA  ####
#### duration: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration.log), sd_duration = sd(duration.log))

df %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration.r), sd_duration = sd(duration.r))

#### accuracy: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_accuracy = mean(accuracy), sd_duration = sd(accuracy))

#### sus scores: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_sus_score = mean(sus), sd_sus_score = sd(sus))

#### ranks ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_rank = mean(rank.r), sd_rank = sd(rank.r))

### VISUALS and TESTS on data ####
# based on: https://www.epa.gov/sites/default/files/2016-06/documents/normality.pdf 

#### Normality ####
##### duration ####
# (1) testing assumption of normality visually
# histogram on duration
hist(df$duration.r[df$notation.r == "natural language"], main = "Natural Language", xlab = "Duration in minutes")
hist(df$duration.r[df$notation.r == "key-value"], main = "Key-Value", xlab = "Duration in minutes")

# histogram on log transformed duration
hist(df$duration.log[df$notation.r == "natural language"], main = "Natural Language", xlab = "Duration in minutes log transformed")
hist(df$duration.log[df$notation.r == "key-value"], main = "Key-Value", xlab = "Duration in minutes log transformed")

# REPORT #
# log transformed duration fits better for the assumption of normality

# (2) testing assumption of normality with shapiro-wilk normality test
shapiro.test(df$duration.log[df$notation.r == "natural language"])
shapiro.test(df$duration.log[df$notation.r == "key-value"])

# REPORT #
# log transformed duration results in shapiro-wilk not rejecting the H0 (the data is from normal distribution)

##### sus ####
# (1) testing assumption of normality visually
# histogram on sus score
hist(df$sus[df$notation.r == "natural language"], main = "Natural Language", xlab = "SUS score")
hist(df$sus[df$notation.r == "key-value"], main = "Key-Value", xlab = "SUS score")

# (2) testing assumption of normality with shapiro-wilk normality test
shapiro.test(df$sus[df$notation.r == "natural language"])
shapiro.test(df$sus[df$notation.r == "key-value"])

# REPORT #
# "Raw SUS scores aren’t normally distributed but the sample mean is"
# Source: https://measuringu.com/10-things-sus/

##### accuracy ####
# (1) testing assumption of normality visually
# histogram on accuracy
hist(df$accuracy[df$notation.r == "natural language"], main = "Natural Language", xlab = "Accuracy")
hist(df$accuracy[df$notation.r == "key-value"], main = "Key-Value", xlab = "Accuracy")

# REPORT #
# 

# (2) testing assumption of normality with shapiro-wilk normality test
shapiro.test(df$accuracy[df$notation.r == "natural language"])
shapiro.test(df$accuracy[df$notation.r == "key-value"])

# REPORT #
# 

# boxplots

boxplot(sus ~ notation.r, data = df, xlab = "Notation", ylab = "SUS score", names = c("1" = "NL", "2" = "KV"))
boxplot(accuracy ~ notation.r, data = df, xlab = "Notation", ylab = "Accuracy", names = c("1" = "NL", "2" = "KV"))

### TRANSFORMATION ####
#### duration ####
# see above / duration is log transformed

#### accuracy ####
# I need help transforming accuracy
nl_acc<-df$accuracy[df$notation.r == "natural language"]
hist(nl_acc)

# transformation trial and error
# link on transform based on skew
# https://anatomisebiostats.com/biostatistics-blog/transforming-skewed-data/
# link on how to reflect
# https://www.r-bloggers.com/2020/01/a-guide-to-data-transformation/

# reflect data (add 1 and substract from maximum and use absolute values)
# original data is negative skewed (left tail) (also sehr viele werte bei 100 herum) (richtig ?)
nl_acc_t0<-abs((nl_acc+1)-100)
hist(nl_acc_t0)

# 1st try
# starke transformation mit log base 10
nl_acc_t<-log10(nl_acc_t0)
plot(density(nl_acc_t))
hist(nl_acc_t)
shapiro.test(nl_acc_t)

# 2nd try
# boxcox transform
nl_acc_bc<-boxcox(nl_acc~1,lambda=seq(-2,4,0.01))
nl_acc_bc_t<-(nl_acc^3.2-1)/3.2
plot(density(nl_acc_bc_t))
hist(nl_acc_bc_t)
shapiro.test(nl_acc_bc_t)

#### sus ####
# natural-langauge sus transformation
# step 1 the actual sus scores are not normal 
nl_sus<-df$sus[df$notation.r == "natural language"]
hist(nl_sus)
# using a box cox transformation

# exploring different lambdas for best fit
bc1<-boxcox(nl_sus~1,lambda=seq(-2,4,0.01))
# from bc1 plot we can say that possibly 2 fits well also in terms of not too complex transformation such as 2.3

# transformation with actual formula
nl_sus_t<-(nl_sus^3.2-1)/3.2
# two plots showing the same yt in different ways
plot(density(nl_sus_t))
hist(nl_sus_t)

shapiro.test(nl_sus_t)
shapiro.test(nl_sus)

# key-value sus transformation
kv_sus<-df$sus[df$notation.r == "key-value"]
hist(kv_sus)
# using a box cox transformation
library(MASS)
# exploring different lambdas for best fit
bc1<-boxcox(kv_sus~1,lambda=seq(-2,4,0.01))
# from bc1 plot we can say that possibly 2 fits well also in terms of not too complex transformation such as 2.3

# transformation with actual formula
kv_sus_t<-(kv_sus^3.2-1)/3.2
# two plots showing the same yt in different ways
plot(density(kv_sus_t))
hist(kv_sus_t)

shapiro.test(kv_sus_t)
shapiro.test(kv_sus)

# persist transformed sus into dataframe
df$sust<-(df$sus^3.2-1)/3.2

### CROSS-OVER ANALYSIS ####
# Testing carry-over and treatment effect with t-tests
# Based on: https://www.lexjansen.com/pharmasug/2006/Posters/PO16.pdf

#### duration  ####
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

#### sus  ####
# calculate and plot sum of means for sequence,period
sp <- df %>%
  group_by(sequence, period) %>%
  summarize(mean_sus = mean(sus))

pd = position_dodge(0)
sp.plot <- ggplot(sp, aes(x = period,
                          y = mean_sus,
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
  ylab("SUS score") +
  xlab("Period") +
  labs(color = "Sequence")

# calculate and plot sum of means for notation,period
np <- df %>%
  group_by(notation.r, period) %>%
  summarize(mean_sus = mean(sus))

pd = position_dodge(0)
np.plot <- ggplot(np, aes(x = period,
                          y = mean_sus,
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
  ylab("SUS score") +
  xlab("Period") +
  labs(color = "Notation")

# arrange both plots in one figure
ggarrange(sp.plot, np.plot, nrow = 1, ncol = 2)

# estimate carry-over effect using sum values by t-test
# Null Hypothesis: there is a significant difference between NL-KV / KV-NL sequences --> this means there is a carry over effect (use only period 1)
# Alternative Hypothesis: there is no significant difference between NL-KV / KV-NL sequences --> this means there is NO carry over effect (use both periods)
# p-value > 0.05 shows possible carry-over effect is not significantly different between NL-KV / KV-NL sequences
t.test(mean_sus ~ sequence, data = sp)
t.test(mean_sus ~ notation.r, data = np)

#### accuracy  ####
# calculate and plot sum of means for sequence,period
sp <- df %>%
  group_by(sequence, period) %>%
  summarize(mean_accuracy = mean(accuracy))

pd = position_dodge(0)
sp.plot <- ggplot(sp, aes(x = period,
                          y = mean_accuracy,
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
  ylab("Accuracy") +
  xlab("Period") +
  labs(color = "Sequence")

# calculate and plot sum of means for notation,period
np <- df %>%
  group_by(notation.r, period) %>%
  summarize(mean_accuracy = mean(accuracy))

pd = position_dodge(0)
np.plot <- ggplot(np, aes(x = period,
                          y = mean_accuracy,
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
  ylab("Accuracy") +
  xlab("Period") +
  labs(color = "Notation")

# arrange both plots in one figure
ggarrange(sp.plot, np.plot, nrow = 1, ncol = 2)

# estimate carry-over effect using sum values by t-test
# Null Hypothesis: there is a significant difference between NL-KV / KV-NL sequences --> this means there is a carry over effect (use only period 1)
# Alternative Hypothesis: there is no significant difference between NL-KV / KV-NL sequences --> this means there is NO carry over effect (use both periods)
# p-value > 0.05 shows possible carry-over effect is not significantly different between NL-KV / KV-NL sequences
t.test(mean_accuracy ~ sequence, data = sp)
t.test(mean_accuracy ~ notation.r, data = np)

### ANALYSIS ####
# Based on paper and script by Johanson & Hasselbring (2017)

#### DURATION  ####
# time spent on solving the coding task (creation of video-based learning module)

##### boxplot ####
boxplot(duration.r ~ notation.r, data = df, xlab = "Notation", ylab = "Duration", names = c("1" = "NL", "2" = "KV"))
boxplot(duration.log ~ notation.r, data = df, xlab = "Notation", ylab = "Duration", names = c("1" = "NL", "2" = "KV"))

##### histogram ####
hist(df$duration.r[df$notation.r == "natural language"])
hist(df$duration.log[df$notation.r == "natural language"])

hist(df$duration.r[df$notation.r == "key-value"])
hist(df$duration.log[df$notation.r == "key-value"])

##### interaction plot ####
interaction.plot(factor(df$period),df$notation,df$duration.r)

##### mean ####
df %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration.r), sd_duration = sd(duration.r), median_duration = median(duration.r))

##### mean difference ####
# values
m.nl <- mean(df$duration.r[df$notation.r == "natural language"])
m.kv <- mean(df$duration.r[df$notation.r == "key-value"])
# difference between nl kv: nl-kv
diffnlkv <- m.nl-m.kv
# avergae of nl kv
avgnlkv <- (m.nl+m.kv)/2
rationlkv <- diffnlkv/avgnlkv
# percentage
percent_diff_nlkv <- rationlkv*100

print(c("mean difference in percentage: ", percent_diff_nlkv))

##### shapiro ####
shapiro.test(df$duration.log[df$notation.r == "natural language"])
shapiro.test(df$duration.log[df$notation.r == "key-value"])

##### t-test ####
t.test(df$duration.log[df$notation.r == "natural language"],df$duration.log[df$notation.r == "key-value"], paired = TRUE, conf.level = 0.95)

##### wilcoxon ####
wilcox.test(df$duration.log[df$notation.r == "natural language"],df$duration.log[df$notation.r == "key-value"], paired = TRUE, conf.int=TRUE, conf.level = 0.95)

##### effect size ####
cohens_d(df$duration.log[df$notation.r == "natural language"],df$duration.log[df$notation.r == "key-value"], paired = TRUE)

##### linear regression ####
m4<-lmer(duration.log~notation.r+factor(period)+(1|subject),data=df)
anova(m4)
summary(m4)

qqnorm(df$duration.log, pch = 1, frame = FALSE)
qqline(df$duration.log, col = "steelblue", lwd = 2)
plot(df$duration.log, resid(m4))
abline(0, 0) 

#### ACCURACY  ####
# correctness of result from coding task (creation of video-based learning module)

##### boxplot ####
boxplot(accuracy ~ notation.r, data = df, xlab = "Notation", ylab = "Accuracy", names = c("1" = "NL", "2" = "KV"))

##### mean ####
df %>%
  group_by(notation.r) %>%
  summarize(mean_accuracy = mean(accuracy), sd_accuracy = sd(accuracy), median_accuracy = median(accuracy))

##### mean difference ####
# values
accuracy_nl <- mean(df$accuracy[df$notation.r == "natural language"])
accuracy_kv <- mean(df$accuracy[df$notation.r == "key-value"])
# difference between nl kv: nl-kv
accuracy_diffnlkv <- accuracy_nl-accuracy_kv
# avergae of nl kv
accuracy_avgnlkv <- (accuracy_nl+accuracy_kv)/2
accuracy_rationlkv <- accuracy_diffnlkv/accuracy_avgnlkv
# percentage
accuracy_percent_diff_nlkv <- abs(accuracy_rationlkv*100)

print(c("mean difference in percentage: ", accuracy_percent_diff_nlkv))

##### shapiro ####
shapiro.test(df$accuracy[df$notation.r == "natural language"])
shapiro.test(df$accuracy[df$notation.r == "key-value"])

##### t-test ####
t.test(df$accuracy[df$notation.r == "natural language"],df$accuracy[df$notation.r == "key-value"], paired = TRUE, conf.level = 0.95)

##### wilcoxon ####
wilcox.test(df$accuracy[df$notation.r == "natural language"],df$accuracy[df$notation.r == "key-value"], paired = TRUE, conf.int=TRUE, conf.level = 0.95)

##### effect size ####
cohens_d(df$accuracy[df$notation.r == "natural language"],df$accuracy[df$notation.r == "key-value"], paired = TRUE)

#### SUS  ####
# correctness of result from coding task (creation of video-based learning module)

##### boxplot ####
boxplot(sus ~ notation.r, data = df, xlab = "Notation", ylab = "Accuracy", names = c("1" = "NL", "2" = "KV"))
boxplot(sust ~ notation.r, data = df, xlab = "Notation", ylab = "Accuracy", names = c("1" = "NL", "2" = "KV"))

##### histogram ####
hist(df$sus[df$notation.r == "natural language"])
hist(df$sust[df$notation.r == "natural language"])

hist(df$sus[df$notation.r == "key-value"])
hist(df$sust[df$notation.r == "key-value"])

##### interaction plot ####
interaction.plot(factor(df$period),df$notation,df$sus)

##### mean ####
df %>%
  group_by(notation.r) %>%
  summarize(mean_sus = mean(sus), sd_sus = sd(sus), median_sus = median(sus))

##### mean difference ####
# values
sus_nl <- mean(df$sus[df$notation.r == "natural language"])
sus_kv <- mean(df$sus[df$notation.r == "key-value"])
# difference between nl kv: nl-kv
sus_diffnlkv <- sus_nl-sus_kv
# avergae of nl kv
sus_avgnlkv <- (sus_nl+sus_kv)/2
sus_rationlkv <- sus_diffnlkv/sus_avgnlkv
# percentage
sus_percent_diff_nlkv <- abs(sus_rationlkv*100)

print(c("mean difference in percentage: ", sus_percent_diff_nlkv))

##### shapiro ####
shapiro.test(df$sus[df$notation.r == "natural language"])
shapiro.test(df$sus[df$notation.r == "key-value"])

##### t-test ####
# use parametric test with transformed data
t.test(df$sust[df$notation.r == "natural language"],df$sust[df$notation.r == "key-value"], paired = TRUE, conf.level = 0.95)

##### wilcoxon ####
# double check with non-parametric test on regular data
wilcox.test(df$sus[df$notation.r == "natural language"],df$sus[df$notation.r == "key-value"], paired = TRUE, conf.int=TRUE, conf.level = 0.95)

##### effect size ####
cohens_d(df$sust[df$notation.r == "natural language"],df$sust[df$notation.r == "key-value"], paired = TRUE)

##### linear regression ####
m_sus<-lmer(sust~notation.r+factor(period)+(1|subject),data=df)
anova(m_sus)
summary(m_sus)

qqnorm(df$sust, pch = 1, frame = FALSE)
qqline(df$sust, col = "steelblue", lwd = 2)
plot(df$sust, resid(m_sus))
abline(0, 0) 