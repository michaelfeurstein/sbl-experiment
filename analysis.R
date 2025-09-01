library(dplyr)
library(ggplot2)
library(ggpubr)
library(lme4)
library(effectsize)
library(multcomp)
library(sjstats)
#library(lmerTest)
#library(simr)
#library(pwr)

#library(MASS)

### IMPORT DATA  ####

# read from prepared csv
df <- read.csv("data_prepared.csv")

################################# #
################################# #

### SUMMARY OF DATA  ####
#### DURATION ####
##### descriptive ####
#### mean, sd, min, max, med, q1, q3

# using log transformed duration
df %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration.log), sd_duration = sd(duration.log))

# using measured duration in minutes
df %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration.r), sd_duration = sd(duration.r), min_duration = min(duration.r), max_duration = max(duration.r), med_duration = median(duration.r), q1 = quantile(duration.r, 0.25), q3 = quantile(duration.r, 0.75))

##### boxplot ####
par(mar=c(3, 6, 3, 3))
bp_duration <- boxplot(duration.r ~ notation.r, main = "Duration", data = df, ylab = "Duration (in min.)", names = c("1" = "NL", "2" = "KV"), xlab = "")

#### accuracy: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean = mean(accuracy), sd = sd(accuracy), min = min(accuracy), max = max(accuracy), med = median(accuracy), q1 = quantile(accuracy, 0.25), q3 = quantile(accuracy, 0.75))

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

qqnorm(df$duration.log[df$notation.r == "natural language"], pch = 1, frame = FALSE)
qqline(df$duration.log[df$notation.r == "natural language"], col = "steelblue", lwd = 2)
qqplot(df$duration.log[df$notation.r == "natural language"])

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
plot(density(nl_acc))
hist(nl_acc)

# transformation trial and error
# link on transform based on skew
# https://anatomisebiostats.com/biostatistics-blog/transforming-skewed-data/
# link on how to reflect
# https://www.r-bloggers.com/2020/01/a-guide-to-data-transformation/

# reflect data (add 1 and substract from maximum and use absolute values)
# original data is negative skewed (left tail) (also sehr viele werte bei 100 herum) (richtig ?)
nl_acc_t0<-abs((nl_acc+1)-100)
plot(density(nl_acc_t0))
hist(nl_acc_t0)

nl_acc_t1<-(nl_acc+1)
hist(nl_acc_t1)

# 1st try
# starke transformation mit log base 10
nl_acc_t<-log10(nl_acc_t0)
plot(density(nl_acc_t))
hist(nl_acc_t)
shapiro.test(nl_acc_t)

# 2nd try
# boxcox transform
nl_acc_bc<-boxcox(nl_acc_t1~1,lambda=seq(-2,4,0.01))
nl_acc_bc_t<-(nl_acc_t1^2-1)/2
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

# step 2 transformation with actual formula
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
# using log transformed duration
np_t <- df %>%
  group_by(notation.r, period) %>%
  summarize(mean_duration = mean(duration.log))

# using non-transformed duration
np <- df %>%
  group_by(notation.r, period) %>%
  summarize(mean_duration = mean(duration.r))

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
  scale_x_continuous(limits = c(0.5, 2.5),breaks=seq(1,2),minor_breaks=NULL) +
  ylab("Duration mean") +
  xlab("Period") +
  labs(color = "Notation")

np.plot

# arrange both plots in one figure
ggarrange(sp.plot, np.plot, nrow = 1, ncol = 2)

##### interaction plot ####
interaction.plot(factor(df$period),df$notation,df$duration.r)

# estimate carry-over effect using sum values by t-test
# Null Hypothesis: there is a significant difference between NL-KV / KV-NL sequences --> this means there is a carry over effect (use only period 1)
# Alternative Hypothesis: there is no significant difference between NL-KV / KV-NL sequences --> this means there is NO carry over effect (use both periods)
# p-value > 0.05 shows possible carry-over effect is not significantly different between NL-KV / KV-NL sequences
t.test(mean_duration ~ sequence, data = sp)
t.test(mean_duration ~ notation.r, data = np_t)

# reassess: this should be teh actual test for crossover effect
t.test(duration.r ~ sequence , data = df, paired = TRUE, conf.level = 0.95, alternative = "two.sided")

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
  scale_x_continuous(limits = c(0.5, 2.5),breaks=seq(1,2),minor_breaks=NULL) +
  ylab("SUS score") +
  xlab("Period") +
  labs(color = "Notation")

np.plot

# arrange both plots in one figure
ggarrange(sp.plot, np.plot, nrow = 1, ncol = 2)

##### interaction plot ####
interaction.plot(factor(df$period),df$notation,df$sus)

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
  scale_x_continuous(limits = c(0.5, 2.5),breaks=seq(1,2),minor_breaks=NULL) +
  ylab("Accuracy") +
  xlab("Period") +
  labs(color = "Notation")

np.plot

# arrange both plots in one figure
ggarrange(sp.plot, np.plot, nrow = 1, ncol = 2)

##### interaction plot ####
interaction.plot(factor(df$period),df$notation,df$accuracy)

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
# unstandardized mean difference between cnl kv
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
## two-sided, paired t-test
t.test(x = df$duration.log[df$notation.r == "natural language"], y = df$duration.log[df$notation.r == "key-value"], paired = TRUE, conf.level = 0.95, alternative = "two.sided")

##### wilcoxon ####
wilcox.test(df$duration.r[df$notation.r == "natural language"],df$duration.r[df$notation.r == "key-value"], paired = TRUE, conf.int=TRUE, conf.level = 0.95)

##### effect size ####
cohens_d(df$duration.log[df$notation.r == "natural language"],df$duration.log[df$notation.r == "key-value"], paired = TRUE, pooled_sd = TRUE)

# compare with hedges_g
hedges_g()

##### linear mixed model ####

# mean difference and confidence interval (CI) bounds

###### lmer ####
# based on input from Thomas Rusch
#m4<-lmer(duration.log~notation.r+factor(period)+(1|subject),data=df)
# and adapted based on Lukas Meier slideset
m4<-lmer(duration.log~notation.r+period+sequence+(1|subject),data=df)
anova(m4)
anova_stats(m4)
summary(m4)

qqnorm(df$duration.log, pch = 1, frame = FALSE)
qqline(df$duration.log, col = "steelblue", lwd = 2)

plot(df$duration.log, resid(m4))
abline(0, 0) 

summary(glht(m4, linfct=mcp(notation.r="Tukey")))

confint(m4)
####
# Source Calc: https://rpubs.com/aaronsc32/post-hoc-analysis-tukey
# Source Plot: https://rpubs.com/Edbbio/885399 

N <- length(df$duration.log) # sample size = 96
k <- length(unique(df$notation.r)) # number of treatments = 2
n <- N / k # number of samples per group

# Mean Square
confint(m4)

means <- tapply(df$duration.log, df$notation.r, mean)
trt1.ctrl.diff <- means[2] - means[1]
trt1.ctrl.diff

trt1.ctrl.diff.lower <- 0.1764396
trt1.ctrl.diff.upper <- 0.3615455

####

###### Tukey HSD ####
# based on input from Stefan Sobernig
model <- aov(duration.log~notation.r, data=df)
summary(model)
result <- TukeyHSD(model, "notation.r", conf.level=.95, ordered = TRUE)
result$notation.r
resultDF <- data.frame(result$notation.r)

# //
# not needed / keep for reference
# extract meandiff and CI bounds from TukeyHSD resultDF
#duration.meandiff <- resultDF[1]$diff
#duration.lwr <- resultDF[2]$lwr
#duration.upr <- resultDF[3]$upr
# //

# Plotting Tukey HSD
# Source: Stefan aov.html + http://sape.inf.usi.ch/quick-reference/ggplot2/geom_pointrange

# (1) using log transformed data
d=data.frame(contrast="nl-kv", lower=trt1.ctrl.diff.lower, mean=trt1.ctrl.diff, upper=trt1.ctrl.diff.upper)

ggplot() + 
  geom_pointrange(data=d, mapping=aes(x=contrast, y=mean, ymin=lower, ymax=upper), size=1, color="black", fill="white", shape=22) + 
  geom_hline(yintercept = 0, linetype="dotted") +
  scale_y_continuous(limits=c(-0.5,1)) +
  coord_flip() +
  ylab('Mean Differences (log(TIME))') +
  xlab('Model') + theme_bw() + theme(legend.position="none",
                                     axis.title.x=element_blank(),
                                     axis.text.x=element_text(size=12),
                                     axis.title.y=element_blank(),
                                     axis.text.y=element_blank())

# (2) using back-transformed data for reporting
duration.meandiff.b <- exp(trt1.ctrl.diff)
duration.lwr.b <- exp(trt1.ctrl.diff.lower)
duration.upr.b <- exp(trt1.ctrl.diff.upper)

d=data.frame(contrast="nl-kv", lower=duration.lwr.b, mean=duration.meandiff.b, upper=duration.upr.b)

ggplot(data=d) +
  geom_bar(aes(x=contrast, y=mean-1), stat="identity", fill="lightblue", position = position_nudge(y = 1)) +
  geom_pointrange(mapping=aes(x=contrast, y=mean, ymin=lower, ymax=upper), size=1, color="black", fill="white", shape=22) +
  geom_hline(yintercept = 1, linetype="dotted") +
  scale_y_continuous(limits=c(-0.5,2)) +
  coord_flip() +
  ylab('Mean Differences ((raw unit duration in minutes))') +
  xlab('Model') + theme_bw() + theme(legend.position="none",
                                     axis.title.x=element_blank(),
                                     axis.text.x=element_text(size=12),
                                     axis.title.y=element_blank(),
                                     axis.text.y=element_blank())

d

# /
# keep for reference
# 2-exp(c(0.1012, 0.26899, 0.4367))
# exp(c(0.1012, 0.26899, 0.4367))
# /

# Result duration.log (log transformed)
# lwr = 0.1012 mean difference = 0.26899 upr = 0.4367

# Result duration (back transformed)
# lwr = 1.106576 mean difference = 1.308645  upr = 1.547615

# d.h. KV is im schlechtesten fall 10% schneller & im schnitt 30% und im Besten Fall 54 % fall

#### ACCURACY  ####
# correctness of result from coding task (creation of video-based learning module)

##### boxplot ####
boxplot(accuracy ~ notation.r, data = df, xlab = "Notation", ylab = "Accuracy in percent")

##### mean ####
df %>%
  group_by(notation.r) %>%
  summarize(mean = mean(accuracy), sd = sd(accuracy), min = min(accuracy), max = max(accuracy), med = median(accuracy), q1 = quantile(accuracy, 0.25), q3 = quantile(accuracy, 0.75))

## percentages of 0% accuracy and 100% accuracy
min_table <- table(df$accuracy[df$notation.r == "natural language"])
min_table_prop <- prop.table(min_table)

# number count
min_table
# fractions
min_table_prop

min_table <- table(df$accuracy[df$notation.r == "key-value"])
min_table_prop <- prop.table(min_table)

# number count
min_table
# fractions
min_table_prop

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
t.test(df$accuracy[df$notation.r == "natural language"],df$accuracy[df$notation.r == "key-value"], paired = TRUE, conf.level = 0.95, alternative = "two.sided")

##### wilcoxon ####
wilcox.test(df$accuracy[df$notation.r == "natural language"],df$accuracy[df$notation.r == "key-value"], paired = TRUE, conf.int=TRUE, conf.level = 0.95)

##### effect size ####
cohens_d(df$accuracy[df$notation.r == "natural language"],df$accuracy[df$notation.r == "key-value"], paired = TRUE, pooled_sd = TRUE)

#### SUS  ####
# correctness of result from coding task (creation of video-based learning module)

##### boxplot ####
boxplot(sus ~ notation.r, data = df, xlab = "Notation", ylab = "SUS score", names = c("1" = "NL", "2" = "KV"))
boxplot(sust ~ notation.r, data = df, xlab = "Notation", ylab = "SUS transformed", names = c("1" = "NL", "2" = "KV"))

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
  summarize(mean = mean(sus), sd = sd(sus), min = min(sus), max = max(sus), med = median(sus), q1 = quantile(sus, 0.25), q3 = quantile(sus, 0.75))

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
t.test(df$sust[df$notation.r == "natural language"],df$sust[df$notation.r == "key-value"], paired = TRUE, conf.level = 0.95, alternative = "two.sided")

##### wilcoxon ####
# double check with non-parametric test on regular data
wilcox.test(df$sus[df$notation.r == "natural language"],df$sus[df$notation.r == "key-value"], paired = TRUE, conf.int=TRUE, conf.level = 0.95)

##### linear regression ####
m_sus<-lmer(sust~notation.r+factor(period)+(1|subject),data=df)
anova(m_sus)
summary(m_sus)

qqnorm(df$sust, pch = 1, frame = FALSE)
qqline(df$sust, col = "steelblue", lwd = 2)
plot(df$sust, resid(m_sus))
abline(0, 0) 

###### Tukey HSD ####

# Sources
# Confidence Interval plots: https://statisticsbyjim.com/hypothesis-testing/confidence-intervals-compare-means/

model <- aov(sus~notation.r, data=df)
summary(model)
TukeyHSD(model, conf.level=.95)
plot(TukeyHSD(model, conf.level=.95), las = 2)

##### effect size ####

###### cohen's d ####
cohens_d(df$sust[df$notation.r == "natural language"],df$sust[df$notation.r == "key-value"], paired = TRUE, pooled_sd = TRUE)

#### RANK  ####
# sorting a df manually: https://stackoverflow.com/a/9392408/693052

##### barplot ####
dfr <- read.csv("ranking_preference.csv", sep = ";", dec = ",", header = TRUE)
desired_order <- c("preferNL","nopref","preferKV")
# Re-order the levels
dfr$preference <- factor( as.character(dfr$preference), levels=desired_order )
# Re-order the data.frame
dfr <- dfr[order(dfr$preference),]
counts <- table(dfr$preference)
b<-barplot(counts, main="Ranking (Personal Preference)",
        xlab="Preference for a notation",names.arg=c("CNL", "No Preference", "KV"),ylim=range(pretty(c(0, counts))))
text(b, counts - 1.5, paste0(sprintf("%4.1f ", counts / sum(counts) * 100), "%", sprintf(" (%d)",counts)), font=1, col=c("black"), cex = 0.9)

# Source: https://statisticsbyjim.com/basics/ordinal-data/
# The ranking data is of type ordinal

# Generally this is calle "Paired Preference Test"
# There is 2-AFC (2 alternatives force choice) and 2-AC (2 alternatives choice)
# Discussion about forcing preference vs no preference 

# Source: https://measuringu.com/preference-data/
# Tests available: binomial / Chi-Square Goodness of Fit test & McNemar
# Problem is with the ties e.g. 1st place 1st place

# Approach:
# plot histogram of 1st place 2nd place of NL and KV
# plot how many ties 
# argue to use ties as no preference
# which test: 

boxplot(rank.r ~ notation.r, data = df)

hist(df$rank.r[df$notation.r == "natural language"])

mood.test(rank.r ~ notation.r, data = df, exact = FALSE)

wilcox.test(rank.r ~ notation.r, data = df, exact = FALSE)

binom.test(33,42)

p_chisq <- pchisq(q = 0.99, df = 1, lower.tail=FALSE)
sprintf("%.12f", p_chisq)

# STATEMENT on outliers
# We are keeping outliers.
# Link: https://statisticsbyjim.com/basics/remove-outliers/
