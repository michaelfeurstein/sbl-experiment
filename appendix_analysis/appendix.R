library(dplyr)
library(ggplot2)

#### Load ####

df <- read.csv("data_prepared/01_main_data_prepared.csv")
df <- subset(df, select = -c(X))

dfr_new <- read.csv("data_prepared/07_data-experiment-ranking_prepared.csv")
dfr_new <- subset(dfr_new, select = -c(X))

#### Filter ####

df_beginners <- df %>% 
  filter(group.r == "beginner-professional" | group.r == "beginner-student")

df_advanced <- df %>% 
  filter(group.r == "advanced-professional" | group.r == "advanced-student")

dfr_new_beginners <- dfr_new %>%
  filter(subject == "1" | subject == "2" | subject == "6" | subject == "7" | subject == "10" | subject == "12" | subject == "15" | subject == "16" | subject == "17" | subject == "18" | subject == "19" | subject == "23" | subject == "26" | subject == "27" | subject == "31" | subject == "33" | subject == "34" | subject == "38" | subject == "39" | subject == "40" | subject == "41" | subject == "42" | subject == "43" | subject == "46")

dfr_new_advanced <- dfr_new %>%
  filter(subject == "3" | subject == "4" | subject == "5" | subject == "8" | subject == "9" | subject == "11" | subject == "13" | subject == "14" | subject == "20" | subject == "21" | subject == "22" | subject == "24" | subject == "25" | subject == "28" | subject == "29" | subject == "30" | subject == "32" | subject == "35" | subject == "36" | subject == "37" | subject == "44" | subject == "45" | subject == "47" | subject == "48")

#### Efficiency (Duration) ####

par(mar=c(5, 5, 3, 2))
bp_duration <- boxplot(duration.r ~ notation.r, main = "Duration", data = df_beginners, ylab = "Duration (in min.)", xlab = "Notation", names = c("KV","CNL"))
bp_duration <- boxplot(duration.r ~ notation.r, main = "Duration", data = df_advanced, ylab = "Duration (in min.)", xlab = "Notation", names = c("KV","CNL"))

# Outliers 
boxplot.stats(df$duration.log)$out

df_beginners %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration.r), sd_duration = sd(duration.r), min_duration = min(duration.r), max_duration = max(duration.r), med_duration = median(duration.r), q1 = quantile(duration.r, 0.25), q3 = quantile(duration.r, 0.75))

df_advanced %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration.r), sd_duration = sd(duration.r), min_duration = min(duration.r), max_duration = max(duration.r), med_duration = median(duration.r), q1 = quantile(duration.r, 0.25), q3 = quantile(duration.r, 0.75))

# (1) Using native Friedman test in R
friedman.test(df_beginners$duration.log, df_beginners$notation.r, df_beginners$subject)
friedman.test(df_advanced$duration.log, df_advanced$notation.r, df_advanced$subject)
friedman.test(df$duration.log, df$notation.r, df$subject)

# beginners p = 0.2207 --> no significant difference between KV-CNL
# advanced  p = 0.014 --> significant difference between KV-CNL

#### Effectiveness (Accuracy) ####
par(mar=c(5, 5, 3, 2))
bp_duration <- boxplot(accuracy ~ notation.r, main = "Accuracy", data = df_beginners, ylab = "Accuracy in %", xlab = "Notation", names = c("KV","CNL"))
bp_duration <- boxplot(accuracy ~ notation.r, main = "Accuracy", data = df_advanced, ylab = "Accuracy in %", xlab = "Notation", names = c("KV","CNL"))

# Outliers 
boxplot.stats(df$accuracy)$out

df_beginners %>%
  group_by(notation.r) %>%
  summarize(mean_accuracy = mean(accuracy), sd_accuracy = sd(accuracy), min_accuracy = min(accuracy), max_accuracy = max(accuracy), med_accuracy = median(accuracy), q1 = quantile(accuracy, 0.25), q3 = quantile(accuracy, 0.75))

df_advanced %>%
  group_by(notation.r) %>%
  summarize(mean_accuracy = mean(accuracy), sd_accuracy = sd(accuracy), min_accuracy = min(accuracy), max_accuracy = max(accuracy), med_accuracy = median(accuracy), q1 = quantile(accuracy, 0.25), q3 = quantile(accuracy, 0.75))

# (1) Using native Friedman test in R
friedman.test(df_beginners$accuracy, df_beginners$notation.r, df_beginners$subject)
friedman.test(df_advanced$accuracy, df_advanced$notation.r, df_advanced$subject)
friedman.test(df$accuracy, df$notation.r, df$subject)

# beginners p = 0.6374 --> no significant difference between KV-CNL
# advanced  p = 0.1088 --> no significant difference between KV-CNL

#### Usability (SUS) ####
par(mar=c(5, 5, 3, 2))
bp_duration <- boxplot(sus ~ notation.r, main = "SUS", data = df_beginners, ylab = "SUS score", xlab = "Notation", names = c("KV","CNL"))
bp_duration <- boxplot(sus ~ notation.r, main = "SUS", data = df_advanced, ylab = "SUS score", xlab = "Notation", names = c("KV","CNL"))

# Outliers 
boxplot.stats(df$sus)$out

df_beginners %>%
  group_by(notation.r) %>%
  summarize(mean_sus = mean(sus), sd_sus = sd(sus), min_sus = min(sus), max_sus = max(sus), med_sus = median(sus), q1 = quantile(sus, 0.25), q3 = quantile(sus, 0.75))

df_advanced %>%
  group_by(notation.r) %>%
  summarize(mean_sus = mean(sus), sd_sus = sd(sus), min_sus = min(sus), max_sus = max(sus), med_sus = median(sus), q1 = quantile(sus, 0.25), q3 = quantile(sus, 0.75))

# (1) Using native Friedman test in R
friedman.test(df_beginners$sus, df_beginners$notation.r, df_beginners$subject)
friedman.test(df_advanced$sus, df_advanced$notation.r, df_advanced$subject)
friedman.test(df$sus, df$notation.r, df$subject)

# beginners p = 0.1444 --> no significant difference between KV-CNL
# advanced  p = 0.000393 --> significant difference between KV-CNL

#### Ranking (Personal Preference) ####
## BEGINNERS
counts_CNL <- count(dfr_new_beginners, ranking.CNL)
counts_KV <- count(dfr_new_beginners, ranking.KV)

notation <- c(rep(c("CNL"),3),rep(c("KV"),3))
ranking <- rep(c("1st place","2nd place","no preference"), 2)  
counts <- c(counts_CNL$n[1:3],counts_KV$n[1:3])
data_ranking <- data.frame(notation,ranking,counts)

desired_order <- c("KV","CNL")
# Re-order the levels
data_ranking$notation <- factor( as.character(data_ranking$notation), levels=desired_order )

# Stacked + count inside bars
experiment_ranking_stacked_beginners <- ggplot(data_ranking, aes(fill=ranking, y=counts, x=notation)) + 
  geom_bar(position="fill", stat = "identity") +
  geom_text(aes(label=after_stat(counts)), stat="identity", position="fill", vjust = 1.2, size = 3.5) +
  scale_y_continuous(labels = scales::label_percent(scale = 48, prefix = "", suffix = "")) +
  labs(title = "Personal Ranking of Notations (Beginners)", subtitle = "Ranking based on personal preference for notation", x = "Notation", y = "Counts", fill = "Ranking", caption = "1st place: the notation you could imagine working with and formulating storyboards with.\n2nd place: the notation you could NOT imagine working with.\nno preference: If you cannot decide, or are unsure.") +
  theme_bw()

experiment_ranking_stacked_beginners

# (1) Binomial Test
count_preferKV <- dim(subset(dfr_new_beginners, ranking.KV=='1st place'))[1]
count_preferNL <- dim(subset(dfr_new_beginners, ranking.CNL=='1st place'))[1]
count_nopref <- dim(subset(dfr_new_beginners, ranking.KV=='no preference'))[1]

binom_result_beginners <- binom.test(count_preferKV,(count_preferKV+count_preferNL), conf.level = 0.95, p = 0.5, alternative = "two.sided")
binom_result_beginners

## ADVANCED
counts_CNL <- count(dfr_new_advanced, ranking.CNL)
counts_KV <- count(dfr_new_advanced, ranking.KV)

notation <- c(rep(c("CNL"),3),rep(c("KV"),3))
ranking <- rep(c("1st place","2nd place","no preference"), 2)  
counts <- c(counts_CNL$n[1:3],counts_KV$n[1:3])
data_ranking <- data.frame(notation,ranking,counts)

desired_order <- c("KV","CNL")
# Re-order the levels
data_ranking$notation <- factor( as.character(data_ranking$notation), levels=desired_order )

# Stacked + count inside bars
experiment_ranking_stacked_advanced <- ggplot(data_ranking, aes(fill=ranking, y=counts, x=notation)) + 
  geom_bar(position="fill", stat = "identity") +
  geom_text(aes(label=after_stat(counts)), stat="identity", position="fill", vjust = 1.2, size = 3.5) +
  scale_y_continuous(labels = scales::label_percent(scale = 48, prefix = "", suffix = "")) +
  labs(title = "Personal Ranking of Notations (Advanced)", subtitle = "Ranking based on personal preference for notation", x = "Notation", y = "Counts", fill = "Ranking", caption = "1st place: the notation you could imagine working with and formulating storyboards with.\n2nd place: the notation you could NOT imagine working with.\nno preference: If you cannot decide, or are unsure.") +
  theme_bw()

experiment_ranking_stacked_advanced

# (1) Binomial Test
count_preferKV <- dim(subset(dfr_new_advanced, ranking.KV=='1st place'))[1]
count_preferNL <- dim(subset(dfr_new_advanced, ranking.CNL=='1st place'))[1]
count_nopref <- dim(subset(dfr_new_advanced, ranking.KV=='no preference'))[1]

binom_result_advanced <- binom.test(count_preferKV,(count_preferKV+count_preferNL), conf.level = 0.95, p = 0.5, alternative = "two.sided")
binom_result_advanced

# beginners p = 0.5235 --> no significant difference between KV-CNL ranking
# advanced  p = 1.097e-05 --> significant difference between KV-CNL ranking (ONLY 1 participant ranked CNL on 1st place!)
