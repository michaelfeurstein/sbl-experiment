library(Superpower)

### IMPORT DATA  ####

mydata <- read.csv("samplesize/data_pretest.csv", sep = ";", dec = ",", header = TRUE)
mydata$time <- as.POSIXct(mydata$time, format="%M:%S")
mydata$duration <- as.numeric(format(mydata$time, "%M")) + as.numeric(format(mydata$time, "%S"))/60

# setup mapping and as.factor
syntaxMapping <- c("nl" = 1, "kv" = 2)
rankingMapping <- c("don't know" = 0, "2nd place" = 1, "1st place" = 2)

mydata$notation.r <- syntaxMapping[mydata$notation]
mydata$rank.r <- rankingMapping[mydata$rank]

mydata$notation.r <- as.factor(mydata$notation.r)
mydata$notation.r <- factor(mydata$notation.r, levels = c("1", "2"), labels = c("natural language", "key-value"))
mydata$sequence <- as.factor(mydata$sequence)
mydata$sequence <- factor(mydata$sequence, levels = c("1", "2"), labels = c("NL-KV", "KV-NL"))
mydata$rank.r <- factor(mydata$rank.r, levels = c("0", "1", "2"), labels = c("don't know", "2nd place", "1st place"))
mydata$rank.r <- as.integer(mydata$rank.r)
mydata$period <- as.factor(mydata$period)

# log transform duration
mydata$duration.log = log(mydata$duration)

# the actual dataframe we'll be working with
df <- subset(mydata, select = c("subject", "sequence", "period", "notation.r", "duration", "duration.log", "accuracy", "sus", "rank.r"))

# write to csv so we don't need to run above lines too often
write.csv(df, "data_pretest_prepared.csv")

############################## #
############################## #

### SUMMARY OF DATA  ####
#### duration: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration.log), sd_duration = sd(duration.log))

df %>%
  group_by(notation.r) %>%
  summarize(mean_duration = mean(duration), sd_duration = sd(duration))

#### accuracy: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_accuracy = mean(accuracy), sd_duration = sd(accuracy))

#### sus scores: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_sus_score = mean(sus), sd_sus_score = sd(sus))

#### ranks: mean, sd ####

df %>%
  group_by(notation.r) %>%
  summarize(mean_rank = mean(rank.r), sd_rank = sd(rank.r))

#
### POWER ANALYSIS ####
#
# Approach: run a simulation of study at different sample sizes where each study is repeated many thousands of times and optimize for power
# We would aim for a power of 80 % but also calculate 90 %

#
# Superpower
# based on vignette: https://cran.r-project.org/web/packages/Superpower/vignettes/intro_to_superpower.html
# and book: https://aaroncaldwell.us/SuperpowerBook/introduction-to-power-analysis.html#overview-of-power-analysis 
# and: https://rdrr.io/cran/Superpower/f/vignettes/more_anova_designs.Rmd 
#

#### DURATION ####
##### pretest data ####
# duration vs. duration.log is of minimal difference in result
# using duration in minutes for simulation for comparability to Johanson & Hasselbring (2017)
design.duration.pretest <- ANOVA_design(design = "2w",
                                        n = 6, 
                                        mu <- c(14, 10), 
                                        sd <- c(5.99, 3.11), 
                                        r = 0, 
                                        label_list = list("Notation" = c( "natural-language", "key-value")),
                                        plot = TRUE)

plot_power(design.duration.pretest, max_n = 100, desired_power = 80)
plot_power(design.duration.pretest, max_n = 100, desired_power = 90)

##### Johanson & Hasselbring (2017) data ####
# data reading taken from p. 2222, Table 5, Time Spent, Parameters (GPL, DSL)
design.duration.johanson <- ANOVA_design(design = "2w",
                                         n = 40, 
                                         mu <- c(6.4, 4.5), 
                                         sd <- c(4.0, 2.1), 
                                         r = 0, 
                                         label_list = list("Notation" = c( "gpl", "dsl")),
                                         plot = TRUE)

plot_power(design.duration.johanson, max_n = 100, desired_power = 80)
plot_power(design.duration.johanson, max_n = 100, desired_power = 90)

##### Hoisl et al. (2014) data ####
# data reading taken from p. 6, Table I, Task 1.1 (N-notation, E-notation)
design.duration.hoisl <- ANOVA_design(design = "2w",
                                      n = 20, 
                                      mu <- c(8.67, 15.43), 
                                      sd <- c(2.25, 6.73), 
                                      r = 0, 
                                      label_list = list("Notation" = c( "n", "e")),
                                      plot = TRUE)

plot_power(design.duration.hoisl, max_n = 100, desired_power = 80)
plot_power(design.duration.hoisl, max_n = 100, desired_power = 90)

#### ACCURACY ####
##### pretest data ####
design.accuracy.pretest <- ANOVA_design(design = "2w",
                                        n = 6, 
                                        mu <- c(91.7, 80.8),
                                        sd <- c(8.16, 15.3),
                                        r = 0, 
                                        label_list = list("Notation" = c( "natural-language", "key-value")),
                                        plot = TRUE)

plot_power(design.accuracy.pretest, max_n = 100, desired_power = 80)
plot_power(design.accuracy.pretest, max_n = 100, desired_power = 90)

##### Johanson & Hasselbring (2017) data ####
# data reading taken from p. 2222, Table 4, Correctness, Parameters (GPL, DSL)
design.accuracy.johanson <- ANOVA_design(design = "2w",
                                         n = 40, 
                                         mu <- c(60.8, 97.8), 
                                         sd <- c(47.3, 6.9), 
                                         r = 0, 
                                         label_list = list("Notation" = c( "gpl", "dsl")),
                                         plot = TRUE)

plot_power(design.accuracy.johanson, max_n = 100, desired_power = 80)
plot_power(design.accuracy.johanson, max_n = 100, desired_power = 90)

##### Juhnke (2017) data ####
# data reading taken from p. 236, Table 6.14, H1, System MW (NL, DSL)
design.accuracy.juhnke <- ANOVA_design(design = "2w",
                                       n = 10, 
                                       mu <- c(4.9, 1.8), 
                                       sd <- c(2.685, 1.751), 
                                       r = 0, 
                                       label_list = list("Notation" = c( "nl", "dsl")),
                                       plot = TRUE)

plot_power(design.accuracy.juhnke, max_n = 100, desired_power = 80)
plot_power(design.accuracy.juhnke, max_n = 100, desired_power = 90)

#### SUS scores ####
##### survey data ####
# pretest data not sufficient, therefore using survey data
design.sus.survey <- ANOVA_design(design = "2w",
                                  n = 55, 
                                  mu <- c(56.32, 58),
                                  sd <- c(21.14, 19.63),
                                  r = 0, 
                                  label_list = list("Notation" = c( "natural-language", "key-value")),
                                  plot = TRUE)

plot_power(design.sus.survey, max_n = 100, desired_power = 80)
plot_power(design.sus.survey, max_n = 120, desired_power = 90)

##### Lewis & Sauro (2022) ####
# Link: https://measuringu.com/sample-sizes-for-sus-comparisons/
# reading n from table taken from website, Table 1, Sample size requirements for various critical differences, d=7.5 and Within; 95% confidence
# n = 46 with 80% power, sd= 17.7
# n = 55 based on mean difference between 90% and 80% power calculations leading to an estimate of 8.89

##### Funk et al. (2007) data ####
# in this study no values for SD are given, therefore the typical SD form Lewis and Sauro (2022) is used (17.7)
# Link: https://measuringu.com/sample-sizes-for-sus-comparisons/
# data reading taken from p. 150, Table 2, Measure mean, (CLIE SUS rating, Protege SUS rating)
design.sus.funk <- ANOVA_design(design = "2w",
                                n = 15, 
                                mu <- c(78, 47), 
                                sd <- c(17.7, 17.7), 
                                r = 0, 
                                label_list = list("Notation" = c( "CLIE", "Protege")),
                                plot = TRUE)

plot_power(design.sus.funk, max_n = 100, desired_power = 80)
plot_power(design.sus.funk, max_n = 100, desired_power = 90)

#### RANK ####
##### pretest data ####
design.rank.pretest <- ANOVA_design(design = "2w",
                                    n = 6,
                                    mu <- c(2.33, 1.83),
                                    sd <- c(0.816, 0.753),
                                    r = 0, 
                                    label_list = list("Notation" = c( "natural-language", "key-value")),
                                    plot = TRUE)

plot_power(design.rank.pretest, max_n = 100, desired_power = 80)
plot_power(design.rank.pretest, max_n = 100, desired_power = 90)

### REFERENCES ####

# Adam Funk, Valentin Tablan, Kalina Bontcheva, Hamish Cunningham, Brian Davis, and Siegfried Handschuh. Clone: Controlled language for ontology edit- ing. In Karl Aberer et al., editors, The Semantic Web, volume 4825 of Lecture Notes in Computer Science, pages 142–155, Berlin, Heidelberg, 2007. Springer Berlin Heidelberg.
# Jim Lewis and Jeff Sauro. Sample Sizes for Comparing SUS Scores – MeasuringU. https://measuringu.com/sample-sizes-for-sus-comparisons/, April 2022. Accessed 2022-10-11.
# Katharina Juhnke. Improving the Quality of Automotive Test Case Specifications. Dissertation, Ulm University, Ulm, August 2020.
# Arne N. Johanson and Wilhelm Hasselbring. Effectiveness and efficiency of a domain-specific language for high-performance marine ecosystem simulation: a con- trolled experiment. Empirical Software Engineering, 22(4):2206–2236, August 2017.
# Bernhard Hoisl, Stefan Sobernig, and Mark Strembeck. Comparing Three Notations for Defining Scenario-based Model Tests: A Controlled Experiment. In Proceedings of the 9th International Conference on the Quality of Information and Communi- cations Technology, pages 95–104, Guimaraes, Portugal, 2014. IEEE.