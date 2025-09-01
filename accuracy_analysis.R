library(ggplot2)

#### Data Preparation ####

mydata <- read.csv("accuracy_details.csv", sep = ";", dec = ",", header = TRUE)
# write to csv so we have readable format for github
write.csv(mydata, "accuracy_details_prepared.csv")
rm(mydata)
# read from prepared csv
mydata <- read.csv("accuracy_details_prepared.csv")

syntaxMapping <- c("nl" = 1, "kv" = 2)
groupMapping <- c("beginner, student" = "bs", "advanced, student" = "as", "beginner, professional" = "bp", "advanced, professional" = "ap")

mydata$notation.r <- syntaxMapping[mydata$notation]
mydata$group.r <- groupMapping[mydata$group.internal]

mydata$notation.r <- as.factor(mydata$notation.r)
mydata$notation.r <- factor(mydata$notation.r, levels = c("1", "2"), labels = c("natural language", "key-value"))
mydata$sequence <- as.factor(mydata$sequence)
mydata$sequence <- factor(mydata$sequence, levels = c("1", "2"), labels = c("NL-KV", "KV-NL"))
mydata$period <- as.factor(mydata$period)
mydata$group.r <- as.factor(mydata$group.r)
mydata$group.r <- factor(mydata$group.r, levels = c("bs", "as", "bp", "ap"), labels = c("beginner-student", "advanced-student", "beginner-professional", "advanced-professional"))

#### - manual calculation to populate flat csv file for import 
#### - I know this is very hacky, but I didn't have the time to create a script for this one time effort
#### - it seemed more efficient to do this manually
value <- table(mydata$q6[mydata$notation.r == "natural language"])
value <- table(mydata$q6[mydata$notation.r == "key-value"])
value
round(100*prop.table(value), digits = 2)
####

#### Data Visualization ####

# Features
f_data <- read.csv("accuracy_features_flat.csv", sep = ";", dec = ",", header = TRUE)
# write to csv so we have tidy format
write.csv(f_data, "accuracy_features_flat_prepared.csv")
rm(f_data)
# read from prepared csv
f_data <- read.csv("accuracy_features_flat_prepared.csv")

# New facet label names for dose variable
feature_f.labels <- c("F1[a]", "F2[a]", "F2[b]", "F2[c]", "F2[d]", "F2[e]", "F3[a]", "F3[b]", "F4[a]", "F4[b]", "F4[c]", "F4[d]", "F4[e]", "F4[f]", "F4[g]", "F5[a]", "F5[b]")
names(feature_f.labels) <- c("f1a", "f2a", "f2b", "f2c", "f2d", "f2e", "f3a", "f3b", "f4a", "f4b", "f4c", "f4d", "f4e", "f4f", "f4g", "f5a", "f5b")

# Draw barplot with grouping & stacking
ggplot(f_data,
       aes(x = notation, y = value, fill = accuracy)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(cols = vars(feature), labeller = labeller(feature = feature_f.labels, .default = label_parsed)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = c("kv","cnl")) +
  scale_fill_manual(values = c("#99CC66","#CC6666")) +
  geom_text(aes(label = paste0(round(value, digits = 0),"%")),
            position = position_stack(),
            hjust = 1.2,
            vjust = 0.5,
            angle = 90,
            size = 2.2) + 
  ggtitle("Detailed accuracy (in %) for features implemented") +
  xlab("Notation") +
  ylab("Accuracy") +
  labs(fill="Feature\nimplemented") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# Quality
q_data <- read.csv("accuracy_quality_flat.csv", sep = ";", dec = ",", header = TRUE)
# write to csv so we have tidy format
write.csv(q_data, "accuracy_quality_flat_prepared.csv")
rm(q_data)
# read from prepared csv
q_data <- read.csv("accuracy_quality_flat_prepared.csv")

# Draw barplot with grouping & stacking

# New facet label names for dose variable
feature_q.labels <- c("Q[1]", "Q[2]", "Q[3]", "Q[4]", "Q[5]", "Q[6]")
names(feature_q.labels) <- c("q1", "q2", "q3", "q4", "q5", "q6")

# plot
ggplot(q_data,
        aes(x = notation, y = value, fill = accuracy)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_grid(cols = vars(feature), labeller = labeller(feature = feature_q.labels, .default = label_parsed)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(aes(label = paste0(round(value, digits = 0),"%")),
            position = position_stack(),
            hjust = 1.2,
            vjust = 0.5,
            angle = 90,
            size = 2.2)
