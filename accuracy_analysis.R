library(ggplot2)

mydata <- read.csv("accuracy_details.csv", sep = ";", dec = ",", header = TRUE)

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

## STACKED BARCHARTS PLOTS

# Features
f_data <- read.csv("accuracy_features_flat.csv", sep = ";", dec = ",", header = TRUE)

# Draw barplot with grouping & stacking
ggplot(f_data,                  
       aes(x = notation,
           y = value,
           fill = accuracy)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ feature)

# Quality
q_data <- read.csv("accuracy_quality_flat.csv", sep = ";", dec = ",", header = TRUE)

# Draw barplot with grouping & stacking
ggplot(q_data,                  
       aes(x = notation,
           y = value,
           fill = accuracy)) + 
  geom_bar(stat = "identity",
           position = "stack") +
  facet_grid(~ feature)
