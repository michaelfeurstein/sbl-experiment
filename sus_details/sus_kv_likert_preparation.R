library(tidyr)
library(dplyr)
library(ggplot2)
library(lemon)
library(stringr)

data <- read.csv("sus_details/sus_kv_likert_responses.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")

# LimeSurvey Field type: A
data[, 1] <- as.numeric(data[, 1])
attributes(data)$variable.labels[1] <- "Seed"
names(data)[1] <- "seed"
# LimeSurvey Field type: A
data[, 2] <- as.character(data[, 2])
attributes(data)$variable.labels[2] <- "[I think that I would like to use this notation frequently.] Please answer the following questions about the notation you just used:"
data[, 2] <- factor(data[, 2], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[2] <- "PQ1. I think that I would like to use this notation frequently."
# LimeSurvey Field type: A
data[, 3] <- as.character(data[, 3])
attributes(data)$variable.labels[3] <- "[I find the notation unnecessarily complex.] Please answer the following questions about the notation you just used:"
data[, 3] <- factor(data[, 3], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[3] <- "NQ1. I find the notation unnecessarily complex."
# LimeSurvey Field type: A
data[, 4] <- as.character(data[, 4])
attributes(data)$variable.labels[4] <- "[I think the notation would be easy to use.] Please answer the following questions about the notation you just used:"
data[, 4] <- factor(data[, 4], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[4] <- "PQ2. I think the notation would be easy to use."
# LimeSurvey Field type: A
data[, 5] <- as.character(data[, 5])
attributes(data)$variable.labels[5] <- "[I think that I would need the support of a technical person*** to be able to use this notation.] Please answer the following questions about the notation you just used:"
data[, 5] <- factor(data[, 5], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[5] <- "NQ2. I think that I would need the support of a technical person to be able to use this notation."
# LimeSurvey Field type: A
data[, 6] <- as.character(data[, 6])
attributes(data)$variable.labels[6] <- "[I find the various functions* in this notation are well integrated.] Please answer the following questions about the notation you just used:"
data[, 6] <- factor(data[, 6], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[6] <- "PQ3. I find the various functions in this notation are well integrated."
# LimeSurvey Field type: A
data[, 7] <- as.character(data[, 7])
attributes(data)$variable.labels[7] <- "[I think there is too much inconsistency** in this notation.] Please answer the following questions about the notation you just used:"
data[, 7] <- factor(data[, 7], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[7] <- "NQ3. I think there is too much inconsistency in this notation."
# LimeSurvey Field type: A
data[, 8] <- as.character(data[, 8])
attributes(data)$variable.labels[8] <- "[I would imagine that most people would learn to use this notation very quickly.] Please answer the following questions about the notation you just used:"
data[, 8] <- factor(data[, 8], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[8] <- "PQ4. I would imagine that most people would learn to use this notation very quickly."
# LimeSurvey Field type: A
data[, 9] <- as.character(data[, 9])
attributes(data)$variable.labels[9] <- "[I would find the notation very cumbersome to use.] Please answer the following questions about the notation you just used:"
data[, 9] <- factor(data[, 9], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[9] <- "NQ4. I would find the notation very cumbersome to use."
# LimeSurvey Field type: A
data[, 10] <- as.character(data[, 10])
attributes(data)$variable.labels[10] <- "[I would feel very confident using the notation.] Please answer the following questions about the notation you just used:"
data[, 10] <- factor(data[, 10], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[10] <- "PQ5. I would feel very confident using the notation."
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- "[I would need to learn a lot of things before I could get going with this notation.] Please answer the following questions about the notation you just used:"
data[, 11] <- factor(data[, 11], levels=c("A1","A2","A3","A4","A5"),labels=c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree'))
names(data)[11] <- "NQ5. I would need to learn a lot of things before I could get going with this notation."

# Remove first column for plotting (we don't need the seed anymore)
data <- data[,2:11]

# Source: https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html
data_tidy <- data %>% 
  pivot_longer(
    1:10, 
    names_to = "question", 
    values_to = "answer"
  ) 


data_tidy <- data_tidy %>% arrange(match(question, 
                                         c("PQ1. I think that I would like to use this notation frequently.",
                                           "NQ1. I find the notation unnecessarily complex.",
                                           "PQ2. I think the notation would be easy to use.",
                                           "NQ2. I think that I would need the support of a technical person to be able to use this notation.",
                                           "PQ3. I find the various functions in this notation are well integrated.",
                                           "NQ3. I think there is too much inconsistency in this notation.",
                                           "PQ4. I would imagine that most people would learn to use this notation very quickly.",
                                           "NQ4. I would find the notation very cumbersome to use.",
                                           "PQ5. I would feel very confident using the notation.",
                                           "NQ5. I would need to learn a lot of things before I could get going with this notation.")))

data_tidy <- data_tidy %>% mutate(question = factor(question, 
                                                    levels = c("PQ5. I would feel very confident using the notation.",
                                                               "PQ4. I would imagine that most people would learn to use this notation very quickly.",
                                                               "PQ3. I find the various functions in this notation are well integrated.",
                                                               "PQ2. I think the notation would be easy to use.",
                                                               "PQ1. I think that I would like to use this notation frequently.",
                                                               "NQ5. I would need to learn a lot of things before I could get going with this notation.",
                                                               "NQ4. I would find the notation very cumbersome to use.",
                                                               "NQ3. I think there is too much inconsistency in this notation.",
                                                               "NQ2. I think that I would need the support of a technical person to be able to use this notation.",
                                                               "NQ1. I find the notation unnecessarily complex.")))

# Add new column named 'notion' based on positive or negative framing of question
# Source: https://stackoverflow.com/q/50001383/693052
data_tidy <- data_tidy %>%
  mutate(
    notion = case_when(
      question == "PQ1. I think that I would like to use this notation frequently." ~ "positive",
      question == "NQ1. I find the notation unnecessarily complex." ~ "negative",
      question == "PQ2. I think the notation would be easy to use." ~ "positive",
      question == "NQ2. I think that I would need the support of a technical person to be able to use this notation." ~ "negative",
      question == "PQ3. I find the various functions in this notation are well integrated." ~ "positive",
      question == "NQ3. I think there is too much inconsistency in this notation." ~ "negative",
      question == "PQ4. I would imagine that most people would learn to use this notation very quickly." ~ "positive",
      question == "NQ4. I would find the notation very cumbersome to use." ~ "negative",
      question == "PQ5. I would feel very confident using the notation." ~ "positive",
      question == "NQ5. I would need to learn a lot of things before I could get going with this notation." ~ "negative"
    )
  )

# write to csv so we don't need to run above lines too often
write.csv(data_tidy, "sus_details/sus_kv_likert_tidy.csv")

#######
#######

# read from prepared csv
data_tidy <- read.csv("sus_details/sus_kv_likert_tidy.csv")

data_tidy <- subset(data_tidy, select = -c(X))

data_tidy <- data_tidy %>% mutate(question = factor(question, 
                                                    levels = c("PQ5. I would feel very confident using the notation.",
                                                               "PQ4. I would imagine that most people would learn to use this notation very quickly.",
                                                               "PQ3. I find the various functions in this notation are well integrated.",
                                                               "PQ2. I think the notation would be easy to use.",
                                                               "PQ1. I think that I would like to use this notation frequently.",
                                                               "NQ5. I would need to learn a lot of things before I could get going with this notation.",
                                                               "NQ4. I would find the notation very cumbersome to use.",
                                                               "NQ3. I think there is too much inconsistency in this notation.",
                                                               "NQ2. I think that I would need the support of a technical person to be able to use this notation.",
                                                               "NQ1. I find the notation unnecessarily complex.")))

data_tidy <- data_tidy %>% mutate(answer = factor(answer, 
                                                  levels = c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree')))

# Source juliasilge (comment on github from Oct 4, 2022)
# Link: https://github.com/juliasilge/tidytext/issues/222
custom_labeler <- function(x) {
  x %>%
    str_replace("___.+$", "") %>%
    str_wrap(width = 50)
}

ggplot(data_tidy, aes(x=question, fill=answer)) + 
  geom_bar(width = 0.7, position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values=c("darkred","red", "grey", "darkolivegreen1", "darkgreen")) +
  scale_y_continuous(expand = expansion(0)) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0), axis.title.y = element_blank(), axis.title.x =element_blank(), legend.position = "bottom", legend.justification = c(1,1), legend.title = element_blank()) +
  facet_rep_grid(notion ~ ., scales = "free", repeat.tick.labels = "all") +
  scale_x_discrete(labels = custom_labeler) +
  coord_capped_cart(bottom="both", left="both") +
  coord_flip()
