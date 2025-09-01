setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyr)
library(dplyr)

###
### data import: profile questionnaire ####
###

p1 <- read.csv("../data_preparation/export_data_profile-questionnaire/participant_profiles_anon.csv", sep = ";", header = TRUE)

###
### data import: experiment questionnaire ####
###

# load
d1 <- read.csv("../data_preparation/export_data_experiment-questionnaire/results-survey442511_selected.csv", sep = ";", header = TRUE)
d2 <- read.csv("../data_preparation/export_data_experiment-questionnaire/results-survey519841_selected.csv", sep = ";", header = TRUE)
d3 <- read.csv("../data_preparation/export_data_experiment-questionnaire/results-survey678389_selected.csv", sep = ";", header = TRUE)
d4 <- read.csv("../data_preparation/export_data_experiment-questionnaire/results-survey864879_selected.csv", sep = ";", header = TRUE)

# merge
df_kv_nl <- rbind(d1,d3)
df_nl_kv <- rbind(d2,d4)

#
### dataset 6: profile ####
#

# extract relevant columns
profiles <- subset(p1, select=c(subject,QUALIFICATION,QUALIFICATION.other.,OCCUPATION,OCCUPATION.other.,PROGRAMMINGyesno,PROGRAMMINGlangs,PROGRAMMINGskill.SQ001.,PROGRAMMINGinterest.SQ001.,Author,Researcher,User,Tutor,Student,authorHOWMANYYEARS,authorHOWMANYVIDEOS,userHOWMANYYEARS,userHOWMANYCOURSES, studentHOWMANYYEARS,studentHOWMANYCOURSE,researchHOWMANYYEARS,researchHOWMANYPUB,tutorHOWMANYYEARS,tutorHOWMANYCOURSES))

##
#### programming experience ####
##

# setup a mapping based on column data:
# mapping text to a number
# the space is missing on purpose
experienceMapping <- c("none"= 0, "beginnerproblems writing easy programs, can read" = 1, "regularcan write basic programs, no problems reading" = 2, "advancedcan write complex programs" = 3, "expertyears of experience" = 4)

# apply mapping
profiles$experience.r <- experienceMapping[profiles$PROGRAMMINGskill.SQ001.]

# use coalesce to replace NA's with 0
profiles$experience.r <- coalesce(profiles$experience.r, 0)

# create a factor for experience.r with labels
profiles$experience.r <- factor(profiles$experience.r, levels = c("0", "1", "2", "3", "4"), labels = c("No experience", "Beginner", "Regular", "Experienced", "Expert"))

##
#### programming interest ####
##

# setup a mapping based on column data:
# mapping text to a number
interestMapping <- c("very low"= 0, "low" = 1, "relatively low" = 2, "relatively high" = 3, "high" = 4, "very high" = 5)

# apply mapping
profiles$interest.r <- interestMapping[profiles$PROGRAMMINGinterest.SQ001.]

# create a factor for interest.r with labels
profiles$interest.r <- factor(profiles$interest.r, levels = c("0", "1", "2", "3", "4", "5"), labels = c("Very low", "Low", "Relatively low", "relatively high", "High", "Very High"))

##
#### remove N/As ####
##

#profiles <- data.frame(lapply(profiles, function(x) {gsub("N/A", "No", x)}))
#profiles %>% replace_na(list(Author = "No", Researcher = "No"))
#profiles %>% replace(is.na(.), 0)

##
#### qualification ####
##

profiles$QUALIFICATION[profiles$QUALIFICATION.other. == 'Mag.'] <- "Master"
profiles$Qual_fac <- factor(profiles$QUALIFICATION, levels = c("Bachelor", "Master",  "PhD", "Other"))

##
#### occupation ####
##

profiles$OCCUPATION[profiles$OCCUPATION.other. == 'Assistant Professor'] <- "Professor"

profiles$Occu_fac <- factor(profiles$OCCUPATION, levels = c("Bachelor student", "Master student",  "Ph.D. student", "Post-doc", "Researcher", "(Senior) Lecturer", "Professor", "Other"))

#### export ####
# the final dataframe for notebook
df_export <-subset(profiles, select = c("Qual_fac","Occu_fac","Author", "Researcher", "User", "Tutor", "Student","PROGRAMMINGlangs","experience.r","interest.r"))

# write to csv so we don't need to run above lines too often
write.csv(df_export, "../data_prepared/06_participant_profiles_prepared.csv")

### dataset 7: rankings ####

# RQ1 = CNL
# RQ2 = KV

# extract rankings
df_rankings_1 <- subset(df_kv_nl, select=c(id,seed,RANKING.SQ001.,RANKING.SQ002.))
names(df_rankings_1)[which(names(df_rankings_1)=="RANKING.SQ001.")] <- "ranking.CNL"
names(df_rankings_1)[which(names(df_rankings_1)=="RANKING.SQ002.")] <- "ranking.KV"

df_rankings_2 <- subset(df_nl_kv, select=c(id,seed,RANKING.SQ001.,RANKING.SQ002.))
names(df_rankings_2)[which(names(df_rankings_2)=="RANKING.SQ001.")] <- "ranking.CNL"
names(df_rankings_2)[which(names(df_rankings_2)=="RANKING.SQ002.")] <- "ranking.KV"

# merge rankings
df_rankings <- rbind(df_rankings_1, df_rankings_1)

# list of seeds to delete (errorneous data or overbooking)
df_rankings <- df_rankings[!(df_rankings$seed %in% "434964103"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "349662352"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "688332622"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "2070192834"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "1216904780"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "1916684881"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "1615544513"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "694876113"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "145471250"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "1714552698"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "1189809838"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "2052958529"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "1474088125"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "374526227"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "165728266"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "1397240311"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "881869422"),]
df_rankings <- df_rankings[!(df_rankings$seed %in% "1725062158"),]
df_rankings <- df_rankings[complete.cases(df_rankings), ]



df_rankings <- df_rankings %>%
  mutate(ranking.CNL = 
           case_when(
             ranking.CNL == ranking.KV ~ "no preference",
             ranking.CNL == "1st place" ~ "1st place",
             ranking.CNL == "2nd place" ~ "2nd place"
            )
  )

df_rankings <- df_rankings %>%
  mutate(ranking.KV = 
           case_when(
             ranking.CNL == "no preference" ~ "no preference",
             ranking.KV == "1st place" ~ "1st place",
             ranking.KV == "2nd place" ~ "2nd place"
           )
  )

# Preparing data-experiment-ranking_prepared.csv
# header: subject, id, seed, ranking.CNL, ranking.KV

# create running index (subject)
df_rankings$subject <- seq.int(nrow(df_rankings)) 

# move subject column to first position
df_rankings <- df_rankings[,c(ncol(df_rankings),1:(ncol(df_rankings)-1))]

#### export ####
# write to csv
write.csv(df_rankings, "../data_prepared/07_data-experiment-ranking_prepared.csv")

#
### dataset 8: exit questions ####
#

# extract exit questions
df_1 <- subset(df_kv_nl, select=c(id,seed,CLAIMEFFICIENCY.SQ001.,CLAIMEFFECTIVENESS.SQ001.,TEXTasFIRSTSTEP.SQ001.,TEXTwithGUI.SQ001.))
df_2 <- subset(df_nl_kv, select=c(id,seed,CLAIMEFFICIENCY.SQ001.,CLAIMEFFECTIVENESS.SQ001.,TEXTasFIRSTSTEP.SQ001.,TEXTwithGUI.SQ001.))

# merge rankings
df_exit <- rbind(df_1, df_2)

# list of seeds to delete (erroneous data or overbooking)
df_exit <- df_exit[!(df_exit$seed %in% "434964103"),]
df_exit <- df_exit[!(df_exit$seed %in% "349662352"),]
df_exit <- df_exit[!(df_exit$seed %in% "688332622"),]
df_exit <- df_exit[!(df_exit$seed %in% "2070192834"),]
df_exit <- df_exit[!(df_exit$seed %in% "1216904780"),]
df_exit <- df_exit[!(df_exit$seed %in% "1916684881"),]
df_exit <- df_exit[!(df_exit$seed %in% "1615544513"),]
df_exit <- df_exit[!(df_exit$seed %in% "694876113"),]
df_exit <- df_exit[!(df_exit$seed %in% "145471250"),]
df_exit <- df_exit[!(df_exit$seed %in% "1714552698"),]
df_exit <- df_exit[!(df_exit$seed %in% "1189809838"),]
df_exit <- df_exit[!(df_exit$seed %in% "2052958529"),]
df_exit <- df_exit[!(df_exit$seed %in% "1474088125"),]
df_exit <- df_exit[!(df_exit$seed %in% "374526227"),]
df_exit <- df_exit[!(df_exit$seed %in% "165728266"),]
df_exit <- df_exit[!(df_exit$seed %in% "1397240311"),]
df_exit <- df_exit[!(df_exit$seed %in% "881869422"),]
df_exit <- df_exit[!(df_exit$seed %in% "1725062158"),]
df_exit <- df_exit[complete.cases(df_exit), ]


# replace text with numbers
# 1 - strongly disagree --> 5 - strongly agree
responseMapping <- c("1 - strongly disagree" = 1, "2" = 2, "3" = 3, "4" = 4, "5 - strongly agree" = 5)
for (i in 3:ncol(df_exit)) {
  df_exit[, i] <- responseMapping[df_exit[, i]]
}

attributes(df_exit)$variable.labels[which(names(df_exit)=="CLAIMEFFICIENCY.SQ001.")] <- "Efficiency of a text-based approach"
names(df_exit)[which(names(df_exit)=="CLAIMEFFICIENCY.SQ001.")] <- "claim.efficiency"
attributes(df_exit)$variable.labels[which(names(df_exit)=="CLAIMEFFECTIVENESS.SQ001.")] <- "Effectiveness of a text-based approach"
names(df_exit)[which(names(df_exit)=="CLAIMEFFECTIVENESS.SQ001.")] <- "claim.effectiveness"
attributes(df_exit)$variable.labels[which(names(df_exit)=="TEXTasFIRSTSTEP.SQ001.")] <- "Using a textual notation as a first step"
names(df_exit)[which(names(df_exit)=="TEXTasFIRSTSTEP.SQ001.")] <- "text.as.first.step"
attributes(df_exit)$variable.labels[which(names(df_exit)=="TEXTwithGUI.SQ001.")] <- "Using a textual notation in combination with a graphical user interface (GUI)"
names(df_exit)[which(names(df_exit)=="TEXTwithGUI.SQ001.")] <- "text.with.gui"

# Preparing data-experiment-exit_prepared.csv

# create running index (subject)
df_exit$subject <- seq.int(nrow(df_exit)) 

# move subject column to first position
df_exit <- df_exit[,c(ncol(df_exit),1:(ncol(df_exit)-1))]

#### export ####
# write to csv
write.csv(df_exit, "../data_prepared/08_data-experiment-exit_prepared.csv")

#
### dataset 9: comments ####
#

# extract kv comments
df_comments_kv_1 <- subset(df_kv_nl, select=c(id,seed,TREATMENT1KVCOMMENTS,RANKINGEXPLAIN,GENERALCOMMENTS))
names(df_comments_kv_1)[which(names(df_comments_kv_1)=="TREATMENT1KVCOMMENTS")] <- "comments.KV"
df_comments_kv_2 <- subset(df_nl_kv, select=c(id,seed,TREATMENT2KVCOMMENTS,RANKINGEXPLAIN,GENERALCOMMENTS))
names(df_comments_kv_2)[which(names(df_comments_kv_2)=="TREATMENT2KVCOMMENTS")] <- "comments.KV"

# merge kv comments
df_comments_kv <- rbind(df_comments_kv_1, df_comments_kv_2)

# extract nl comments
df_comments_nl_1 <- subset(df_nl_kv, select=c(id,seed,TREATMENT1NLCOMMENTS))
names(df_comments_nl_1)[which(names(df_comments_nl_1)=="TREATMENT1NLCOMMENTS")] <- "comments.CNL"
df_comments_nl_2 <- subset(df_kv_nl, select=c(id,seed,TREATMENT2NLCOMMENTS))
names(df_comments_nl_2)[which(names(df_comments_nl_2)=="TREATMENT2NLCOMMENTS")] <- "comments.CNL"

# merge nl comments
df_comments_nl <- rbind(df_comments_nl_1, df_comments_nl_2)

# merge based on seed
df_comments_all = merge(x = df_comments_kv, y = df_comments_nl, by = "seed")

# rearrange columns
col_order <- c("seed", "id.x", "id.y","comments.CNL", "comments.KV", "RANKINGEXPLAIN", "GENERALCOMMENTS")
df_comments_all <- df_comments_all[, col_order]

# list of seeds to delete (errorneous data or overbooking)
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "434964103"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "349662352"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "688332622"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "2070192834"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "1216904780"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "1916684881"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "1615544513"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "694876113"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "145471250"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "1714552698"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "1189809838"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "2052958529"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "1474088125"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "374526227"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "165728266"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "1397240311"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "881869422"),]
df_comments_all <- df_comments_all[!(df_comments_all$seed %in% "1725062158"),]
df_comments_all <- df_comments_all[complete.cases(df_comments_all), ]

# Preparing data-survey-sus_prepared.csv
# header: subject, id, seed, notation, sus

# create running index (subject)
df_comments_all$subject <- seq.int(nrow(df_comments_all)) 

# move subject column to first position
df_comments_all <- df_comments_all[,c(ncol(df_comments_all),1:(ncol(df_comments_all)-1))]

#### export ####
# write to csv
write.csv(df_comments_all, "../data_prepared/09_data-comments_prepared.csv")
