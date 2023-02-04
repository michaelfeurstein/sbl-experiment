library(dplyr)

# Import data ####
profiledata <- read.csv("participant_profiles_anon.csv", sep = ";", dec = ",", header = TRUE)

# Data Preparation ####

##
## prepare programming experience data ####
##

# setup a mapping based on column data:
# mapping text to a number
# the space is missing on purpose
experienceMapping <- c("none"= 0, "beginnerproblems writing easy programs, can read" = 1, "regularcan write basic programs, no problems reading" = 2, "advancedcan write complex programs" = 3, "expertyears of experience" = 4)

# apply mapping
profiledata$experience.r <- experienceMapping[profiledata$PROGRAMMINGskill.SQ001.]

# use coalesce to replace NA's with 0
profiledata$experience.r <- coalesce(profiledata$experience.r, 0)

# create a factor for experience.r with labels
profiledata$experience.r <- factor(profiledata$experience.r, levels = c("0", "1", "2", "3", "4"), labels = c("No experience", "Beginner", "Regular", "Experienced", "Expert"))

##
## prepare programming interest data ####
##

# setup a mapping based on column data:
# mapping text to a number
interestMapping <- c("very low"= 0, "low" = 1, "relatively low" = 2, "relatively high" = 3, "high" = 4, "very high" = 5)

# apply mapping
profiledata$interest.r <- interestMapping[profiledata$PROGRAMMINGinterest.SQ001.]

# create a factor for interest.r with labels
profiledata$interest.r <- factor(profiledata$interest.r, levels = c("0", "1", "2", "3", "4", "5"), labels = c("Very low", "Low", "Relatively low", "relatively high", "High", "Very High"))

##
## prepare profile data ####
##

# use gsub to replace N/A's with No
profiledata <- data.frame(lapply(profiledata, function(x) {gsub("N/A", "No", x)}))

# Export/Import prepared data ####

# the actual dataframe we'll be working with
df_export <- subset(profiledata, select = c("QUALIFICATION", "QUALIFICATION.other.", "OCCUPATION", "OCCUPATION.other.", "DISCIPLINE", "experience.r", "interest.r", "PROGRAMMINGlangs", "Author", "Researcher", "User", "Tutor", "Student", "authorHOWMANYYEARS", "authorHOWMANYVIDEOS", "userHOWMANYYEARS", "userHOWMANYCOURSES", "studentHOWMANYYEARS", "studentHOWMANYCOURSE", "researchHOWMANYYEARS", "researchHOWMANYPUB", "tutorHOWMANYYEARS", "tutorHOWMANYCOURSES"))

# write to csv so we don't need to run above lines too often
write.csv(df_export, "participant_profiles_anon_prepared.csv")

# read from prepared csv
#profiledata <- read.csv("data_profiles_prepared.csv")

#
# Generate visualizations
#

# Qualification ####
profiledata$QUALIFICATION[profiledata$QUALIFICATION.other. == 'Mag.'] <- "Master"
profiledata$Qual_fac <- factor(profiledata$QUALIFICATION, levels = c("Bachelor", "Master",  "PhD", "Other"))
qual_table <- table(profiledata$Qual_fac)
qual_table_prop <- prop.table(qual_table)

# barplot seems useless
barplot(qual_table, main="Qualification")
# number count
qual_table
# fractions
qual_table_prop

# Occupation ####
# clean data
profiledata$OCCUPATION[profiledata$OCCUPATION.other. == 'Assistant Professor'] <- "Professor"

profiledata$Occu_fac <- factor(profiledata$OCCUPATION, levels = c("Bachelor student", "Master student",  "Ph.D. student", "Post-doc", "Researcher", "(Senior) Lecturer", "Professor", "Other"))
occu_table <- table(profiledata$Occu_fac)
occu_table_prop <- round((prop.table(occu_table) * 100),2)

# generate boxplot
par(mar=c(3, 10, 3, 1))
bp <- barplot(occu_table, main="Occupation",horiz=T , las=1, xlim=c(0,13))
text(occu_table, bp,  occu_table_prop, labels = paste(occu_table_prop, '%'), pos = 4, cex = 0.8)

occu_table
occu_table_prop

# Discipline ####
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Business Informatics'] <- "Information Systems"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Information Systems (Wirtschaftsinformatik)'] <- "Information Systems"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Business Information Systems'] <- "Information Systems"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Wirtschaftsinformatik / Business Informatics'] <- "Information Systems"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Business and information systems '] <- "Information Systems"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Computer science engineering'] <- "Computer Science"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Computer Science '] <- "Computer Science"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Computer Science, Human-Computer Interaction'] <- "Computer Science"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Business education'] <- "Business Education"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Applied linguistics'] <- "Languages"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'In languages '] <- "Languages"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Business administration'] <- "Business Administration"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'BSc: Accounting & Controlling, MSc: Supply Chain Management'] <- "Supply Chain Management"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Life sciences '] <- "Life Sciences"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == '/'] <- "Other"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'IT'] <- "Other (IT-related)"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Matura'] <- "Other"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'General education'] <- "Other"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'General qualification '] <- "Other"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Medientechnik'] <- "Other (IT-related)"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Mathematics and IT'] <- "Other (IT-related)"
#profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Electromechanics'] <- "Other (IT-related)"
#profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Electrical Engineering'] <- "Other (IT-related)"
#profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Social Sciences'] <- "Other"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Supply Chain Management'] <- "Logistics"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'International Shipping and Transport Logistics'] <- "Logistics"
profiledata$DISCIPLINE[profiledata$DISCIPLINE == 'Economics '] <- "Economics"
disc <- table(profiledata$DISCIPLINE)
barplot(disc)
disc
disc_prop <- round((prop.table(disc) * 100),2)
disc_prop

#
# Boxplot references
#
# working with axis: https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
#

# Programming Experience ####
par(mar=c(3, 12, 3, 10))
bp_exp <- boxplot(profiledata$experience.r, main = "Programming Experience", yaxt="n")
tickmarks = c(1,2,3,4,5)
axis(2, at = tickmarks, labels = c("No experience", "Beginner", "Regular", "Experienced", "Expert"), las = 2, mgp = c(3,1,0))
summary(profiledata$experience.r)

# Interest in Programming ####
par(mar=c(3, 12, 3, 10))
bp_interest <- boxplot(profiledata$interest.r, main = "Interest in Programming", yaxt="n")
tickmarks = c(1,2,3,4,5,6)
axis(2, at = tickmarks, labels = c("Very low", "Low", "Relatively low", "Relatively high", "High", "Very High"), las = 2, mgp = c(3,1,0))
summary(profiledata$interest.r)

# Programming Languages ####
# Count occurences of programming languages

# prepare a dataframe
columns = c("language","frequency") 
df_l = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_l) = columns

df_l[nrow(df_l) + 1,] = c("Javascript",length(grep("[Jj]avascript|JS", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Python",length(grep("[Pp]ython", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("PHP",length(grep("[Pp][Hh][Pp]", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("R",length(grep("[Rr]", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("C",length(grep("[Cc]", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Java",length(grep("[Jj]ava", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("C#",length(grep("[Cc]#", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("SQL",length(grep("[Ss][Qq][Ll]", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("PyTorch",length(grep("[Pp]y[Tt]orch", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("HTML",length(grep("[Hh][Tt][Mm][Ll]", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Bash",length(grep("[Bb]ash", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Tcl",length(grep("[Tt][Cc][Ll]", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Solidity",length(grep("[Ss]olidity", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("CSS",length(grep("[Cc][Ss][Ss]", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Logic",length(grep("[Ll]ogic", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Julia",length(grep("[Jj]ulia", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Rust",length(grep("[Rr]ust", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Matlab",length(grep("[Mm]atlab", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Gauss",length(grep("[Gg]auss", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Fortran",length(grep("[Ff]ortran", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Maple",length(grep("[Mm]aple", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Pascal",length(grep("[Pp]ascal", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("C++","8") # counted by hand
df_l[nrow(df_l) + 1,] = c("Clipper",length(grep("[Cc]lipper", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Visual Basic",length(grep("[Vv]isual [Bb]asic|VBA", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("XOTcl",length(grep("[Xx][Oo][Tt][Cc][Ll]", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("ooRexx",length(grep("[Oo][Oo][Rr][Ee][Xx][Xx]", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Pearl",length(grep("[Pp]earl", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Delphi",length(grep("[Dd]elphi", profiledata$PROGRAMMINGlangs)))
df_l[nrow(df_l) + 1,] = c("Stata",length(grep("[Ss][Tt][Aa][Tt][Aa]", profiledata$PROGRAMMINGlangs)))

# convert frequency column into numeric format
#print(sapply(df_l, class)) 
df_l$frequency = as.numeric(as.character(df_l$frequency)) 

# order dataframe ascending
# this way it's displayed descending in horizontal barplot
desc_df_l <- df_l[order(df_l$frequency),] 

# plot barplot
counts <- table(desc_df_l$frequency)
par(mar=c(3, 7, 3, 3))
lang_plot <- barplot(height=desc_df_l$frequency, names=desc_df_l$language, main="Programming Languages", horiz=T,las=1,xlim=c(0,30),cex.names = 0.8,cex.axis = 0.8)
text(x=desc_df_l$frequency, lang_plot, labels = paste(desc_df_l$frequency), pos=4, offset=0.2, cex=0.8)

# Participant Profiles ####
authors <- colSums(profiledata == "Yes")
profile_columns = c("Profile","Frequency") 
df_p = data.frame(matrix(nrow = 0, ncol = length(profile_columns))) 
colnames(df_p) = profile_columns
df_p[nrow(df_p) + 1,] = c("Author",authors["Author"])
df_p[nrow(df_p) + 1,] = c("Researcher",authors["Researcher"])
df_p[nrow(df_p) + 1,] = c("User",authors["User"])
df_p[nrow(df_p) + 1,] = c("Tutor",authors["Tutor"])
df_p[nrow(df_p) + 1,] = c("Student",authors["Student"])

# convert frequency column into numeric format
df_p$Frequency = as.numeric(as.character(df_p$Frequency))

# order dataframe ascending
# this way it's displayed descending in horizontal barplot
desc_df_p <- df_p[order(df_p$Frequency),] 

# plot barplot
counts <- table(df_p$Frequency)
par(mar=c(3, 7, 3, 3))
profile_plot <- barplot(height=desc_df_p$Frequency, names=desc_df_p$Profile, main="Participant Profiles", horiz=T,las=1,xlim=c(0,25),width = c(2,2,2,2,2))
text(x=desc_df_p$Frequency, profile_plot, labels = paste(desc_df_p$Frequency), pos=4, offset=0.3, xpd=T)

# Author ####
## Years active ####
profiledata$authorHOWMANYYEARS <- as.numeric(as.character(profiledata$authorHOWMANYYEARS))
summary(na.omit(profiledata$authorHOWMANYYEARS))

## Videos created ####
profiledata$authorHOWMANYVIDEOS <- as.numeric(as.character(profiledata$authorHOWMANYVIDEOS))
summary(na.omit(profiledata$authorHOWMANYVIDEOS))

# User ####
## Years active ####
profiledata$userHOWMANYYEARS <- as.numeric(as.character(profiledata$userHOWMANYYEARS))
summary(na.omit(profiledata$userHOWMANYYEARS))

## Courses ####
profiledata$userHOWMANYCOURSES <- as.numeric(as.character(profiledata$userHOWMANYCOURSES))
summary(na.omit(profiledata$userHOWMANYCOURSES))

# Student ####
## Years studying ####
profiledata$studentHOWMANYYEARS <- as.numeric(as.character(profiledata$studentHOWMANYYEARS))
summary(na.omit(profiledata$studentHOWMANYYEARS))

## Courses attended that used educational videos ####
profiledata$studentHOWMANYCOURSE <- as.numeric(as.character(profiledata$studentHOWMANYCOURSE))
summary(na.omit(profiledata$studentHOWMANYCOURSE))

# Researcher ####
## Years researching ####
profiledata$researchHOWMANYYEARS <- as.numeric(as.character(profiledata$researchHOWMANYYEARS))
summary(na.omit(profiledata$researchHOWMANYYEARS))

## Papers published ####
profiledata$researchHOWMANYPUB <- as.numeric(as.character(profiledata$researchHOWMANYPUB))
summary(na.omit(profiledata$researchHOWMANYPUB))

# Tutor ####
## Years active as tutor ####
profiledata$tutorHOWMANYYEARS <- as.numeric(as.character(profiledata$tutorHOWMANYYEARS))
summary(na.omit(profiledata$tutorHOWMANYYEARS))

## Courses ####
profiledata$tutorHOWMANYCOURSES <- as.numeric(as.character(profiledata$tutorHOWMANYCOURSES))
summary(na.omit(profiledata$tutorHOWMANYCOURSES))
