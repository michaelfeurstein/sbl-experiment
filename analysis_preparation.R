### IMPORT DATA  ####

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

# boxcox transform sus
mydata$sus.boxcox = (mydata$sus^3.2-1)/3.2

# the actual dataframe we'll be working with
df <- subset(mydata, select = c("subject", "sequence", "period", "group.r", "notation.r", "duration.r", "duration.log", "accuracy", "sus", "sus.boxcox", "rank.r"))

# write to csv so we don't need to run above lines too often
write.csv(df, "data_prepared.csv")

# read from prepared csv
df <- read.csv("data_prepared.csv")