library(tidyverse)
library(blockrand)

# first batch of participants n = 48 (power = 80%)

# stratified randomization for each stratum 
beginner.professional.list <- blockrand(n=12, num.levels = 2, levels = c("NL-KV", "KV-NL"), block.sizes = c(2), stratum='beginner, professional', id.prefix='bp', block.prefix='bp')
advanced.professional.list <- blockrand(n=12, num.levels = 2, levels = c("NL-KV", "KV-NL"), block.sizes = c(2), stratum='advanced, professional', id.prefix='ap', block.prefix='ap')

beginner.student.list <- blockrand(n=12, num.levels = 2, levels = c("NL-KV", "KV-NL"), block.sizes = c(2), stratum='beginner, student', id.prefix='bs', block.prefix='bs')
advanced.student.list <- blockrand(n=12, num.levels = 2, levels = c("NL-KV", "KV-NL"), block.sizes = c(2), stratum='advanced, student', id.prefix='as', block.prefix='as')

# order by treatment for zoom sessions
df.bp <- beginner.professional.list[order(beginner.professional.list$treatment),]
df.ap <- advanced.professional.list[order(advanced.professional.list$treatment),]

df.bs <- beginner.student.list[order(beginner.student.list$treatment),]
df.as <- advanced.student.list[order(advanced.student.list$treatment),]

# export to csv for private logging of participants with zoom sessions
write.csv(df.bp,"randomization/participants_beginner_professional.csv", row.names = FALSE)
write.csv(df.ap,"randomization/participants_advanced_professional.csv", row.names = FALSE)

write.csv(df.bs,"randomization/participants_beginner_student.csv", row.names = FALSE)
write.csv(df.as,"randomization/participants_advanced_student.csv", row.names = FALSE)
