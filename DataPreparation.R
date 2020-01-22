# 
# dat_birth <- read.csv("Input/WPP2019_Fertility_by_Age.csv", stringsAsFactors=F)
# dat_death <- read.csv("Input/WPP2019_Life_Table.csv", stringsAsFactors=F)
# dat_pop <- read.csv("Input/WPP2019_PopulationByAgeSex_Medium.csv", stringsAsFactors=F)
# 
# 
# dat_birth$Location[dat_birth$Location == "China, Taiwan Province of China"] <- "Taiwan, ROC"
# dat_death$Location[dat_death$Location == "China, Taiwan Province of China"] <- "Taiwan, ROC"
# dat_pop$Location[dat_pop$Location == "China, Taiwan Province of China"] <- "Taiwan, ROC"
# 
# time_start <- 2000
# time_end <- 2100
# 
# dat_birth <- subset(dat_birth, MidPeriod >= time_start & MidPeriod <= time_end)
# dat_death <- subset(dat_death, MidPeriod >= time_start & MidPeriod <= time_end)
# dat_pop <- subset(dat_pop, MidPeriod >= time_start & MidPeriod <= time_end)
# save(dat_birth, dat_death, dat_pop, file="Input/Demo.rdata")


rm(list=ls())
load(file="Input/Demo.rdata")


############################
######## Birth data ########
############################


