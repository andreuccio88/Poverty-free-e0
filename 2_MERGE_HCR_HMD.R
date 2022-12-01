library(readxl)
library(tidyverse)
library(reshape2)

HCR <- read_excel("HCR 2004-2020.xls")
HCR

HCR$Ageclass <-gsub("classe","",HCR$Ageclass) 

unique(HCR$CNT)


HCR <- melt(HCR) %>% rename(Year="variable", HCR="value") 
head(HCR)

HCR$Ageclass[HCR$Ageclass==" 00-04"] <- 0
HCR$Ageclass[HCR$Ageclass==" 05-09"] <- 1
HCR$Ageclass[HCR$Ageclass==" 10-14"] <- 2
HCR$Ageclass[HCR$Ageclass==" 15-19"] <- 3
HCR$Ageclass[HCR$Ageclass==" 20-24"] <- 4
HCR$Ageclass[HCR$Ageclass==" 25-29"] <- 5
HCR$Ageclass[HCR$Ageclass==" 30-34"] <- 6
HCR$Ageclass[HCR$Ageclass==" 35-39"] <- 7
HCR$Ageclass[HCR$Ageclass==" 40-44"] <- 8
HCR$Ageclass[HCR$Ageclass==" 45-49"] <- 9
HCR$Ageclass[HCR$Ageclass==" 50-54"] <- 10
HCR$Ageclass[HCR$Ageclass==" 55-59"] <- 11
HCR$Ageclass[HCR$Ageclass==" 60-64"] <- 12
HCR$Ageclass[HCR$Ageclass==" 65-69"] <- 13
HCR$Ageclass[HCR$Ageclass==" 70-74"] <- 14
HCR$Ageclass[HCR$Ageclass==" 75-79"] <- 15
HCR$Ageclass[HCR$Ageclass==" 80+"] <- 16

head(HCR)

HCR$Ageclass <- as.factor(HCR$Ageclass)
HCR$CNT <- as.factor(HCR$CNT)
HCR <- HCR %>% filter(CNT%in%c("DK","IT","BG","DE","HU"))
HCR$Year <- as.factor(HCR$Year)

str(HCR)

load("All.RData")
All$Year <- as.factor(All$Year)
All$Ageclass <- as.factor(All$Ageclass)
All$CNT <- as.factor(All$CNT)
str(All)
str(HCR)


Total=HCR %>% left_join(All)


Total %>%mutate(Year=as.numeric(as.character(Year))) %>%  filter(Year>=2007,Year<=2019)
save(Total,file = "Total.RData")

