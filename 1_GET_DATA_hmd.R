library(tidyverse) # version 1.0.0
library(HMDHFDplus) # version 1.1.8

#country <- getHMDcountries()



myusername <- "andrea.nigri@student.unisi.it"
mypassword <- "Dottorato17"


country <- c("DNK","ITA","BGR","DEUTNP","HUN")
             
           

  Ex <- list()
  for (i in 1: length(country)) {
    cnt <- country[i]
    Ex[[cnt]] <- readHMDweb(cnt, "Exposures_1x1",
                            myusername, mypassword)
    
    # let's print the progress
    paste(i,'out of',length(country)) 
  }

#load("Ex.RData")

Ex
Ex <- do.call(rbind.data.frame, Ex)
Ex$CNT <- rownames(Ex) 
Ex$CNT<- gsub('[0-9.]', '', Ex$CNT)
Ex
rownames(Ex) <- NULL
head(Ex)
tail(Ex)


Ex$Ageclass <- NA
Ex$Ageclass[Ex$Age%in%c(0:4)] <- 0
Ex$Ageclass[Ex$Age%in%c(5:9)] <- 1
Ex$Ageclass[Ex$Age%in%c(10:14)] <- 2 
Ex$Ageclass[Ex$Age%in%c(15:19)] <- 3
Ex$Ageclass[Ex$Age%in%c(20:24)] <- 4
Ex$Ageclass[Ex$Age%in%c(25:29)] <- 5
Ex$Ageclass[Ex$Age%in%c(30:34)] <- 6
Ex$Ageclass[Ex$Age%in%c(35:39)] <- 7
Ex$Ageclass[Ex$Age%in%c(40:44)] <- 8
Ex$Ageclass[Ex$Age%in%c(45:49)] <- 9
Ex$Ageclass[Ex$Age%in%c(50:54)] <- 10
Ex$Ageclass[Ex$Age%in%c(55:59)] <- 11
Ex$Ageclass[Ex$Age%in%c(60:64)] <- 12
Ex$Ageclass[Ex$Age%in%c(65:69)] <- 13
Ex$Ageclass[Ex$Age%in%c(70:74)] <- 14
Ex$Ageclass[Ex$Age%in%c(75:79)] <- 15
Ex$Ageclass[Ex$Age>=80] <- 16


Ex$CNT[Ex$CNT=="DNK"] <- "DK"
Ex$CNT[Ex$CNT=="ITA"] <- "IT"
Ex$CNT[Ex$CNT=="BGR"] <- "BG"
Ex$CNT[Ex$CNT=="DNK"] <- "DK"
Ex$CNT[Ex$CNT=="DEUTNP"] <- "DE"
Ex$CNT[Ex$CNT=="HUN"] <- "HU"

unique(Ex$CNT)

head(Ex)
Ex2 <-Ex[,c(1,5,7,8)]

head(Ex)
head(Ex2)



#Ex %>% filter(Ageclass=="20-24",CNT=="BG",Year=="1947") %>% select(Total) %>% sum()

Ex2 <- aggregate(Ex2$Total,by = list(Ex2$Ageclass,Ex2$Year,Ex2$CNT),FUN=sum)%>% 
  rename(Ageclass=Group.1,Year=Group.2, CNT=Group.3, Expo=x) 

head(Ex2)
unique(Ex2$CNT)
######################################################
# Dx
##################################################

 Dx <- list()
 for (i in 1: length(country)) {
   cnt <- country[i]
   Dx[[cnt]] <- readHMDweb(cnt, "Deaths_1x1",
                           myusername, mypassword)
   
   # let's print the progress
   paste(i,'out of',length(country)) 
 }

#load("Dx.RData")
Dx
Dx <- do.call(rbind.data.frame, Dx)
Dx$CNT <- rownames(Dx) 
Dx$CNT<- gsub('[0-9.]', '', Dx$CNT)
Dx
rownames(Dx) <- NULL
head(Dx)
tail(Dx)


Dx$Ageclass <- NA
Dx$Ageclass[Dx$Age%in%c(0:4)] <- 0
Dx$Ageclass[Dx$Age%in%c(5:9)] <- 1
Dx$Ageclass[Dx$Age%in%c(10:14)] <- 2
Dx$Ageclass[Dx$Age%in%c(15:19)] <- 3
Dx$Ageclass[Dx$Age%in%c(20:24)] <- 4
Dx$Ageclass[Dx$Age%in%c(25:29)] <- 5
Dx$Ageclass[Dx$Age%in%c(30:34)] <- 6
Dx$Ageclass[Dx$Age%in%c(35:39)] <- 7
Dx$Ageclass[Dx$Age%in%c(40:44)] <- 8
Dx$Ageclass[Dx$Age%in%c(45:49)] <- 9
Dx$Ageclass[Dx$Age%in%c(50:54)] <- 10
Dx$Ageclass[Dx$Age%in%c(55:59)] <- 11
Dx$Ageclass[Dx$Age%in%c(60:64)] <- 12
Dx$Ageclass[Dx$Age%in%c(65:69)] <- 13
Dx$Ageclass[Dx$Age%in%c(70:74)] <- 14
Dx$Ageclass[Dx$Age%in%c(75:79)] <- 15
Dx$Ageclass[Dx$Age>=80] <- 16


Dx$CNT[Dx$CNT=="DNK"] <- "DK"
Dx$CNT[Dx$CNT=="ITA"] <- "IT"
Dx$CNT[Dx$CNT=="BGR"] <- "BG"
Dx$CNT[Dx$CNT=="DNK"] <- "DK"
Dx$CNT[Dx$CNT=="DEUTNP"] <- "DE"
Dx$CNT[Dx$CNT=="HUN"] <- "HU"

head(Dx)
Dx2 <-Dx[,c(1,5,7,8)]

head(Dx)
head(Dx2)


unique(Dx2$Ageclass)

unique(Ex2$Ageclass)



#Dx %>% filter(Ageclass=="20-24",CNT=="BG",Year=="1947") %>% select(Total) %>% sum()

Dx2 <- aggregate(Dx2$Total,by = list(Dx2$Ageclass,Dx2$Year,Dx2$CNT),FUN=sum)%>% 
  rename(Ageclass=Group.1,Year=Group.2, CNT=Group.3, Dx=x) 

All <- cbind(Dx2,Ex2)
All <- All[,c(1,2,3,4,8)]


unique(All$CNT)

unique(All$Ageclass)


All$mx <- All$Dx/All$Expo
str(All)
str(All)
head(All)
All$CNT
tail(All)


All %>% filter(Year==2010)%>% ggplot(aes(Ageclass,log(mx)))+geom_point()+facet_wrap(~CNT)

save(All,file = "All.RData")


