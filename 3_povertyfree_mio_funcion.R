e0.frommx <- function(nmx =  mx,wx, sex=1, age = c(seq(0, 80, 5)), nax = NULL){
  n   <- c(diff(age), 999)
  
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == 1) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == 2) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  e0 <- ex[1]
  
  Lx.health <-  nLx*(1-wx)
  # 4) Healthy Life expectancy at age 0
  ex.health <- sum(Lx.health)/lx
  ex.health <-  ex.health[1]
  
  return(list=c(e0,ex.health))
}

load("Total.RData")

unique(Total$CNT)
library(data.table)
library(tidyverse)

Total$Ageclass <- as.numeric(as.character(Total$Ageclass))


mxtot <- Total %>% filter(Year==2010,CNT=="IT")

mxtot <-mxtot[order(as.numeric(as.character(mxtot$Ageclass))),]
plot(mxtot$mx[c(1:17)])
e0.frommx(mxtot$mx,sex = 1,wx = mxtot$HCR)


###################

head(Total)

Total <- Total %>%mutate(Year=as.numeric(as.character(Year)))  %>% filter(CNT!="DE",Year>2006)

Y <- 2007:2019

#Total$HCR <- Total$HCR/5

str(Total)

Total$CNT <- as.character(Total$CNT)
fill_ <- function(data,Y,cnt=cnt){
  
  Res <- matrix(nrow =length(Y) ,ncol =4)
  Res[,1] <- Y
  Res[,2] <- cnt
  for (i in 1:length(Y)) {
    
    year <- Y[i]
    
    mxtot <- Total %>% filter(Year==year,CNT==cnt)
    #mxtot <-mxtot[order(as.numeric(as.character(mxtot$Ageclass))),]
    Res[i,3] <- e0.frommx(mxtot$mx,sex = 1,wx = mxtot$HCR)[1]
    Res[i,4] <- e0.frommx(mxtot$mx,sex = 1,wx = mxtot$HCR)[2]
    

    
  }
  return(Res)
}

  
prova<- unique(Total$CNT)[1]
fill_(data=Total,Y=2007:2019,cnt=prova)

# 
# Res <- matrix(nrow =length(Y) ,ncol =3)
# Res[,1] <- Y
# for (i in 1:length(Y)) {
#   
#   year <- Y[i]
#   
#   mxtot <- Total %>% filter(Year==year,CNT=="IT")
#   mxtot <-mxtot[order(as.numeric(as.character(mxtot$Ageclass))),]
#   Res[i,2] <- e0.frommx(mxtot$mx,sex = 1,wx = mxtot$HCR)[1]
#   Res[i,3] <- e0.frommx(mxtot$mx,sex = 1,wx = mxtot$HCR)[2]
#   
# 
# }

Y

Res2 <- list()

country <- unique(Total$CNT)
for (i in 1:length(country)) {
  cnt <- country[i]
  

  Res2[[cnt]] <-  fill_(data=Total,Y=Y,cnt=cnt)
  
}



Res_Total <-  do.call(rbind.data.frame, Res2)
rownames(Res_Total) <- NULL

str(Res_Total)
Res_Total <- Res_Total %>% rename(Year=V1,Country=V2,
                     LE=V3, Poverty_free_LE=V4)

Res_Total$Year <- as.numeric(Res_Total$Year)
Res_Total$LE <- as.numeric(Res_Total$LE)
Res_Total$Poverty_free_LE <- as.numeric(Res_Total$Poverty_free_LE)


plot <- Res_Total %>% ggplot(aes(Year,LE,col="LE"))+geom_line(size=1)+geom_line(aes(Year,Poverty_free_LE,color="PFLE"),size=1)+
  labs(y= "Value")+
  facet_wrap(~Country)+theme_minimal(18)

plot

cor(Res_Total$Poverty_free_LE,Res_Total$LE)^2

ggsave("pfLE.pdf",plot, width = 12, height = 10)

