load("Total.RData")
library(data.table)
str(Total)

Total$Ageclass <- as.numeric(as.character(Total$Ageclass))
Total <- data.table(Total)

mxtot <- Total %>% filter(Year==2010,CNT=="IT")

mxtot <-mxtot[order(as.numeric(as.character(mxtot$Ageclass))),]
plot(mxtot$mx[c(1:17)])


rates <- c(mxtot$mx,mxtot$HCR)

length(mxtot$mx)

length(mxtot$HCR)

start.age <- 0
open.age <- 80
seq(start.age,open.age,5)

AGE=c(seq(start.age,open.age,5))

age=AGE
sex = 'Male'
#Sullivan.fun = function (rates,age=AGE, sex=Gender) {
  # 1) First, we split from our single vector 'rates' the set of age-specific death rates (mx) and age-specific prevalence of disability (wx)
  lengthvec <- length(rates)
  mx <- rates[1:(lengthvec / 2)]
  wx <- rates[(lengthvec / 2 + 1):lengthvec]
  
  # 2) Calculating period life table functions
  # ax
  length(wx)
  length(diff(age))
  n <- c(diff(age), 1)
  ax <- 0.5 * n
  # This part of the code is only for calculations when the
  # starting age is zero.
  # Formulas are from Andreev & Kingkade (2015)
  #---------------------------------------------------------
  if (age[1] == 0) {
    if (sex == 'Male') {
      ax[1] <- ifelse(mx[1] >= 0.08307,0.29915,
                      ifelse(mx[1] < 0.023,
                             0.14929 - 1.99545 * mx[1],
                             0.02832 + 3.26021 * mx[1] ))}
    if (sex == 'Female') {
      ax[1] <- ifelse(mx[1] >= 0.06891,0.31411,
                      ifelse(mx[1] < 0.01724,
                             0.14903 - 2.05527 * mx[1],
                             0.04667 + 3.88089 * mx[1] ))} }
  #---------------------------------------------------------
  # probability of dying (qx) and surviving (px)
  length(n)
  length(mx)
  
  qx <- (n * mx)/(1 + (n - ax) * mx)
  length(qx)
  
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  px <- 1 - qx
  # survivors at age x (lx)
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths between ages x and x+n (dx)
  dx <- lx * qx
  # person-years lived between ages x and x+n (Lx)
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  # 3) Person-years lived without disability
  Lx.health <- Lx*(1-wx)
  # 4) Healthy Life expectancy at age 0
  ex.health <- sum(Lx.health)/lx[1]
  ex.health
  
  Tx <- rev(cumsum(rev(Lx)))
  
  ex <- Tx/lx
  ex[1] 
  
  return(ex.health)
}



mx <- c(mx$mx,mx$HCR)