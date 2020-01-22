rm(list=ls())
load(file="Input/Demo.rdata")


agegrp <- c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
            "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
            "90-94", "95-99", "100+")

agespan <- c(1, 4, rep(5, 19), 1)


locations <- unique(dat_death$Location)


dat_full <- list()

for (loc in locations) {
  dat <- list(Location=loc)
  
  ############################
  ######## Birth data ########
  ############################
  
  
  bir <- subset(dat_birth, Location==loc & Variant=="Medium")
  
  temp <- tapply(bir$ASFR, list(bir$AgeGrp, bir$Time), sum) / 1e3
  temp <- apply(temp, 2, rep, each=5)
  temp <- apply(temp, 1, rep, each=5)
  colnames(temp) <- 15:49
  rownames(temp) <- 2000:2099
  
  dat$BirR <- temp
  
  
  temp <- tapply(bir$Births, list(bir$AgeGrp, bir$Time), sum) * 1e3 / 5 / 5
  temp <- apply(temp, 2, rep, each=5)
  temp <- apply(temp, 1, rep, each=5)
  colnames(temp) <- 15:49
  rownames(temp) <- 2000:2099
  
  dat$BirN <- temp
  
  
  ############################
  ######## Death data ########
  ############################
  
  
  dea <- subset(dat_death, Location==loc)
  
  temp <- tapply(dea$mx, list(dea$AgeGrp, dea$Time, dea$Sex), sum)[agegrp, , ]
  temp <- apply(temp, c(2, 3), rep, times=agespan)
  temp <- apply(temp, c(1, 3), rep, each=5)
  
  dimnames(temp)[[1]] <- 2000:2099
  dimnames(temp)[[2]] <- 0:100
  
  dat$DeaR_F <- temp[, , "Female"]
  dat$DeaR_M <- temp[, , "Male"]
  dat$DeaR_T <- temp[, , "Total"]
  
  
  #################################
  ######## Population data ########
  #################################
  
  
  pop <- subset(dat_pop, Location==loc)
  
  temp <- tapply(pop$PopFemale, list(pop$Time, pop$AgeGrp), sum) * 1e3
  colnames(temp) <- 0:100
  dat$Pop_F <- temp
  
  temp <- tapply(pop$PopMale, list(pop$Time, pop$AgeGrp), sum) * 1e3
  colnames(temp) <- 0:100
  dat$Pop_M <- temp
  
  temp <- tapply(pop$PopTotal, list(pop$Time, pop$AgeGrp), sum) * 1e3
  colnames(temp) <- 0:100
  dat$Pop_T <- temp
  
  
  
  dat_full[[loc]] <- dat
}


save(dat_full, file="Input/DataByCountry.rdata")

