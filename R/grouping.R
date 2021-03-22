reframe_agp_sex <- function(dat, year0, year1, agp, agl) {
  years <- dat$FerR$Time
  
  stopifnot(year0 < year1)
  stopifnot(year1 < dat$Range[2])
  stopifnot(dat$Range[1] < year0)
  
  n_agp <- length(unique(agp))
  ageing <- as.vector(1 / table(agp))
  ageing[length(ageing)] <- 0
  
  yrs <- years[-1]
  
  pop <- array(0, c(nrow(dat$PopN_F), n_agp, 2))
  pop[, , 1] <- t(apply(dat$PopN_F[, -1], 1, function(x) { tapply(x, agp, sum) }))
  pop[, , 2] <- t(apply(dat$PopN_M[, -1], 1, function(x) { tapply(x, agp, sum) }))
  #pop <- pop[-dim(pop)[1], , ]
  
  pop_fr <- (pop[-1, , ] + pop[-dim(pop)[1], , ]) / 2  # 1951 - 2099
  pop_to <- pop_fr[-1, , ]  # 1952 - 2099
  pop_fr <- pop_fr[-dim(pop_fr)[1], , ]  # 1951 - 2099 
  pop_mi <- pop[2:(nrow(pop) - 1), , ]
  
  # plot(1950:2100 + 0.5, apply(pop, 1, sum), type ="l")
  # points(yrs, apply(pop_fr, 1, sum), col = 2)
  # points(yrs + 1, apply(pop_to, 1, sum), col = 3)
  # points(yrs + 0.5, apply(pop_mi, 1, sum), col = 4)
  
  dea <- array(0, c(nrow(dat$DeaR_F) - 1, n_agp, 2))
  
  
  pm_f <- dat$PopN_F[, -1]
  pm_f <- pm_f[2:(nrow(pm_f) - 1)]
  
  fer <- t(apply(pm_f * dat$FerR[-1, -1], 1, function(x) { tapply(x, agp, sum) }))
  fer <- fer / pop_mi[, , 1]
  
  dea[, , 1] <- t(apply(pm_f * dat$DeaR_F[-1, -1], 1, function(x) { tapply(x, agp, sum) }))
  
  
  pm_m <- dat$PopN_M[, -1]
  pm_m <- pm_f[2:(nrow(pm_m) - 1)]
  
  dea[, , 2] <- t(apply(pm_m * dat$DeaR_M[-1, -1], 1, function(x) { tapply(x, agp, sum) }))
  
  dea <- dea / pop_mi
  
  i_sel <- (yrs >= year0) & (yrs <= (year1 + 1))
  
  dimnames(pop_fr)[[1]] <- dimnames(pop_mi)[[1]] <- dimnames(pop_to)[[1]] <- yrs
  dimnames(pop_fr)[[2]] <- dimnames(pop_mi)[[2]] <- dimnames(pop_to)[[2]] <- unique(agl)
  dimnames(fer)[[1]] <- dimnames(dea)[[1]] <- yrs
  dimnames(fer)[[2]] <- dimnames(dea)[[2]] <- unique(agl)
  
  return(list(
    Year = yrs[i_sel],
    PopFr = pop_fr[i_sel, , ],
    PopMi = pop_mi[i_sel, , ],
    PopTo = pop_to[i_sel, , ],
    FerR = fer[i_sel, ],
    DeaR = dea[i_sel, , ],
    AgeR = ageing,
    AgpN = n_agp 
  ))
}
