require(tidyverse)

dat <- dpdt::fetch_demography("ByCountry/Malawi")

dat$FerR[, "50"] <- 0

agp = c(rep(1:15, each = 5), rep(16, 26))
agl = c(rep(paste0("[", 0:14 * 5, ",", 1:15 * 5, ")"), each = 5), rep("[75,Inf)", 26))


year0 <- 1970
year1 <- 2035
sex_ratio = 107


n_agp <- length(unique(agp))
ageing <- as.vector(1 / table(agp))
ageing[length(ageing)] <- 0


f <- "ode/dPop.R"
model <- odin::odin(f)


fn <- function(x, rsd, i, cm, y0, y1, yr0, yr) {
  rsd[i, , ] <- array(x, dim(y0))

  cm$set_user(user = list(r_dea = rsd, Y0 = y0))
  ys = cm$run(seq(yr0, yr + 1, 0.25))
  sum((ys[nrow(ys), startsWith(colnames(ys), "Y[")] - y1) ^ 2)
}



years <- dat$FerR$Time

pop <- array(0, c(nrow(dat$PopN_F), n_agp, 2))
pop[, , 1] <- t(apply(dat$PopN_F[, -1], 1, function(x) { tapply(x, agp, sum) }))
pop[, , 2] <- t(apply(dat$PopN_M[, -1], 1, function(x) { tapply(x, agp, sum) }))

pop_fr <- pop[1:(nrow(dat$PopN_F) - 1), , ]
pop_to <- pop[-1, , ]
pop_mi <- (pop_fr + pop_to) / 2

pop_fr <- pop_fr[years >= year0 & years <= year1, , ]
pop_to <- pop_to[years >= year0 & years <= year1, , ]



i_ti <- dat$FerR$Time
i_ti <- i_ti >= year0 & i_ti <= (year1 + 1)


dea <- array(0, c(nrow(dat$DeaR_F), n_agp, 2))
pm <- (dat$PopN_F[-1, -1] + dat$PopN_F[-nrow(dat$PopN_F), -1]) / 2
dea[, , 1] <- t(apply(pm * dat$DeaR_F[, -1], 1, function(x) { tapply(x, agp, sum) }))
fer <- t(apply(pm * dat$FerR[, -1], 1, function(x) { tapply(x, agp, sum) }))
 

pm <- (dat$PopN_M[-1, -1] + dat$PopN_M[-nrow(dat$PopN_F), -1]) / 2
dea[, , 2] <- t(apply(pm * dat$DeaR_M[, -1], 1, function(x) { tapply(x, agp, sum) }))

dea <- dea[i_ti, , ] / pop_mi[i_ti, , ]
bir <- (rowSums(fer[i_ti, ]) / apply(pop_mi[i_ti, , ], 1, sum)) %*% t(c(100, sex_ratio) / (100 + sex_ratio))



pars <- list(
  tt = years[i_ti],
  n_agp = n_agp,
  r_bir = bir,
  r_dea = dea,
  r_mig = array(0, dim(dea)),
  ageing = ageing,
  Y0 = pop_fr[1, , ]
)


cm <- model(user = pars)



years <- year0:year1


rsd <- dea
se <- rep(0, length(years))


for(i in 1:length(years)) {
  opti <- optim(c(rsd[i, , ]), fn, method = "L-BFGS-B", lower = -2, upper = 2, 
                rsd = rsd, i = i, cm = cm, 
                y0 = pop_fr[1, , ], y1 = pop_to[i, , ], 
                yr0 = year0, yr = years[i])
  
  
  rsd[i, , ] <- array(opti$par, dim(pop_to[1, , ]))
  
  se[i] <- opti$value

  print(years[i])
}




pars_demo <- pars
pars_demo$r_dea <- dea
pars_demo$r_mig <- dea - rsd



cm$set_user(user = pars_demo)


ys <- cm$run(seq(year0, year1 + 1, 0.1))
#ys <- ys[round(ys[, "t"]) == ys[, "t"],]


(matrix(ys[nrow(ys), 2:33], 16, 2) - pop_to[length(years), , ]) / pop_to[length(years), , ]


save(pars_demo, agl, ys, file = "../../SCALE/pars_HitTB_SCALE/data/Pars_Demography_Malawi.rdata")


