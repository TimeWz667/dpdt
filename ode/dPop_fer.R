## ODEs -----
deriv(Y[, ]) <- d_pop[i, j]
dim(Y) <- c(n_agp, 2)


## Initial values -----
initial(Y[, ]) <- Y0[i, j]

Y0[, ] <- user()
dim(Y0) <- c(n_agp, 2)



## dims -----
ageing[] <- user()
dim(ageing) <- n_agp

tt[] <- user() # data times for AIM by 1
dim(tt) <- user()


## lengths -----
n_agp <- user()
n_tt <- length(tt)


## birth -----
r_fer[, ] <- user()
dim(r_fer) <- c(n_tt, n_agp)

r_fer_t[] <- interpolate(tt, r_fer, "constant")
dim(r_fer_t) <- c(n_agp)

sex_ratio <- user(1.07)

births[, 1] <- r_fer_t[i] * Y[i, 1] / (1 + sex_ratio) 
births[, 2] <- r_fer_t[i] * Y[i, 1] * sex_ratio / (1 + sex_ratio) 
dim(births) <- c(n_agp, 2)


## death + migration -----
r_dea[, , ] <- user()
dim(r_dea) <- c(n_tt, n_agp, 2)
r_dea_t[, ] <- interpolate(tt, r_dea, "constant")
dim(r_dea_t) <- c(n_agp, 2)

deaths[, ] <- r_dea_t[i, j] * Y[i, j]
dim(deaths) <- c(n_agp, 2)


r_mig[, , ] <- user()
dim(r_mig) <- c(n_tt, n_agp, 2)
r_mig_t[, ] <- interpolate(tt, r_mig, "constant")
dim(r_mig_t) <- c(n_agp, 2)

migrations[, ] <- r_mig_t[i, j] * Y[i, j]
dim(migrations) <- c(n_agp, 2)

N <- sum(Y)
output(N) <- TRUE
output(N_Birth) <- sum(births)
output(N_Death) <- sum(deaths)
output(N_Mig) <- sum(migrations)

output(NatureInc) <- (sum(births) - sum(deaths)) / N
output(SocialInc) <- sum(migrations) / N

output(BirthRate[]) <- sum(births) / N
dim(BirthRate) <- 2

output(DeathRate[, ]) <- r_dea_t[i, j]
dim(DeathRate) <- c(n_agp, 2)
output(MigrationRate[, ]) <- r_mig_t[i, j]
dim(MigrationRate) <- c(n_agp, 2)
output(FerRate[]) <- r_fer_t[i]
dim(FerRate) <- n_agp
output(PrBirthFemale) <- 1 / (1 + sex_ratio)

  
d_pop[1, ] <- sum(births[, j]) - ageing[i] * Y[1, j] + migrations[i, j] - deaths[i, j]
d_pop[2:n_agp, ] <- ageing[i - 1] * Y[i - 1, j] - ageing[i] * Y[i, j] + migrations[i, j] - deaths[i, j]
dim(d_pop) <- c(n_agp, 2)
