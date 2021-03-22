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
r_bir[, ] <- user()
dim(r_bir) <- c(n_tt, 2)

r_bir_t[] <- interpolate(tt, r_bir, "constant")
dim(r_bir_t) <- 2


births[] <- r_bir_t[i] * sum(Y[, i])
dim(births) <- 2


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

output(N) <- sum(Y)
output(N_Birth) <- sum(births) / sum(Y)
output(N_Death) <- sum(deaths) / sum(Y)
output(N_Mig) <- sum(migrations) / sum(Y)


d_pop[1, ] <- births[j] - ageing[i] * Y[1, j] + migrations[i, j] - deaths[i, j]
d_pop[2:n_agp, ] <- ageing[i - 1] * Y[i - 1, j] - ageing[i] * Y[i, j] + migrations[i, j] - deaths[i, j]
dim(d_pop) <- c(n_agp, 2)
