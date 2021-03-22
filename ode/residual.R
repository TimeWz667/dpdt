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
bir[, ] <- user()
dim(bir) <- c(n_tt, 2)

bir_t[] <- interpolate(tt, bir, "constant")
dim(bir_t) <- 2


births[] <- bir_t[i] * sum(Y[, i])
dim(births) <- 2


output(N_Birth) <- sum(births)

## death + migration -----
rsd[, , ] <- user()
dim(rsd) <- c(n_tt, n_agp, 2)
rsd_t[, ] <- interpolate(tt, rsd, "constant")
dim(rsd_t) <- c(n_agp, 2)


d_pop[1, ] <- sum(births[j]) - (rsd_t[1, j] + ageing[i]) * Y[1, j]
d_pop[2:n_agp, ] <- ageing[i - 1] * Y[i - 1, j] - (rsd_t[i, j] + ageing[i]) * Y[i, j]
dim(d_pop) <- c(n_agp, 2)
