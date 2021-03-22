calc_migration_agp <- function(p0, p1, br, dr, ageing, n_agp, optim_fn) {
  p0 <- matrix(p0, n_agp, 1)
  p1 <- matrix(p1, n_agp, 1)
  
  BAD <- diag(- dr - ageing)
  BAD[1, ] <- BAD[1, ] + br
  BAD[2:n_agp, 1:(n_agp - 1)] <- BAD[2:n_agp, 1:(n_agp - 1)] + diag(ageing[1:(n_agp - 1)])
  
  trBAD <- expm::expm(BAD)
  PBAD <- trBAD %*% p0
  
  
  mr <- as.vector(log(2 - p1 / PBAD))
  
  fn <- function(x, BAD, p1, p0) {
    trMBAD <- expm::expm(diag(x) + BAD)
    PMBAD <- trMBAD %*% p0
    mean((PMBAD - p1)^2)
  }
  
  sol <- optim_fn(mr, fn, method = "L-BFGS-B", lower = -1, upper = 1, p1 = p1, p0 = p0, BAD = BAD)
  mr <- sol$par
  
  trMBAD <- expm::expm(diag(mr) + BAD)
  PMBAD <- trMBAD %*% p0
  
  return(list(
    MigR = mr,
    P_hat = as.vector(PMBAD),
    MSE = mean((PMBAD - p1)^2)
  ))
}


calc_migration_age_sex <- function(p0, p1, fer, dr, ageing, n_agp, bir_female, optim_fn) {
  p0 <- matrix(p0, n_agp * 2, 1)
  p1 <- matrix(p1, n_agp * 2, 1)
  dr <- c(dr)
  
  BAD <- diag(- dr - rep(ageing, 2))
  BAD[1, 1:n_agp] <- BAD[1, 1:n_agp] + fer * bir_female
  BAD[n_agp + 1, 1:n_agp] <- BAD[n_agp + 1, 1:n_agp] + fer * (1 - bir_female)
  
  BAD[2:n_agp, 1:(n_agp - 1)] <- BAD[2:n_agp, 1:(n_agp - 1)] + diag(ageing[1:(n_agp - 1)])
  BAD[2:n_agp + n_agp, 1:(n_agp - 1) + n_agp] <- BAD[2:n_agp + n_agp, 1:(n_agp - 1) + n_agp] + diag(ageing[1:(n_agp - 1)])
  
  trBAD <- expm::expm(BAD)
  PBAD <- trBAD %*% p0
  
  
  mr <- as.vector(log(2 - p1 / PBAD))
  
  fn <- function(x, BAD, p1, p0) {
    trMBAD <- expm::expm(diag(x) + BAD)
    PMBAD <- trMBAD %*% p0
    mean((PMBAD - p1)^2)
  }
  
  sol <- optim_fn(mr, fn, method = "L-BFGS-B", lower = -2, upper = 2, p1 = p1, p0 = p0, BAD = BAD)
  mr <- sol$par
  
  trMBAD <- expm::expm(diag(mr) + BAD)
  PMBAD <- trMBAD %*% p0
  
  return(list(
    MigR = matrix(mr, n_agp, 2),
    P_hat = as.vector(PMBAD),
    MSE = mean((PMBAD - p1)^2)
  ))
}



calc_migration_agg <- function(p0, p1, br, dr) {
  mr <- log(p1/p0) - br + dr
  p_hat <- p0 * exp(mr + br - dr)
  
  return(list(
    MigR = mr,
    P_hat = p_hat,
    MSE = p_hat - p1
  ))
}