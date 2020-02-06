calc_migration_agp <- function(p0, p1, br, dr, ageing, n_agp, optim_fn) {
  p0 <- matrix(p0, n_agp, 1)
  p1 <- matrix(p1, n_agp, 1)
  
  BAD <- diag(-dr - ageing)
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