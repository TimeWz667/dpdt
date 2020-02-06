calculate_mr_five <- function(p0, p1, dr, br, ageing_to_dead=F) {
  n_age <- length(dr)
  
  p0 <- matrix(p0, n_age, 1)
  p1 <- matrix(p1, n_age, 1)
  
  BAD <- diag(- dr - 1/5)
  BAD[1, ] <- BAD[1, ] + br
  # BAD[1:(n_age - 1), 1:(n_age - 1)] <- BAD[1:(n_age - 1), 1:(n_age - 1)] + diag(-1/5, n_age - 1)
  BAD[2:n_age, 1:(n_age - 1)] <- BAD[2:n_age, 1:(n_age - 1)] + diag(1/5, n_age - 1)
  
  trBAD <- expm(BAD)
  PBAD <- trBAD %*% p0

  mp <- (p1 - PBAD)/PBAD
  mr <- as.vector(-log(1-mp))
  
  
  if (adj) {
    fn <- function(x, BAD, p1, p0) {
      trMBAD <- expm::expm(diag(x) + BAD)
      PMBAD <- trMBAD %*% p0
      sum((PMBAD - p1)^2)
    }
    
    sol <- optimParallel(mr, fn, method = "L-BFGS-B", lower = -1, upper = 1, p1 = p1, p0 = p0, BAD = BAD)
    mr <- sol$par
  }
  
  trMBAD <- expm::expm(diag(mr) + BAD)
  PMBAD <- trMBAD %*% p0

  return(list(
    MigR = mr,
    P_hat = as.vector(PMBAD),
    Error = sum((PMBAD - p1)^2)
  ))
}


calculate_mr_agg <- function(p0, p1, dr, br) {
  mr <- log(p1/p0) - br + dr
  p_hat <- p0 * exp(mr + br - dr)
  
  return(list(
    MigR=mr,
    P_hat=p_hat,
    Error=p_hat - p1
  ))
}