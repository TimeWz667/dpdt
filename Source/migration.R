library(Matrix)


calculate_mr_five <- function(p0, p1, dr, br, adj=T) {
  n_age <- length(dr)
  
  P0 <- c(p0, 0)
  P1 <- c(p1, 0)
  
  BAD <- matrix(0, n_age + 1, n_age + 1)
  BAD[1:n_age + 1, 1:n_age] <- diag(1/5, n_age)
  BAD[1, 2:n_age] <- br
  BAD[n_age + 1, 2:n_age] <- dr[2:n_age] - br
  BAD[n_age + 1, 1] <- dr[1]
  BAD <- BAD - diag(colSums(BAD), n_age + 1)
  
  trBAD <- expm(BAD)
  PBAD <- trBAD %*% P0
  PBAD <- PBAD[1:n_age]
  
  mp <- (PBAD - p1)/PBAD
  mr <- -log(1-mp)
  
  
  if (adj) {
    M <- matrix(0, n_age + 1, n_age + 1)
    
    fn <- function(x) {
      m1 <- M
      m1[n_age + 1, 1:n_age] <- x
      m1 <- m1 - diag(colSums(m1), n_age + 1)
      trMBAD <- expm(m1 + BAD)
      
      PMBAD <- trMBAD %*% P0
      sum((PMBAD[1:n_age] - p1)^2)
    }
    
    sol <- optim(mr, fn, method=c("L-BFGS-B"), lower=-1, upper=.1)
    mr <- sol$par
  }
  
  M <- matrix(0, n_age + 1, n_age + 1)
  M[n_age + 1, 1:n_age] <- mr
  M <- M - diag(colSums(M), n_age + 1)
  
  trMBAD <- expm(M + BAD)
  
  PMBAD <- trMBAD %*% P0
  PMBAD <- PMBAD[1:n_age]
  
  return(list(
    MigR=-mr,
    P_hat=PMBAD,
    Error=sum(PMBAD-p1)
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