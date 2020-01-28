rm(list=ls())

source("Source/migration.R")
load(file="Input/DataByCountry.rdata")


### Assumptions

agegp <- c(rep(1:16, each=5), rep(16, 21))
srb <- 107

### Initialisation

sim_agp_sex <- list()
sim_agp <- list()
sim_sex <- list()
sim_all <- list()

bpm <- srb/(1 + srb)
bpf <- 100/(1 + srb)

n_agegp <- 16
n_yr <- 100



for (country in names(dat_full)) {
  dat <- dat_full[[country]]
  
  ## With Age groups
  drs_f <- matrix(0, n_yr - 1, n_agegp)
  drs_m <- matrix(0, n_yr - 1, n_agegp)
  drs_t <- matrix(0, n_yr - 1, n_agegp)
  
  mrs_f <- matrix(0, n_yr - 1, n_agegp)
  mrs_m <- matrix(0, n_yr - 1, n_agegp)
  mrs_t <- matrix(0, n_yr - 1, n_agegp)
  
  brs_f <- rep(0, n_yr - 1)
  brs_m <- rep(0, n_yr - 1)
  brs_t <- rep(0, n_yr - 1)
  
  ps_f <- matrix(0, n_yr - 1, n_agegp)
  ps_m <- matrix(0, n_yr - 1, n_agegp)
  ps_t <- matrix(0, n_yr - 1, n_agegp)
  
  
  for (yr in 1:(n_yr - 1)) {
  
    bn <- sum(dat$BirR[yr, ] * dat$Pop_F[yr, 16:50], na.rm=T)
    
    #### Total
    p0 <- dat$Pop_T[yr, ]
    p1 <- dat$Pop_T[yr + 1, ]
    dr <- dat$DeaR_T[yr, ]
    
    drs_t[yr, ] <- dr <- tapply(dr*p0, agegp, sum) / tapply(p0, agegp, sum)
    p0 <- tapply(p0, agegp, sum)
    p1 <- tapply(p1, agegp, sum)
    
    brs_t[yr] <- br <- bn / sum(p0, na.rm=T)
    mrs_t[yr, ] <- calculate_mr_five(p0, p1, dr, br, adj=T)$MigR
    ps_t[yr, ] <- p0
    
    
    #### Female
    p0 <- dat$Pop_F[yr, ]
    p1 <- dat$Pop_F[yr + 1, ]
    dr <- dat$DeaR_F[yr, ]
    
    drs_f[yr, ] <- dr <- tapply(dr*p0, agegp, sum) / tapply(p0, agegp, sum)
    p0 <- tapply(p0, agegp, sum)
    p1 <- tapply(p1, agegp, sum)
    
    brs_f[yr] <- br <- bn * bpf / sum(p0, na.rm=T)
    mrs_f[yr, ] <- calculate_mr_five(p0, p1, dr, br, adj=T)$MigR
    ps_f[yr, ] <- p0
    
    #### Male
    p0 <- dat$Pop_M[yr, ]
    p1 <- dat$Pop_M[yr + 1, ]
    dr <- dat$DeaR_M[yr, ]
    
    drs_m[yr, ] <- dr <- tapply(dr*p0, agegp, sum) / tapply(p0, agegp, sum)
    p0 <- tapply(p0, agegp, sum)
    p1 <- tapply(p1, agegp, sum)
    
    brs_m[yr] <- br <- bn * bpm / sum(p0, na.rm=T)
    mrs_m[yr, ] <- calculate_mr_five(p0, p1, dr, br, adj=T)$MigR
    ps_m[yr, ] <- p0
    
  }
  
  
  sim_agp_sex[[country]] <- list(
    PopN_F=ps_f, PopN_M=ps_m, 
    BirR_F=brs_f, BirR_M=brs_m,
    DeaR_F=drs_f, DeaR_M=drs_f,
    MigR_F=mrs_f, MigR_M=mrs_m
  )
  
  sim_agp[[country]] <- list(
    PopN_T=ps_t, BirR_T=brs_t, DeaR_T=drs_t, MigR_T=mrs_t
  )
  

  ## Without age groups
  drs_f <- rep(0, n_yr - 1)
  drs_m <- rep(0, n_yr - 1)
  drs_t <- rep(0, n_yr - 1)
  
  mrs_f <- rep(0, n_yr - 1)
  mrs_m <- rep(0, n_yr - 1)
  mrs_t <- rep(0, n_yr - 1)
  
  brs_f <- rep(0, n_yr - 1)
  brs_m <- rep(0, n_yr - 1)
  brs_t <- rep(0, n_yr - 1)
  
  ps_f <- rep(0, n_yr - 1)
  ps_m <- rep(0, n_yr - 1)
  ps_t <- rep(0, n_yr - 1)
  
  
  for (yr in 1:(n_yr - 1)) {
    bn <- sum(dat$BirR[yr, ] * dat$Pop_F[yr, 16:50], na.rm=T)
    
    #### Total
    p0 <- dat$Pop_T[yr, ]
    p1 <- sum(dat$Pop_T[yr + 1, ])
    dr <- dat$DeaR_T[yr, ]
    
    drs_t[yr] <- dr <- sum(dr*p0) / sum(p0)
    p0 <- sum(p0)
    
    brs_t[yr] <- br <- bn / p0
    mrs_t[yr] <- calculate_mr_agg(p0, p1, dr, br)$MigR
    ps_t[yr] <- p0
    
    
    #### Female
    p0 <- dat$Pop_F[yr, ]
    p1 <- sum(dat$Pop_F[yr + 1, ])
    dr <- dat$DeaR_F[yr, ]
    
    drs_f[yr] <- dr <- sum(dr*p0) / sum(p0)
    p0 <- sum(p0)
    
    brs_f[yr] <- br <- bn * bpf / p0
    mrs_f[yr] <- calculate_mr_agg(p0, p1, dr, br)$MigR
    ps_f[yr] <- p0
    
    
    #### Male
    p0 <- dat$Pop_M[yr, ]
    p1 <- sum(dat$Pop_M[yr + 1, ])
    dr <- dat$DeaR_M[yr, ]
    
    drs_m[yr] <- dr <- sum(dr*p0) / sum(p0)
    p0 <- sum(p0)
    
    brs_m[yr] <- br <- bn * bpm / p0
    mrs_m[yr] <- calculate_mr_agg(p0, p1, dr, br)$MigR
    ps_m[yr] <- p0
  }


  sim_sex[[country]] <- data.frame(
    PopN_F=ps_f, PopN_M=ps_m, 
    BirR_F=brs_f, BirR_M=brs_m, 
    DeaR_F=drs_f, DeaR_M=drs_f, 
    MigR_F=mrs_f, MigR_M=mrs_m
  )
  
  
  sim_all[[country]] <- data.frame(PopN_T=ps_t, BirR_T=brs_t, DeaR_T=drs_t, MigR_T=mrs_t)

  cat(country, "--> completed\n")
}


save(sim_agp_sex, sim_agp, sim_sex, sim_all, file="Output/SimDemo.rdata")

