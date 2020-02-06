#' Title
#'
#' @param dat 
#' @param year0 
#' @param year1 
#' @param agp 
#' @param agl 
#' @param sex "F" female; "M" male; "T" total
#' @param ageing_to_dead TRUE if the ageing in the final age group will bring death; otherwise no ageing for them.
#' @param k 
#' @param sex_ratio sex ratio at birth, 107 for the global average
#' @param bind bind the result table (data.table) or not (list)
#' @param multi_task cores for parallel computation, FALSE for single tread.
#'
#' @return
#' @import data.table
#' @export
#'
#' @examples
#' demo <- fetch_demography()
#' agp = c(rep(1:16, each=5), rep(16, 21))
#' agl = c(paste(seq(0, 70, 5), seq(4, 74, 5), sep = "-"), "75+")
#' res = as_sim_age(dat, 2000, 2005, agp, agl, sex = "T", k = 1, bind = T)
#' 
as_sim_age <- function(dat, year0, year1, agp, agl, sex = c("T", "F", "M"), 
                       ageing_to_dead = FALSE, k = 1, bind = FALSE, multi_task = FALSE) {

  year0 <- max(year0, dat$Range[1])
  year1 <- min(year1, dat$Range[2])
  years <- year0:year1
  stopifnot(year0 < year1)
  
  n_year <- length(years)
  n_agp <- length(agl)
  
  stopifnot(agp == sort(agp))
  stopifnot(length(unique(agp)) == n_agp)
  
  age_span <- as.numeric(table(agp))
  ageing <- 1 / age_span
  
  
  if (!ageing_to_dead) {
    ageing[length(ageing)] <- 0
  }
  
  sex <- match.arg(sex)
  
  if (multi_task) {
    stopifnot(is.numeric(multi_task))
    cl <- optimParallel::makeCluster(multi_task)
    optimParallel::setDefaultCluster(cl=cl) 
    optim_fn <- optimParallel::optimParallel
  } else {
    optim_fn <- stats::optim 
  }
  
  res.brs <- rep(0, n_year)
  res.ps <- matrix(0, n_year + 1, n_agp)
  res.mrs <- matrix(0, n_year, n_agp)
  res.drs <- matrix(0, n_year, n_agp)
  res.mses <- rep(0, n_year)
  
  
  bns <- dat$BirthN[, .(Time, Brs = Births * k)]
  ps <- dat[[paste0("PopN_", sex)]]
  drs <- dat[[paste0("DeaR_", sex)]]
  
  
  for (yr0 in year0:year1) {
    yr1 <- yr0 + 1
    
    index <- yr0 - year0 + 1
    
    bn <- bns[Time == yr0, Brs]
    
    p0 <- as.numeric(ps[Time == yr0, -1])
    p1 <- as.numeric(ps[Time == yr1, -1])
    dr <- as.numeric(drs[Time == yr0, -1])
    
    res.drs[index, ] <- dr <- tapply(dr * p0, agp, sum) / tapply(p0, agp, sum)
    res.ps[index, ] <- p0 <- tapply(p0, agp, sum)
    p1 <- tapply(p1, agp, sum)
    
    res.brs[index] <- br <- bn / sum(p0)
    res.ps[index, ] <- p0
    
    mig <- calc_migration_agp(p0, p1, br, dr, ageing, n_agp, optim_fn)
    res.mrs[index, ] <- mig$MigR
    res.mses[index] <- mig$MSE
  }
  res.ps[index + 1, ] <- p1
  

  if (bind) {
    colnames(res.drs) <- paste0("DeaR [", agl, "]")
    colnames(res.mrs) <- paste0("MigR [", agl, "]")
    colnames(res.ps) <- paste0("PopN [", agl, "]")
    
    res <- cbind(
      Year = years,
      BirR = res.brs,
      DeaR = res.drs,
      MigR = res.mrs,
      PopN = res.ps[1:n_year, ],
      MSE = res.mses
    )
    res <- data.table::data.table(res)
    
  } else {
    names(res.brs) <- names(res.mses) <- years
    dimnames(res.drs) <- dimnames(res.mrs) <- list(years, agl)
    dimnames(res.ps) <- list(year0:(year1 + 1), agl)
    
    res <- list(
      Year = years,
      Labels = agl,
      Ageing = ageing,
      BirR = res.brs,
      DeaR = res.drs,
      MigR = res.mrs,
      PopN = res.ps,
      MSE = res.mses
    )
    
    class(res) <- "pop_agp"
  }
  res
}


#' @rdname as_sim_age
#' @export
as_sim_age_sex <- function(dat, year0, year1, agp, agl, sex_ratio = 107,  ...) {
  
  prop_f <- 100 / (100 + sex_ratio)
  prop_m <- 1 - prop_f
  
  res_female <- as_sim_age(dat, year0, year1, agp, agl, sex = "F", k = prop_f, bind = F, ...)
  res_male <- as_sim_age(dat, year0, year1, agp, agl, sex = "M", k = prop_m, bind = F, ...)
  
  res <- c(res_female, res_male[-(1:3)])
  
  names(res)[3:8] <- paste0(names(res)[3:8], "_F")
  names(res)[9:13] <- paste0(names(res)[9:13], "_M")
  
  class(res) <- "pop_agp_sex"
  return(res)
}


#' @rdname as_sim_age
#' @import data.table
#' @export
as_sim_all <- function(dat, year0, year1, sex = c("T", "F", "M"), k = 1) {
  
  year0 <- max(year0, dat$Range[1])
  year1 <- min(year1, dat$Range[2])
  years <- year0:year1
  stopifnot(year0 < year1)
  
  n_year <- length(years)
  
  sex <- match.arg(sex)
  
  res.brs <- rep(0, n_year)
  res.ps <- rep(0, n_year + 1)
  res.mrs <- rep(0, n_year)
  res.drs <- rep(0, n_year)
  res.mses <- rep(0, n_year)
  
  
  bns <- dat$BirthN[, .(Time, Brs = Births * k)]
  ps <- dat[[paste0("PopN_", sex)]]
  drs <- dat[[paste0("DeaR_", sex)]]
  
  
  for (yr0 in year0:year1) {
    yr1 <- yr0 + 1
    
    index <- yr0 - year0 + 1
    
    bn <- bns[Time == yr0, Brs]
    
    p0 <- as.numeric(ps[Time == yr0, -1])
    p1 <- as.numeric(ps[Time == yr1, -1])
    dr <- as.numeric(drs[Time == yr0, -1])
    
    res.drs[index] <- dr <- sum(dr * p0) / sum(p0)
    res.ps[index] <- p0 <- sum(p0)
    p1 <- sum(p1)
    
    res.brs[index] <- br <- bn / sum(p0)
    res.ps[index] <- p0
    
    mig <- calc_migration_agg(p0, p1, br, dr)
    res.mrs[index] <- mig$MigR
    res.mses[index] <- mig$MSE
  }
  res.ps[index + 1] <- p1
  
  res <- cbind(
    Year = years,
    BirR = res.brs,
    DeaR = res.drs,
    MigR = res.mrs,
    PopN = res.ps[1:n_year],
    MSE = res.mses
  )
  res <- data.table::data.table(res)

  return(res)
}


#' @rdname as_sim_age
#' @export
as_sim_sex <- function(dat, year0, year1, sex_ratio = 107) {
  
  prop_f <- 100 / (100 + sex_ratio)
  prop_m <- 1 - prop_f
  
  res_female <- as_sim_all(dat, year0, year1, sex = "F", k = prop_f)
  res_male <- as_sim_all(dat, year0, year1, sex = "M", k = prop_m)
  
  names(res_female)[2:6] <- paste0(names(res_female)[2:6], "_F")
  names(res_male)[2:6] <- paste0(names(res_male)[2:6], "_M")
  
  res <- cbind(res_female, res_male[, -1])
  
  res <- data.table::data.table(res)
  
  return(res)
}
