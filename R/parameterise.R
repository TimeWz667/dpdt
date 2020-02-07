#' Title
#'
#' @param sim 
#' @param pkg 
#'
#' @return
#' @export
#'
#' @examples
as_pars_all <- function(sim, pkg = c("odin", "deSolve")) {
  pkg <- match.arg(pkg)
  
  if (pkg == "odin") {
    pars <- list(
      tt = sim$Year,
      br = sim$BirR,
      mr = sim$MigR,
      dr = sim$DeaR,
      N0 = sim$PopN[1]
    )
  } else {
    # todo
  }
  
  return(pars)
}


#' @rdname as_pars_all
#' @export
as_pars_sex <- function(sim, pkg = c("odin", "deSolve")) {
  pkg <- match.arg(pkg)
  
  if (pkg == "odin") {
    pars <- list(
      tt = sim$Year,
      br_f = sim$BirR_F,
      mr_f = sim$MigR_F,
      dr_f = sim$DeaR_F,
      F0 = sim$PopN_F[1],
      br_m = sim$BirR_M,
      mr_m = sim$MigR_M,
      dr_m = sim$DeaR_M,
      M0 = sim$PopN_M[1]
    )
  } else {
    # todo
  }
  
  return(pars)
}