#' Fetch data from a github repo
#'
#' @param folder folder of data within a repo
#' @param repo name of the repo
#' @param branch selected branch
#'
#' @return a 'pop_raw' object
#' @export
#'
#' @examples
#' demo <- fetch_demography(folder = "ByCountry/United Kingdom")
#' print(demo)
#' 
fetch_demography <- function(folder = "ByCountry/United Kingdom", 
                             repo = "TimeWz667/pop4modelling", 
                             branch = "master") {
  require(data.table)

  in_folder <- paste("https://raw.githubusercontent.com", repo, branch, folder, sep = "/")
  in_folder <- gsub(" ", "%20", in_folder)
  
  res <- list(
    BirthN = data.table::fread(paste0(in_folder, "/Births.csv"), key = "Time"),
    DeaR_F = data.table::fread(paste0(in_folder, "/DeaF.csv")),
    DeaR_M = data.table::fread(paste0(in_folder, "/DeaM.csv")),
    DeaR_T = data.table::fread(paste0(in_folder, "/DeaT.csv")),
    PopN_F = data.table::fread(paste0(in_folder, "/PopF.csv")),
    PopN_M = data.table::fread(paste0(in_folder, "/PopM.csv")),
    PopN_T = data.table::fread(paste0(in_folder, "/PopT.csv"))
  )
  
  res$Range <- range(res$BirthN$Time)
  res$N_Age <- dim(res$DeaR_F)[2] - 1
  res$Setting <- folder
  
  class(res) <- "pop_raw"
  return(res)
}

#' @rdname fetch_demography
#' @export
print.pop_raw <- function(obj) {
  cat("Raw population dynamic data\n")
  cat("Setting: ", obj$Setting, "\n")
  cat("Year: [", obj$Range[1], ", ", obj$Range[2], "]\n")
  cat("Age: [ 0, ", obj$N_Age - 1, "]\n")
}
