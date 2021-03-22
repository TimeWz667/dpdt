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
    FerR = data.table::fread(paste0(in_folder, "/FertSpline.csv")),
    # BirthN = data.table::fread(paste0(in_folder, "/Births.csv"), key = "Time"),
    DeaR_F = data.table::fread(paste0(in_folder, "/DeaSplineF.csv")),
    DeaR_M = data.table::fread(paste0(in_folder, "/DeaSplineM.csv")),
    # DeaR_T = data.table::fread(paste0(in_folder, "/DeaT.csv")),
    PopN_F = data.table::fread(paste0(in_folder, "/PopF.csv")),
    PopN_M = data.table::fread(paste0(in_folder, "/PopM.csv"))
  )
  
  res$Range <- range(res$FerR$Time)
  res$N_Age <- ncol(res$DeaR_F) - 1
  res$Setting <- folder
  
  class(res) <- "pop_raw"
  return(res)
}


load_demography <- function(folder) {
  require(data.table)
  # todo
}


#' @rdname fetch_demography
#' @export
print.pop_raw <- function(obj) {
  cat("Raw population dynamic data\n")
  cat("Setting: ", obj$Setting, "\n")
  cat("Year: [", obj$Range[1], ", ", obj$Range[2], "]\n")
  cat("Age: [ 0, ", obj$N_Age - 1, "]\n")
}
