#' read_query
#'
#' Read in GCAM queries
#' @import dplyr tidyr

read_query <- function(...){
  suppressMessages(readr::read_csv(...))
}
