#' data_processer
#'
#' Returns a list of processed data for all queries that have processing code
#' @param folder Directory with GCAM queries only
#' @param scenarios Vector of scenarios to include in data
#' @import dplyr tidyr
#' @export

data_processer <- function(folder = QUERY_FOLDER, scenarios = SCENARIOS){
  data_output <- list()
  queries <- query_id(folder)
  for (i in 1:dim(queries)[1]){
    if (queries$title[i] %in% query_function_map){
      fun_name <- which(query_function_map == queries$title[i]) %>% names
      for (name in fun_name){
        fun <- get(name)
        object_name <- name %>% stringr::str_replace("_data", "")
        data_output[[object_name]] <- fun(queries$file[i], scenarios)
      }
    }
  }
  return(data_output)
}
