#' diff_processer
#'
#' Returns a list of processed data for all queries that have processing code, showing difference between scenarios and a reference scenario
#' @param folder Directory with GCAM queries only
#' @param scenarios Vector of scenarios to include in data
#' @param diff_scenario Scenario to diff all other scenarios by. Must be length 1
#' @import dplyr tidyr
#' @export

diff_processer <- function(folder = QUERY_FOLDER, scenarios = SCENARIOS, diff_scenario = DIFF_SCENARIO){
  stopifnot(length(diff_scenario) == 1)
  data_output <- list()
  if (substr(folder, nchar(folder), nchar(folder)) != "/"){
    folder <- paste0(folder, "/")
  }
  queries <- query_id(folder)
  for (i in 1:dim(queries)[1]){
    if (queries$title[i] %in% query_function_map){
      fun_name <- which(query_function_map == queries$title[i]) %>% names
      for (name in fun_name){
        fun <- get(name)
        object_name <- name %>% stringr::str_replace("_data", "")
        scenarios_df <- fun(queries$file[i], scenarios, folder) %>% rename(scen_value = value)
        attr_data <- attributes(scenarios_df)
        diff_scenario_df <- fun(queries$file[i], diff_scenario, folder) %>%
          rename(diff_scenario = scenario, diff_value = value)
        data_output[[object_name]] <- scenarios_df %>%
          left_join(diff_scenario_df, by = c("region", attributes(scenarios_df)$fill, "Units", "year")) %>%
          mutate(value = scen_value - diff_value)
        attr(data_output[[object_name]], "query") <- attr_data$query
        attr(data_output[[object_name]], "colors") <- attr_data$colors
        attr(data_output[[object_name]], "fill") <- attr_data$fill
      }
    }
  }
  return(data_output)
}
