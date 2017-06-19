#' gas_prod_data
#'
#' Process gas production query
#' @param query GCAM query containing gas production output data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords gas production
#' @import dplyr tidyr
#' @export
#' @examples
#' gas_prod_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

gas_prod_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  tech_order <- c("coal gasification", "natural gas", "biomass gasification")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  GP <- readr::read_csv(paste0(query_dir,query), skip = 1) %>%
    select(-X27) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    mutate(technology = factor(technology, levels = tech_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(GP, "query") <- "Gas Production"
  attr(GP, "colors") <- GAS_PROC_COLORS
  attr(GP, "fill") <- "technology"
  return(GP)
}
