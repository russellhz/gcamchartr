#' refined_liquids_data
#'
#' Process refined liquids query
#' @param query GCAM query containing refined liquids data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords refined liquids
#' @import dplyr tidyr
#' @export
#' @examples
#' refined_liquids_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))


refined_liquids_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  refined_liquids_lookup <- readr::read_csv(system.file("extdata", "refined_liquids_lookup.csv", package = "gcamchartr"))

  fuel_order <- c("Oil", "Natural Gas", "Coal", "Coal CCS", "Biomass", "Biomass CCS")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  RL <- readr::read_csv(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    left_join(refined_liquids_lookup, by = "technology") %>%
    group_by(scenario, region, Units, year, fuel) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(fuel = factor(fuel, levels = fuel_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(RL, "query") <- query_title
  attr(RL, "colors") <- REFINED_LIQUIDS_COLORS
  attr(RL, "fill") <- "fuel"
  return(RL)
}
