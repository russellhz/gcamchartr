#' elec_gen_data
#'
#' Process electricity generation query
#' @param query GCAM query containing primary energy data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords electricity generation
#' @import dplyr tidyr
#' @export
#' @examples
#' elec_gen_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

elec_gen_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  tech_order <- c("Oil", "Oil w/ CCS", "Gas", "Gas w/ CCS", "Coal", "Coal w/ CCS",
                  "Biomass", "Biomass w/ CCS", "Nuclear",
                  "Hydro", "Solar", "Wind", "Geothermal", "CHP")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  EG <- readr::read_csv(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    mutate(technology = if_else(substr(technology,2,2) == " ",
                                substr(technology, 3, stringr::str_length(technology)), technology), technology) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    mutate(technology = factor(technology, levels = tech_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(EG, "query") <- query_title
  attr(EG, "colors") <- ELEC_GEN_COLORS
  attr(EG, "fill") <- "technology"
  return(EG)
}
