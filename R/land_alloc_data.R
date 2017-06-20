#' land_alloc_data
#'
#' Process land allocation query
#' @param query GCAM query containing land allocation data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords land allocation
#' @import dplyr tidyr
#' @export
#' @examples
#' land_alloc_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

land_alloc_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  land_order <- c("pasture (other)", "desert", "grass", "shrubs", "crops", "pasture (grazed)",
                  "forest (managed)","forest (unmanaged)", "biomass", "otherarable",
                  "urban", "tundra")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  LA <- readr::read_csv(paste0(query_dir,query), skip = 3) %>%
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
  attr(LA, "query") <- query_title
  attr(LA, "colors") <- ELEC_GEN_COLORS
  attr(LA, "fill") <- "technology"
  return(LA)
}
