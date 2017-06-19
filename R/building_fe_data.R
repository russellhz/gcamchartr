#' building_fe_data
#'
#' Process building final energy query
#' @param query GCAM query containing building final energy output data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords building final energy
#' @import tidyverse stringr
#' @export
#' @examples
#' building_fe_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

building_fe_data <- function(query, scenarios, diff = NULL){
  building_fe_lookup <- read_csv(paste0(DIR,"assumptions/building_fe_lookup.csv"))

  fuel_order <- c("Refined Liquids", "Natural Gas (delivered)", "Coal (delivered)", "Biomass (delivered)",
                  "Electricity", "Traditional Biomass", "District Heat")

  query_title <- query_id(QUERY_FOLDER) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  BFE <- read_csv(paste0(QUERY_FOLDER,query), skip = 1) %>%
    select(-X27) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    left_join(building_fe_lookup, by = "input") %>%
    group_by(scenario, region, Units, year, fuel) %>%
    summarise(value = sum(value)) %>%
    ungroup %>%
    mutate(fuel = factor(fuel, levels = fuel_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(BFE, "query") <- "Building Final Energy"
  attr(BFE, "colors") <- BUILDING_FE_COLORS
  attr(BFE, "fill") <- "fuel"

  return(BFE)
}
