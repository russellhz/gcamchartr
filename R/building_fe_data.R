#' building_fe_data
#'
#' Process building final energy query
#' @param query GCAM query containing building final energy output data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords building final energy
#' @import dplyr tidyr
#' @export
#' @examples
#' building_fe_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

building_fe_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  building_fe_lookup <-  suppressMessages(readr::read_csv(system.file("extdata", "building_fe_lookup.csv", package = "gcamchartr")))

  fuel_order <- c("Refined Liquids", "Natural Gas (delivered)", "Coal (delivered)", "Biomass (delivered)",
                  "Electricity", "Traditional Biomass", "District Heat")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  BFE <- read_query(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010,
           Units != 'EJ_or_Share') %>%
    left_join(building_fe_lookup, by = "input") %>%
    group_by(scenario, region, Units, year, fuel) %>%
    summarise(value = sum(value)) %>%
    ungroup %>%
    mutate(fuel = factor(fuel, levels = fuel_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(BFE, "query") <- "Building Final Energy"
  attr(BFE, "colors") <- FINAL_ENERGY_COLORS
  attr(BFE, "fill") <- "fuel"
  attr(BFE, "default_plot") <- "bar"

  return(BFE)
}

#' building_heating_data
#'
#' Process building final energy for query
#' @param query GCAM query containing building final energy output data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords building final energy
#' @import dplyr tidyr
#' @export
#' @examples
#' building_heating_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

building_heating_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  building_fe_lookup <-  suppressMessages(readr::read_csv(system.file("extdata", "building_fe_lookup.csv", package = "gcamchartr")))

  fuel_order <- c("Refined Liquids", "Natural Gas (delivered)", "Coal (delivered)", "Biomass (delivered)",
                  "Electricity", "Traditional Biomass", "District Heat")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  BHE <- read_query(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010,
           grepl("heating", sector),
           Units != 'EJ_or_Share') %>%
    left_join(building_fe_lookup, by = "input") %>%
    group_by(scenario, region, Units, year, fuel) %>%
    summarise(value = sum(value)) %>%
    ungroup %>%
    mutate(fuel = factor(fuel, levels = fuel_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(BHE, "query") <- "Building Final Energy for Heating"
  attr(BHE, "colors") <- FINAL_ENERGY_COLORS
  attr(BHE, "fill") <- "fuel"
  attr(BHE, "default_plot") <- "bar"

  return(BHE)
}
