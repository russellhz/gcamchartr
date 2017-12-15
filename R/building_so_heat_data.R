#' building_so_heat_data
#'
#' Process building service output query for only heat
#' @param query GCAM query containing building final energy output data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords building final energy
#' @import dplyr tidyr
#' @export
#' @examples
#' building_so_heat_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

building_so_heat_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  fuel_order <- c("Refined Liquids", "Gas", "Coal", "Biomass",
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
           grepl('heating', sector)) %>%
    group_by(scenario, region, Units, year, subsector) %>%
    summarise(value = sum(value)) %>%
    ungroup %>%
    mutate(fuel = stringr::str_to_title(subsector),
           fuel = factor(fuel, levels = fuel_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(BHE, "query") <- "Building Heating Service Output"
  attr(BHE, "colors") <- SERVICE_OUTPUT_COLORS
  attr(BHE, "fill") <- "fuel"
  attr(BHE, "default_plot") <- "bar"

  return(BHE)
}

