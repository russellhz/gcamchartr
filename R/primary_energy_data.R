#' primary_energy_data
#'
#' Process primary energy query
#' @param query GCAM query containing primary energy data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords primary energy
#' @import dplyr tidyr
#' @export
#' @examples
#' primary_energy_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

primary_energy_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  fuel_order <- c("Oil", "Oil CCS", "Natural Gas", "Natural Gas CCS", "Coal", "Coal CCS",
                  "Biomass", "Biomass CCS", "Regional Corn For Ethanol CCS", "Nuclear",
                  "Hydro", "Solar", "Wind", "Geothermal", "Traditional Biomass")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  PE <- read_query(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios,
           !grepl("bio-ceiling|bio-floor", fuel)) %>%
    mutate(fuel = if_else(substr(fuel,2,2) == " ", stringr::str_to_title(substr(fuel, 3, stringr::str_length(fuel))),
                          stringr::str_to_title(fuel), fuel)) %>%
    mutate(fuel = stringr::str_replace(fuel, "Ccs","CCS")) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    mutate(fuel = if_else(fuel == "Regional Corn For Ethanol CCS", "Biomass CCS", fuel)) %>%
    group_by(scenario, region, fuel, year, Units) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(fuel = factor(fuel, levels = fuel_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(PE, "query") <- query_title
  attr(PE, "colors") <- PRIMARY_ENERGY_COLORS
  attr(PE, "fill") <- "fuel"
  attr(PE, "default_plot") <- "bar"

  return(PE)
}
