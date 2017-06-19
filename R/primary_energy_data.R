#' primary_energy_data
#'
#' Process primary energy query
#' @param query GCAM query containing primary energy data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords primary energy
#' @import tidyverse stringr
#' @export
#' @examples
#' primary_energy_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

primary_energy_data <- function(query, scenarios){
  fuel_order <- c("oil", "oil CCS", "natural gas", "natural gas CCS", "coal", "coal CCS",
                  "biomass", "biomass CCS", "regional corn for ethanol CCS", "nuclear",
                  "hydro", "solar", "wind", "geothermal", "traditional biomass")

  query_title <- query_id(QUERY_FOLDER) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  PE <- read_csv(paste0(QUERY_FOLDER,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios,
           !grepl("bio-ceiling", fuel)) %>%
    mutate(fuel = if_else(substr(fuel,2,2) == " ", substr(fuel, 3, str_length(fuel)), fuel), fuel) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    mutate(fuel = factor(fuel, levels = fuel_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(PE, "query") <- query_title
  attr(PE, "colors") <- PRIMARY_ENERGY_COLORS
  attr(PE, "fill") <- "fuel"
  return(PE)
}
