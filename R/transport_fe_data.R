#' transport_fe_data
#'
#' Process transport final energy query
#' @param query GCAM query containing transport final energy output data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords transport final energy
#' @import dplyr tidyr
#' @export
#' @examples
#' transport_fe_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

transport_fe_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  transport_fe_lookup <- read_query(system.file("extdata", "transport_fe_lookup.csv", package = "gcamchartr"))

  fuel_order <- c("Refined Liquids", "Natural Gas (delivered)", "Coal (delivered)",
                  "Electricity", "Hydrogen")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  TFE <- read_query(paste0(query_dir, query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    left_join(transport_fe_lookup, by = "input") %>%
    group_by(scenario, region, Units, year, fuel) %>%
    summarise(value = sum(value)) %>%
    ungroup %>%
    mutate(fuel = factor(fuel, levels = fuel_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(TFE, "query") <- query_title
  attr(TFE, "colors") <- FINAL_ENERGY_COLORS
  attr(TFE, "fill") <- "fuel"
  attr(TFE, "default_plot") <- "bar"


  return(TFE)
}
