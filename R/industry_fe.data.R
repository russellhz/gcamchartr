#' industry_fe_data
#'
#' Process industry final energy query
#' @param query GCAM query containing industry final energy output data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords industry final energy
#' @import dplyr tidyr
#' @export
#' @examples
#' industry_fe_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

industry_fe_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  industry_fe_lookup <- readr::read_csv(system.file("extdata", "industry_fe_lookup.csv", package = "gcamchartr"))

  fuel_order <- c("Refined Liquids", "Natural Gas (wholesale)", "Coal (delivered)", "Biomass (delivered)",
                  "Electricity", "Hydrogen", "District Heat")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  IFE <- readr::read_csv(paste0(query_dir,query), skip = 1) %>%
    select(-X27) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    left_join(industry_fe_lookup, by = "input") %>%
    group_by(scenario, region, Units, year, fuel) %>%
    summarise(value = sum(value)) %>%
    ungroup %>%
    mutate(fuel = factor(fuel, levels = fuel_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(IFE, "query") <- "Industry Final Energy"
  attr(IFE, "colors") <- FINAL_ENERGY_COLORS
  attr(IFE, "fill") <- "fuel"

  return(IFE)
}
