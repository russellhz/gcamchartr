#' industry_so_enuse_data
#'
#' Process industrial primary output query for only industrial energy use
#' @param query GCAM query containing building final energy output data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords building final energy
#' @import dplyr tidyr
#' @export
#' @examples
#' industry_so_enuse_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

industry_so_enuse_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  tech_order <- c("Refined Liquids", "Gas", "Coal", "Biomass",
                  "Electricity", "Hydrogen",  "Traditional Biomass", "District Heat")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  IEU <- read_query(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010,
           grepl('industrial energy use', sector)) %>%
    mutate(technology = stringr::str_replace(technology, " cogen", "")) %>%
    group_by(scenario, region, Units, year, technology) %>%
    summarise(value = sum(value)) %>%
    ungroup %>%
    mutate(technology = stringr::str_to_title(technology),
           technology = factor(technology, levels = tech_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(IEU, "query") <- "Industrial Energy Use Primary Output"
  attr(IEU, "colors") <- SERVICE_OUTPUT_COLORS
  attr(IEU, "fill") <- "technology"
  attr(IEU, "default_plot") <- "bar"

  return(IEU)
}

