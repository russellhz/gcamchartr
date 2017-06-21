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
  land_alloc_lookup <- read_query(system.file("extdata", "land_alloc_lookup.csv", package = "gcamchartr"))

  land_order <- c("Forest (unmanaged)","Forest (managed)","Pasture (other)", "Pasture (grazed)", "Desert",
                  "Grass", "Shrubs", "Crops", "Biomass", "OtherArable", "Urban", "Tundra")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  LA <- read_query(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    mutate(`land-allocation` = substr(`land-allocation`, 1, nchar(`land-allocation`)-5)) %>%
    gather(year, value, `1990`:`2100`) %>%
    left_join(land_alloc_lookup, by = "land-allocation") %>%
    group_by(scenario, region, Units, year, Land) %>%
    summarise(value = sum(value)) %>%
    ungroup %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    mutate(Land = factor(Land, levels = land_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(LA, "query") <- query_title
  attr(LA, "colors") <- LAND_COLORS
  attr(LA, "fill") <- "Land"
  attr(LA, "default_plot") <- "bar"

  return(LA)
}
