#' pass_transport_so_data
#'
#' Process transportation service output query - filtering out only trn_pass_road_LDV_4W
#' @param query GCAM query containing transportation service output data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords passenger transportation service output
#' @import dplyr tidyr
#' @export
#' @examples
#' pass_transport_so_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

pass_transport_so_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  tech_order <- c("Liquids", "Hybrid Liquids", "NG", "BEV", "FCEV")

  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  PSO <- readr::read_csv(paste0(query_dir,query), skip = 1) %>%
    select(-X28) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios,
           sector == "trn_pass_road_LDV_4W") %>%
    gather(year, value, `1990`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    mutate(value = value/1000,
           Units = "billion-pass-km",
           technology = factor(technology, levels = tech_order),
           scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(PSO, "query") <- paste("Passenger",query_title)
  attr(PSO, "colors") <- TRANSPORT_COLORS
  attr(PSO, "fill") <- "technology"
  return(PSO)
}
