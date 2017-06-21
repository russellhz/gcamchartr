# Climate data functions are all very similar & simple

#' temperature_data
#'
#' Process temperature query
#' @param query GCAM query containing temperature data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords temperature
#' @import dplyr tidyr
#' @export
#' @examples
#' temperature_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

temperature_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  df <- read_query(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1980`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    mutate(scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(df, "query") <- query_title
  attr(df, "colors") <- NULL
  attr(df, "default_plot") <- "line"
  return(df)
}


#' climate_forcing_data
#'
#' Process climate forcing query
#' @param query GCAM query containing climate forcing data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords climate forcing
#' @import dplyr tidyr
#' @export
#' @examples
#' climate_forcing_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

climate_forcing_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  df <- read_query(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1980`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    mutate(scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(df, "query") <- query_title
  attr(df, "colors") <- NULL
  attr(df, "default_plot") <- "line"
  return(df)
}


#' CO2_concentration_data
#'
#' Process CO2 concentration query
#' @param query GCAM query containing CO2 concentration data of one or multiple scenarios
#' @param scenario GCAM scenarios to include in processed data
#' @keywords CO2 concentration
#' @import dplyr tidyr
#' @export
#' @examples
#' CO2_concentration_data("queryA.csv", c("Reference1,date=2017-9-6T13:43:53-07:00", "Reference2,date=2017-9-6T13:43:53-07:00"))

CO2_concentration_data <- function(query, scenarios, query_dir = QUERY_FOLDER){
  query_title <- query_id(query_dir) %>%
    filter(file == query) %>%
    select(title) %>%
    as.character

  df <- read_query(paste0(query_dir,query), skip = 1) %>%
    filter(scenario != query_title, scenario != "scenario",
           scenario %in% scenarios) %>%
    gather(year, value, `1980`:`2100`) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 2010) %>%
    mutate(scenario = if_else(grepl(",date", scenario),
                              substr(scenario, 1, regexpr(",date", scenario)[1]-1),
                              scenario))
  attr(df, "query") <- query_title
  attr(df, "colors") <- NULL
  attr(df, "default_plot") <- "line"
  return(df)
}
