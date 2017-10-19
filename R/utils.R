#' read_query
#'
#' Read in GCAM queries
#' @import dplyr tidyr

read_query <- function(...){
  suppressMessages(readr::read_csv(...))
}

#' query_id
#'
#' Creates tibble with query output file names and query names
#' @param folder Directory with GCAM queries only
#' @export
#' @examples
#' query_id(output_dir = "gcam_queries")

query_id <- function(folder){
  if (substr(folder, nchar(folder), nchar(folder)) != "/"){
    folder <- paste0(folder, "/")
  }
  out_df <- tibble::tibble(file = character(), title = character())
  input_files <- dir(folder, pattern = ".csv")
  for (file in input_files){
    line <- readr::read_lines(paste0(folder,file), n_max =1) %>%
      # Assumes no commas in any query title
      stringr::str_replace_all(",","")
    out_df <- out_df %>% dplyr::bind_rows(tibble::tibble(file = file, title = line ))
  }
  return(out_df)
}

#' all_bar_chart
#'
#' Saves all bar charts in list from data_processer
#' @param data List from data_processer
#' @param output_dir Directory to save figures to
#' @export

all_bar_chart <- function(data, output_dir, ...){
  # Only plot data with default_plot == "bar_plot"
  bar_data <- list()
  for (name in names(data)){
    if (attributes(data[[name]])$default_plot == "bar"){
      bar_data[[name]] <- data[[name]]
    }
  }
  invisible(lapply(bar_data, bar_chart_plot,output_dir = output_dir, ...))
} # all_bar_chart

#' all_line_chart
#'
#' Saves all line charts in list from data_processer
#' @param data List from data_processer
#' @param output_dir Directory to save figures to
#' @export

all_line_chart <- function(data, output_dir, ...){
  # Only plot data with default_plot == "bar_plot"
  line_data <- list()
  for (name in names(data)){
    if (attributes(data[[name]])$default_plot == "line"){
      line_data[[name]] <- data[[name]]
    }
  }
  invisible(lapply(line_data, line_chart_plot,output_dir = output_dir, ...))
} # all_bar_chart

#' scenario_rename
#'
#' Returns a list of processed data for all queries that have processing code
#' @param data Processed GCAm data
#' @param name_lookup Vector of old scenarios (without timestamp) and new names, see example
#' @import dplyr tidyr
#' @export
#' @examples
#' scenario_rename(data = df[["primary_energy"]], name_lookup = c(Reference = "BAU", Ctax20 = "Carbon Tax $20"))
#' lapply(data, scenario_rename, name_lookup = c(Reference = "BAU", Ctax20 = "Carbon Tax $20"))
scenario_rename <- function(data, name_lookup){
  same <- intersect(unique(data$scenario), names(name_lookup))
  if (length(same) != length(unique(data$scenario))){
    stop("Not all scenarios have new names")
  }else{
    attr_data <- attributes(data)
    data <- data %>%
      mutate(scenario = name_lookup[scenario]) %>%
      mutate(scenario = factor(scenario, levels = name_lookup))
    attributes(data) <- attr_data
  }

  if ("diff_scenario" %in% names(data)){
    if(unique(data$diff_scenario) %in% names(name_lookup)){
      data <- data %>% mutate(diff_scenario = name_lookup[diff_scenario])
      attributes(data) <- attr_data
    }else{
      warning("Diff Scenario does not have a new name")
    }

  }
  return(data)
}

#' data_processer
#'
#' Returns a list of processed data for all queries that have processing code
#' @param folder Directory with GCAM queries only
#' @param scenarios Vector of scenarios to include in data
#' @import dplyr tidyr
#' @export

data_processer <- function(folder = QUERY_FOLDER, scenarios = SCENARIOS){
  data_output <- list()
  if (substr(folder, nchar(folder), nchar(folder)) != "/"){
    folder <- paste0(folder, "/")
  }
  queries <- query_id(folder)
  for (i in 1:dim(queries)[1]){
    if (queries$title[i] %in% query_function_map){
      fun_name <- which(query_function_map == queries$title[i]) %>% names
      for (name in fun_name){
        fun <- get(name)
        object_name <- name %>% stringr::str_replace("_data", "")
        data_output[[object_name]] <- fun(queries$file[i], scenarios, folder)
      }
    }
  }
  return(data_output)
}

#' diff_processer
#'
#' Returns a list of processed data for all queries that have processing code, showing difference between scenarios and a reference scenario
#' @param folder Directory with GCAM queries only
#' @param scenarios Vector of scenarios to include in data
#' @param diff_scenario Scenario to diff all other scenarios by. Must be length 1
#' @import dplyr tidyr
#' @export

diff_processer <- function(folder = QUERY_FOLDER, scenarios = SCENARIOS, diff_scenario = DIFF_SCENARIO){
  stopifnot(length(diff_scenario) == 1)
  data_output <- list()
  if (substr(folder, nchar(folder), nchar(folder)) != "/"){
    folder <- paste0(folder, "/")
  }
  queries <- query_id(folder)
  for (i in 1:dim(queries)[1]){
    if (queries$title[i] %in% query_function_map){
      fun_name <- which(query_function_map == queries$title[i]) %>% names
      for (name in fun_name){
        fun <- get(name)
        object_name <- name %>% stringr::str_replace("_data", "")
        scenarios_df <- fun(queries$file[i], scenarios, folder) %>% rename(scen_value = value)
        attr_data <- attributes(scenarios_df)
        diff_scenario_df <- fun(queries$file[i], diff_scenario, folder) %>%
          rename(diff_scenario = scenario, diff_value = value)
        data_output[[object_name]] <- scenarios_df %>%
          left_join(diff_scenario_df, by = c("region", attributes(scenarios_df)$fill, "Units", "year")) %>%
          mutate(value = scen_value - diff_value)
        attr(data_output[[object_name]], "query") <- attr_data$query
        attr(data_output[[object_name]], "colors") <- attr_data$colors
        attr(data_output[[object_name]], "fill") <- attr_data$fill
        attr(data_output[[object_name]], "default_plot") <- attr_data$default_plot
      }
    }
  }
  return(data_output)
}

#' query_splitter
#'
#' For one csv file with multiple queries. Writes into individual query csv files
#' @param query_file Query file location
#' @param output_dir Where output csvs are to be written
#' @import dplyr tidyr
#' @export

query_splitter <- function(query_file, output_dir){
  # Read in non-scenario-specific queries copied to excel and saved as csv's
  # This for loop divides up the query into individual queries, based on where "scenario" occurs in the query output
  query_text <- readr::read_lines(query_file)
  query_start <- grep("scenario", query_text)
  # keep queries whose names are in the query_function_map
  query_names <- unique(query_text[query_start - 1])
  query_names <- query_names[query_names %in% query_function_map]

  # Loop through for each query name (there may be multiple locations with same query)
  for (name in query_names){
    nums <- which(query_text[query_start - 1] == name)
    tibble_list <- list()
    for (i in nums){
      query_pos <- query_start[i]
      # If the query is not the last query in file, only read in until next query
      if (i != length(query_start)){
        next_query_pos <- query_start[i+1]
        df <- read_query(query_file, skip = query_pos - 1, n_max = next_query_pos - query_pos - 2)
        # Otherwise, read until end of file
      }else{
        df <- read_query(query_file, skip = query_pos - 1)
      }
      tibble_list[[i]] <- df
    }
    # bind rows of same query and write output
    df <- do.call(bind_rows, tibble_list)
    num <- which(query_names == name)
    fqfn <- paste0(output_dir,"query_", num,".csv")

    # Add title to csv, then write
    cat(name, file = fqfn, sep = "\n")
    readr::write_csv(df, fqfn, append = TRUE, col_names = TRUE)
  }
}
