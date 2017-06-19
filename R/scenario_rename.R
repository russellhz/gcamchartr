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
    data <- data %>% mutate(scenario = SCEN_NAMES[scenario])
    attributes(data) <- attr_data
  }
  return(data)
}
