#' all_bar_chart
#'
#' Saves all bar charts in list from data_processer
#' @param data List from data_processer
#' @param output_dir Directory to save figures to
#' @export

all_bar_chart <- function(data, output_dir, ...){
  invisible(lapply(data, bar_chart_plot,output_dir = output_dir, ...))
} # all_bar_chart
