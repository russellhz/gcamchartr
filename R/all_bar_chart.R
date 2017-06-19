#' all_bar_chart
#'
#' Saves all bar charts in list from data_processer
#' @param data List from data_processer
#' @param output_dir Directory to save figures to
#' @export

all_bar_chart <- function(data, output_dir, width = 10, height = 7, break_size = 10){
  invisible(lapply(data, bar_chart_plot,output_dir = output_dir, width = 10, height = 7, break_size = 10))
} # all_bar_chart
