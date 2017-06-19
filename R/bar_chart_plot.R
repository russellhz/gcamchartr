#' bar_chart_plot
#'
#' Create bar chart of processed data
#' @param data Dataframe/tibble of processed GCAM data
#' @param output_dir Output directory to save charts to
#' @param fill_col Column name, in quotes, to color by. Defaults to attribute of "fill" for processed data.
#' @param colors Vector of colors for chart. Defaults to attribute of "colors" for processed data
#' @param break_size Interval of years to print on x axis, starting with minimum year in data. Default is 10 years
#' @keywords bar chart
#' @import tidyverse stringr
#' @export
#' @examples
#' bar_chart_plot(df, output_dir = "/outputs", fill = "technology", colors = c("red", "blue"))
#' bar_chart_plot(data[["primary_energy"]], output_dir = "/outputs")

bar_chart_plot <- function(data, output_dir, fill_col = attributes(data)$fill, colors = attributes(data)$colors, width = 10, height = 7, break_size = 10){
  if ("Units" %in% names(data)) {y_axis <- unique(data$Units)}

  all_scen_plot <- ggplot(data, aes_string("year", "value", fill = fill_col)) +
    geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = colors) +
    guides(fill = guide_legend(reverse = TRUE, title=str_to_title(fill_col))) +
    labs(y = y_axis, title = attributes(data)$query) +
    theme(plot.title = element_text(size = 18, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12, angle = 45),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 14),
          legend.title = element_text(size = 14)) +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year),break_size))


  if ("scenario" %in% names(data) & length(data$scenario) > 1){
    all_scen_plot <- all_scen_plot +
      facet_wrap(~scenario, ncol = 2)
  }
  ggsave(paste0(output_dir, attributes(data)$query,".png"), plot = all_scen_plot, width = width, height = height)
}
