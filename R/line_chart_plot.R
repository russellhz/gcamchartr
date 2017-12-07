#' line_chart_plot
#'
#' Create line chart of processed data
#' @param data Dataframe/tibble of processed GCAM data
#' @param output_dir Output directory to save charts to
#' @param fill_col Column name, in quotes, to color by. Default is "scenario". To use other column, may need to add facet.
#' @param diff If true, adds subtitle to plot and DIFF to output name
#' @param break_size Interval of years to print on x axis, starting with minimum year in data. Default is 10 years
#' @param region String of region name to filter by
#' @keywords line chart
#' @import dplyr ggplot2 tidyr
#' @export
#' @examples
#' line_chart_plot(df, output_dir = "/outputs")
#' line_chart_plot(data[["primary_energy"]], output_dir = "/outputs")

line_chart_plot <- function(data, output_dir, fill_col = "scenario", diff = F,
                           region = NULL, width = 10, height = 7, break_size = 10,
                           manual_colors = NULL, jitter = F){
  # Filter by region if necessary
  if (!is.null(region)){
    if (length(unique(data$region)) > 1){
      region_name <- region
      data <- data %>% filter(region == region_name)
    }else{
      warning(paste("Data only exists for region",unique(data$region), "in", attributes(data)$query))
    }
  }

  # Make sure output directory exists and ends in "/"
  if (substr(output_dir, nchar(output_dir), nchar(output_dir)) != "/"){
    output_dir <- paste0(output_dir, "/")
  }
  if (!dir.exists(output_dir)) {dir.create(output_dir)}

  if ("Units" %in% names(data)) {y_axis <- unique(data$Units)}

  # Data needs to be aggregated to only the fill(color) column and year
  data <- data %>% group_by_(fill_col, "year") %>% summarise(value = sum(value))

  max_y_axis <- max(data$value) * 1.05

  min_y_axis <- min(0, min(data$value) * 1.05)

  all_scen_plot <- ggplot(data, aes_string("year", "value", color = fill_col))

  if (jitter == T){
    all_scen_plot <- all_scen_plot +
      geom_line(size = 1.5, position = position_jitter(h = 0))
  } else {
    all_scen_plot <- all_scen_plot +
      geom_line(size = 1.5)
  }
  all_scen_plot <- all_scen_plot +
    guides(color = guide_legend(title = stringr::str_to_title(fill_col))) +
    labs(y = y_axis, title = attributes(data)$query) +
    theme(plot.title = element_text(size = 18, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background = element_rect(color = "white", fill = "white"),
          panel.grid.major.y = element_line(color = "gray62"),
          panel.grid.minor.y = element_line(color = "gray62")) +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year),break_size),
                       expand = c(0,0)) +
    expand_limits(y = 0) +
    scale_y_continuous(expand = c(0, 0), limits = c(min_y_axis, max_y_axis))

  # Use own colors if not NULL
  if (!is.null(manual_colors)){
    all_scen_plot <- all_scen_plot +
      scale_color_manual(values = manual_colors)
  }

  if (diff == T){
    all_scen_plot <- all_scen_plot +
      labs(subtitle = paste("Change from", unique(data$diff_scenario))) +
      theme(plot.subtitle = element_text(size = 14, face = "italic"))

    filename <- paste0(output_dir, "DIFF_", attributes(data)$query,"_line.png")
  } else{ filename <- paste0(output_dir, attributes(data)$query,"_line.png")}

  if (!is.null(region)){
    if (exists("region_name")){
      start <- regexpr(attributes(data)$query, filename)[1]
      filename <- paste0(substr(filename, 1, start - 1), region_name, "_", substr(filename, start, nchar(filename)))
    }
  }

  ggsave(filename, plot = all_scen_plot, width = width, height = height)

}
