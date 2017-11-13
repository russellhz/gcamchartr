#' bar_chart_plot
#'
#' Create bar chart of processed data
#' @param data Dataframe/tibble of processed GCAM data
#' @param output_dir Output directory to save charts to
#' @param fill_col Column name, in quotes, to color by. Defaults to attribute of "fill" for processed data.
#' @param diff If true, adds subtitle to plot and DIFF to output name
#' @param colors Vector of colors for chart. Defaults to attribute of "colors" for processed data
#' @param break_size Interval of years to print on x axis, starting with minimum year in data. Default is 10 years
#' @param region String of region name to filter by
#' @keywords bar chart
#' @import dplyr ggplot2 tidyr
#' @export
#' @examples
#' bar_chart_plot(df, output_dir = "/outputs", fill = "technology", colors = c("red", "blue"))
#' bar_chart_plot(data[["primary_energy"]], output_dir = "/outputs")

bar_chart_plot <- function(data, output_dir, fill_col = attributes(data)$fill, colors = attributes(data)$colors, diff = F,
                           region = NULL, width = 10, height = 7, break_size = 10, remove_title = F){
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

  max_y_axis <- data %>%
    group_by(scenario, year) %>%
    summarise(value = sum(value)) %>%
    ungroup()

  max_y_axis <- max(max_y_axis$value) * 1.05

  all_scen_plot <- ggplot(data, aes_string("year", "value", fill = fill_col)) +
    geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = colors) +
    guides(fill = guide_legend(reverse = TRUE, title = stringr::str_to_title(fill_col))) +
    labs(y = y_axis) +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background = element_rect(color = "white", fill = "white"),
          panel.grid.major.y = element_line(color = "gray62"),
          panel.grid.minor.y = element_line(color = "gray62")) +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year),break_size),
                       expand = c(0.01,0)) +
    expand_limits(y = 0) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_y_axis))


  if (remove_title == FALSE){
    all_scen_plot <- all_scen_plot +
      labs(title = attributes(data)$query) +
      theme(plot.title = element_text(size = 18, face = "bold"))
  }

  if ("scenario" %in% names(data) & length(data$scenario) > 1){
    all_scen_plot <- all_scen_plot +
      facet_wrap(~scenario, nrow = 2, scales = "free_x")
  }

  if (diff == T){
    all_scen_plot <- all_scen_plot +
      labs(subtitle = paste("Change from", unique(data$diff_scenario))) +
      theme(plot.subtitle = element_text(size = 14, face = "italic")) +
      geom_hline(yintercept=0)

    filename <- paste0(output_dir, "DIFF_", attributes(data)$query,".png")
  } else{ filename <- paste0(output_dir, attributes(data)$query,".png")}

  if (!is.null(region)){
    if (exists("region_name")){
      start <- regexpr(attributes(data)$query, filename)[1]
      filename <- paste0(substr(filename, 1, start - 1), region_name, "_", substr(filename, start, nchar(filename)))
    }
  }

  ggsave(filename, plot = all_scen_plot, width = width, height = height)

}
