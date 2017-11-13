#' tornado_chart_plot
#'
#' Create tornado chart of one year/variable from processed data
#' @param data Dataframe/tibble of processed GCAM data
#' @param ref_scenario Scenario to use as base for tornado to start from
#' @param plot_title Title to add, defaults to no title
#' @param output_dir Output directory to save charts to
#' @param savename Name to save plot under
#' @param color_flip Default is green above, red below. Switch to T to flip
#' @param break_size Interval of years to print on x axis, starting with minimum year in data. Default is 10 years
#' @param region String of region name to filter by
#' @keywords bar chart
#' @import dplyr ggplot2 tidyr
#' @export

tornado_chart_plot <- function(data, output_dir, ref_scenario, savename, plot_title = NULL, color_flip = F, region = NULL, width = 10, height = 7, break_size = 10){
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

  # Set y axis label
  if ("Units" %in% names(data)) {y_axis <- unique(data$Units)}

  max_y_axis <- max(data$value) * 1.05

  # Contort data to allow for tornado plotting -- must be single year and type (fuel, input, etc.)
  data <- data %>%
    mutate(diff = value - value[scenario == ref_scenario],
           empty = if_else(diff > 0, value[scenario == ref_scenario], value),
           above = if_else(diff >= 0, diff, 0),
           below = if_else(diff < 0, -1 * diff, 0),
           empty = replace(empty, scenario == ref_scenario, 0),
           base = if_else(scenario == ref_scenario, value, 0)) %>%
    select(-value) %>%
    gather(bar_type, value, empty, above, below, base) %>%
    mutate(bar_type = factor(bar_type, levels = c("base", "empty", "above", "below")))

  # Get value for dotted line to add to plot
  base_line <- data[data$bar_type == "base" & data$scenario == ref_scenario,]$value

  all_scen_plot <- ggplot(data, aes(scenario, value, fill = bar_type, alpha = bar_type)) +
    geom_bar(stat="identity", position = position_stack(reverse = TRUE))

  if (color_flip == TRUE){
    all_scen_plot <- all_scen_plot +
      scale_fill_manual(values = c(empty = "white", base = "navy",
                                   above = "firebrick2", below = "chartreuse3"))
  } else {
    all_scen_plot <- all_scen_plot +
      scale_fill_manual(values = c(empty = "white", base = "navy",
                                   above = "chartreuse3", below = "firebrick2"))
  }
  all_scen_plot <- all_scen_plot +
    scale_alpha_manual(values = c(empty = 0, base = 1, above = 1, below = 1)) +
    theme(legend.position="none") +
    labs(y = y_axis, x = "Scenario") +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background = element_rect(color = "white", fill = "white"),
          panel.grid.major.y = element_line(color = "gray62"),
          panel.grid.minor.y = element_line(color = "gray62")) +
    scale_x_discrete(expand = c(0.01,0)) +
    expand_limits(y = 0) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_y_axis)) +
    geom_hline(yintercept = base_line, linetype = "dashed", size = 1)


  if (!is.null(plot_title)){
    all_scen_plot <- all_scen_plot +
      labs(title = plot_title) +
      theme(plot.title = element_text(size = 18, face = "bold"))
  }

  filename <- paste0(output_dir, savename,".png")

  if (!is.null(region)){
    if (exists("region_name")){
      start <- regexpr(attributes(data)$query, filename)[1]
      filename <- paste0(substr(filename, 1, start - 1), region_name, "_", substr(filename, start, nchar(filename)))
    }
  }

  ggsave(filename, plot = all_scen_plot, width = width, height = height)

}
