#'Evolution of traffic (global, per mode ou per direction), smoothed traffic
#'during a period.
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param modes Character vector. Different modes of transportation aggregated (heavy, car, bike, pedestrian) . Default: heavy & car
#' @param direction Character. Direction of the traffic (lft, rgt, both). Default to both.
#' @param smoothed Boolean. Should the smoothed traffic be plotted ? Default: True
#' @param agg_day Boolean. Should the data be aggregated per day ? Default : True
#'
#' @return graph showing the evolution of traffic (for specified parameters) during the specified period.
#' @export
#'
#' @import dplyr
#' @import ggplot2
#'
gg_traffic_evolution <- function(enriched_data,
                                 date_range = NULL,
                                 segments = NULL,
                                 modes = c('heavy','car'),
                                 direction = "both",
                                 smoothed = TRUE,
                                 agg_day = TRUE){
  enriched_data$date <- as.POSIXct(enriched_data$date,
                                      format = "%Y-%m-%d %H:%M:%S",
                                      tz = "CET")

  # Parameters check
  transportation_options = c('pedestrian','car','heavy','bike')
  mode = check_options_graph(modes, transportation_options, c('heavy','car'))
  directions_options = c('both','lft','rgt')
  direction = check_options_graph(direction, directions_options, c('both'))

  # Filter on parameters
  if(length(date_range) > 1){
    enriched_data <- enriched_data %>%
      filter(dplyr::between(.data$day, as.Date(date_range[1]), as.Date(date_range[2])))
  }
  if(length(segments) > 1){
    enriched_data <- enriched_data %>%
      filter(segment_id %in% segment)
  }
  if(direction != "both"){
    mode = paste(mode, direction, sep = "_")
  }

  # Aggregation by day or by date
  agg_level = if_else(agg_day, "date", "day")
  traffic <- enriched_data %>%
    group_by_at(agg_level) %>%
    summarise(traffic_sum = sum(c_across(mode)))

  # Graph
  graph <- ggplot(traffic, aes_string(x = agg_level, y = "traffic_sum")) +
    geom_line() +
    labs(title = "Traffic over time") +
    xlab("Date") +
    ylab(paste("Number of", paste(mode, collapse = " and "))) +
    theme_bw()
  if(smoothed){ # (smooth option)
    graph <- graph +
      geom_smooth(method='gam', formula=y ~ s(x, bs = "cs"),color="#B1D62E", linewidth=2)
  }

  return(graph)
}
