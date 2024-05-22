#'Evolution of traffic and smoothed traffic.
#'
#' @description
#' Evolution of traffic (global, per mode ou per direction), smoothed traffic
#' during a period.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. Example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment by its name, all if NULL (default).
#' @param modes Character vector. Different modes of transportation aggregated (heavy, car, bike, pedestrian) . Default: heavy & car
#' @param direction Character. Direction of the traffic (lft, rgt, both). Default to both.
#' @param smoothed Boolean. Should the smoothed traffic be plotted ? Default: True
#' @param agg_day Boolean. Should the data be aggregated per day ? Default : True
#'
#' @return Graph showing the evolution of traffic (for specified parameters) during the specified period.
#' @export
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' gg_traffic_evolution(traffic)
#' gg_traffic_evolution(traffic,
#'   date_range = c('2022-01-01','2022-03-01'),
#'   segment = 'RteVitre-06',
#'   mode = c('car','pedestrian'),
#'   direction = 'lft',
#'   smoothed = FALSE,
#'   agg_day = FALSE)
gg_traffic_evolution <- function(enriched_data,
                                 date_range = NULL,
                                 segments = NULL,
                                 modes = c('heavy','car'),
                                 direction = "both",
                                 smoothed = TRUE,
                                 agg_day = TRUE){

  result <- filter_agg(enriched_data,
                       date_range = date_range,
                       segments = segments,
                       modes = modes,
                       direction = direction)

  # Aggregation by day or by date
  agg_level = if_else(agg_day, "date", "day")
  traffic <- result$data %>%
    group_by_at(agg_level) %>%
    summarise('traffic_sum' = sum(.data$traffic_sum))

  # Graph
  graph <- ggplot(traffic, aes_string(x = agg_level, y = "traffic_sum")) +
    geom_line() +
    labs(title = "Traffic over time",
         subtitle = get_graph_subtitles(weekdays= result$weekday,
                                        segments= result$segment,
                                        directions= result$direction,
                                        modes= result$mode,
                                        hours = result$hour)) +
    xlab("Date") +
    ylab(paste("Number of", paste(result$mode, collapse = " and "))) +
    theme_bw() +
    ylim(0, NA)
  if(smoothed){ # (smooth option)
    graph <- graph +
      geom_smooth(method='gam', formula=y ~ s(x, bs = "cs"),color="#B1D62E", linewidth=2)
  }

  return(graph)
}
