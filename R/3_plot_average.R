#'Average of traffic during a week over a period for a segment or a subset of segment,
#'for a transportation mode or more, for a direction or both.
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param modes Character vector. Different modes of transportation aggregated (heavy, car, bike, pedestrian) . Default: heavy & car
#' @param direction Character. Direction of the traffic (lft, rgt, both). Default to both.
#' @param agg_day Boolean. Should the data be aggregated per day ? Default : True
#'
#' @return graph showing weekly average evolution of traffic (for specified parameters) during the specified period.
#' @export
#'
#' @import dplyr
#' @import ggplot2
#'
gg_traffic_avg <- function(enriched_data,
                           date_range = NULL,
                           segments = NULL,
                           modes = c('heavy','car'),
                           direction = "both",
                           agg_day = TRUE){

  result <- filtering_agg(enriched_data,
                          date_range,
                          segments,
                          modes,
                          direction)

  # Aggregation by weekday
  traffic <- result$data %>%
    group_by(.data$weekday, hour) %>%
    summarise(traffic_sum = sum(traffic))

  # Ordering weekdays
  traffic$weekday <- ordered(traffic$weekday, levels = c("lundi","mardi","mercredi","jeudi",                                                         "vendredi","samedi","dimanche"))

  # Graph
  graph <- ggplot(traffic, aes(x = hour, y = .data$traffic_sum, color = .data$weekday)) +
    geom_line() +
    labs(title = "Average traffic per weekday") +
    xlab("Hour") +
    ylab(paste("Number of", paste(result$mode, collapse = " and "))) +
    theme_bw() +
    theme(legend.position = "bottom")

  return(graph)
}
