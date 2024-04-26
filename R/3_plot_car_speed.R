#'Histogram of car speed over a period, for a segment or a subset of segment.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param weekday Character vector. Weekday choosen. Default to the all week.
#' @param aggregated_by Character. Enables comparison with other segments or weekdays. Options are : 'segment_name', 'weekday', NULL (no comparison, default).
#'
#' @return graph showing histogram of car speed over a period.
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr unnest_wider
#' @importFrom scales percent
#'
gg_car_speed_histogram <- function(enriched_data,
                                   date_range = NULL,
                                   segments = NULL,
                                   weekday = NULL,
                                   aggregated_by = NULL){

  aggregated_by <- check_options_graph(aggregated_by,
                                       c('segment_name','weekday'),
                                       NULL)

  # filtering data
  result <- filtering_agg(enriched_data,
                          date_range = date_range,
                          segments = segments,
                          weekdays = weekday)
  result$data <- result$data %>% filter(.data$mode == 'car',
                                        .data$direction == 'both')

  # generate labels
  speed_labels <- paste(seq(0,120,5), '-', c(seq(5,120,5),'120+'), 'km/h')
  names(speed_labels) <- paste('car_speed_hist_0to120plus',1:25, sep = "_")

  # aggregate speed histogramm
  id.vars = c('segment_name','weekday','traffic_sum')
  speed_hist <- result$data %>%
    select(all_of(c(id.vars, 'car_speed_hist_0to120plus'))) %>%
    unnest_wider(.data$car_speed_hist_0to120plus, names_sep = "_") %>%
    melt(id.vars = id.vars,
         variable.name = "speed_class",
         value.name = "speed_prop") %>%
    mutate('speed_class' = speed_labels[.data$speed_class])

  # aggregation by criteria
  if(!is.null(aggregated_by)){
    speed_hist <- speed_hist %>% group_by_at(c(aggregated_by, "speed_class"))
  } else {
    speed_hist <- speed_hist %>% group_by(.data$speed_class)
  }
  speed_hist <- speed_hist %>%
    summarise('speed_sum' = sum(.data$speed_prop*.data$traffic_sum)) %>%
    mutate('speed_prop' = .data$speed_sum/sum(.data$speed_sum))

  # Order labels
  speed_hist$speed_class <- ordered(speed_hist$speed_class, levels = speed_labels)

  # Graph
  if(!is.null(aggregated_by)){
    graph <- ggplot(speed_hist, aes(x = .data$speed_class,
                                    y = .data$speed_prop,
                                    fill = as.factor(.data[[aggregated_by]]))) +
      geom_bar(stat = 'identity', position = position_dodge()) +
      scale_fill_discrete(name = aggregated_by)
  } else {
    graph <- ggplot(speed_hist, aes(x = .data$speed_class,
                                    y = .data$speed_prop)) +
      geom_bar(stat = 'identity')
  }
  graph <- graph +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = 'bottom') +
    scale_y_continuous(labels = percent) +
    labs(title = "Speed Histogram",
         subtitle = paste(
           paste("Weekdays:",paste(result$weekday, collapse = ", ")),
           paste("\nSegments:",paste(result$segment, collapse = ", ")),
           sep = ", ")) +
    xlab('') + ylab("Proportion of cars")

  return(graph)
}
