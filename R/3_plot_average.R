#'Average of traffic during a week over a period for a segment or a subset of segment,
#'for a transportation mode or more, for a direction or both.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param modes Character vector. Different modes of transportation aggregated (heavy, car, bike, pedestrian) . Default: heavy & car
#' @param direction Character. Direction of the traffic (lft, rgt, both). Default to both.
#' @param weekday Character vector. Weekday choosen. Default to the all week.
#' @param aggregated_by Character. Options are : 'segment_name', 'weekday', 'direction', 'mode'. Default : 'weekday'.
#'
#' @return graph showing weekly average evolution of traffic (for specified parameters) during the specified period.
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr unnest
#'
gg_traffic_avg <- function(enriched_data,
                           date_range = NULL,
                           segments = NULL,
                           modes = c('heavy','car'),
                           direction = "both",
                           weekday = NULL,
                           aggregated_by = "weekday"){

  if(aggregated_by == 'direction'){
    direction = c('lft','rgt')
  }

  result <- filtering_agg(enriched_data,
                          date_range = date_range,
                          segments = segments,
                          modes = modes,
                          direction = direction,
                          weekdays = weekday)

  # Aggregation
  if(length(aggregated_by)!=1){
    message('Invalid aggregation level: please select only one level.')
    aggregated_by = "weekday"
  }
  agg_level = check_options_graph(aggregated_by, c('segment_name','weekday','direction','mode'),'weekday')

  traffic <- result$data %>%
    group_by_at(c(aggregated_by, 'hour')) %>%
    summarise(traffic_sum = mean(.data$traffic_sum))

  # Graph
  seg <- unique(result$data$segment_name)
  graph <- ggplot(traffic, aes(x = hour,
                               y = .data$traffic_sum,
                               color = as.factor(.data[[aggregated_by]]))) +
    geom_line() +
    labs(title = paste("Average traffic per", aggregated_by),
         subtitle = graph_subtitles(weekdays= result$weekday,
                                    segments= result$segment,
                                    directions= result$direction,
                                    modes= result$mode,
                                    hours = result$hour)) +
    xlab("Hour") +
    ylab(paste("Number of", paste(result$mode, collapse = " and "))) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_color_manual(name = aggregated_by,
                       values = custom_colors_palette(seg)[[aggregated_by]]) +
    ylim(0, NA)

  return(graph)
}
