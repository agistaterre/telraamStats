#'Histogram of car speed over a period, for a segment or a subset of segment.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. Example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param weekday Character vector. Weekday choosen. Default to the all week.
#' @param hours Integer vector. Hours choosen, default to the all day.
#' @param aggregated_by Character. Enables comparison with other segments or weekdays. Options are : 'segment_name', 'weekday', NULL (no comparison, default).
#'
#' @return Graph showing histogram of car speed over a period.
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr unnest_wider
#' @importFrom scales percent
#'
#' @examples
#' library(dplyr)
#' subset_traffic <- traffic %>% filter(day < '2022-02-01', hour > 9)
#' gg_car_speed_histogram(subset_traffic)
#' gg_car_speed_histogram(subset_traffic,
#'   aggregated_by = 'segment_name')
#' gg_car_speed_histogram(subset_traffic,
#'   weekday = c('monday','sunday'),
#'   segments = 'RteVitre-06',
#'   hours = 17:20,
#'   aggregated_by = "weekday")
gg_car_speed_histogram <- function(enriched_data,
                                   date_range = NULL,
                                   segments = NULL,
                                   weekday = NULL,
                                   hours = NULL,
                                   aggregated_by = NULL){

  # filtering data and create speed labels
  result <- preprocess_car_speed(enriched_data = enriched_data,
                                 date_range = date_range,
                                 segments = segments,
                                 weekday = weekday,
                                 hours = hours,
                                 aggregated_by = aggregated_by)

  # aggregate speed histogramm
  id.vars = c('segment_name','weekday','traffic_sum')
  speed_hist <- result$data %>%
    select(all_of(c(id.vars, 'car_speed_hist_0to120plus'))) %>%
    unnest_wider(.data$car_speed_hist_0to120plus, names_sep = "_") %>%
    melt(id.vars = id.vars,
         variable.name = "speed_class",
         value.name = "speed_prop") %>%
    mutate('speed_class' = result$speed_labels[.data$speed_class])

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
  speed_hist$speed_class <- ordered(speed_hist$speed_class, levels = result$speed_labels)

  # Graph
  aggregated_label <- gsub("[[:punct:]]", " ", aggregated_by)
  if(!is.null(aggregated_by)){
    seg <- unique(result$data$segment_name)
    graph <- ggplot(speed_hist, aes(x = .data$speed_class,
                                    y = .data$speed_prop,
                                    fill = as.factor(.data[[aggregated_by]]))) +
      geom_bar(stat = 'identity', position = position_dodge()) +
      scale_fill_manual(name = aggregated_label,
                         values = get_custom_palette(seg)[[aggregated_by]])
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
         subtitle = get_graph_subtitles(weekdays= result$weekday,
                                        segments= result$segment,
                                        hours= result$hour)
         ) +
    xlab('') + ylab("Proportion of cars")

  return(graph)
}


#' Average of v85 car speed per hour over a period, for a segment or a subset of segment.
#'
#' @description
#' v85 is the estimated car speed limit in km/h that 85% of all cars respect.
#' 15% of drivers drive faster than this v85 indicator.
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. Example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param weekday Character vector. Weekday choosen. Default to the all week.
#' @param hours Integer vector. Hours choosen, default to the all day.
#' @param aggregated_by Character. Enables comparison with other segments or weekdays. Options are : 'segment_name', 'weekday', NULL (no comparison, default).
#'
#' @return Graph showing the average of v85 speed per hour.
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr unnest_wider drop_na
#' @importFrom scales percent
#' @importFrom stats weighted.mean
#'
#' @examples
#' library(dplyr)
#' subset_traffic <- traffic %>% filter(day < '2022-02-01', hour > 9)
#' gg_car_speed_histogram(subset_traffic[0:100,])
#' gg_car_speed_histogram(subset_traffic, aggregated_by = 'segment_name')
#' gg_car_speed_histogram(subset_traffic,
#'   weekday = c('monday','sunday'),
#'   segments = 'RteVitre-06',
#'   hours = 17:20,
#'   aggregated_by = "weekday")
gg_car_speed_v85 <- function(enriched_data,
                             date_range = NULL,
                             segments = NULL,
                             weekday = NULL,
                             hours = NULL,
                             aggregated_by = NULL){

  # filtering data and create speed labels
  result <- preprocess_car_speed(enriched_data,
                                 date_range,
                                 segments,
                                 weekday,
                                 hours,
                                 aggregated_by)

  # Aggregation
  if(length(aggregated_by)>1){
    message('Invalid aggregation level: please select only one level.')
    aggregated_by = NULL
  }
  agg_level = check_options_graph(aggregated_by, c('segment_name','weekday'),NULL)

  traffic <- result$data %>%
    group_by_at(c(aggregated_by, 'hour')) %>%
    summarise(v85_avg = weighted.mean(.data$v85, .data$traffic_sum)) %>%
    drop_na()


  # Graph
  aggregated_label <- gsub("[[:punct:]]", " ", aggregated_by)
  if(!is.null(aggregated_by)){
    seg <- unique(result$data$segment_name)
    graph <- ggplot(traffic, aes(x = .data$hour,
                                 y = .data$v85_avg,
                                 color = as.factor(.data[[aggregated_by]]))) +
      scale_color_manual(name = aggregated_label,
                         values = get_custom_palette(seg)[[aggregated_by]])
  } else {
    graph <- ggplot(traffic, aes(x = .data$hour,
                                    y = .data$v85_avg))
  }

  graph <- graph +
    geom_line() +
    labs(title = "Average of v85 (15% of drivers drive faster)",
         subtitle = get_graph_subtitles(weekdays= result$weekday,
                                        segments= result$segment
                                        )) +
    xlab("Hour") +
    ylab("Average v85 for this period") +
    theme_bw() +
    theme(legend.position = "bottom") +
    ylim(0, NA)

  return(graph)

}

#'Preprocessing mandatory for car speed graphs functions
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. Example: c('2021-01-01','2022-01-01').
#' @param segments Character vector. Selected road segment.
#' @param weekday Character vector. Weekday choosen.
#' @param hours Integer vector. Hours choosen.
#' @param aggregated_by Character. Enables comparison with other segments or weekdays. Options are : 'segment_name', 'weekday'.
#'
#' @return list of parameters and data preprocessed.
#' @export
#'
#' @import dplyr
#'
#' @keywords internal
#'
#' @examples
#' preprocess_car_speed(traffic,
#'   date_range = c('2022-01-01','2022-03-01'),
#'   segments = 'RteVitre-06',
#'   weekday = c('monday','sunday'),
#'   hours = 17:20,
#'   aggregated_by = "weekday")
#'
preprocess_car_speed <- function(enriched_data,
                                 aggregated_by,
                                 date_range,
                                 segments,
                                 weekday,
                                 hours
                                 ){

  aggregated_by <- check_options_graph(aggregated_by,
                                       c('segment_name','weekday'),
                                       NULL)

  # filtering data
  result <- filter_agg(enriched_data,
                       date_range = date_range,
                       segments = segments,
                       weekdays = weekday,
                       hours = hours)
  result$data <- result$data %>% filter(.data$mode == 'car',
                                        .data$direction == 'both')

  # generate labels
  result$speed_labels <- paste(seq(0,120,5), '-', c(seq(5,120,5),'120+'), 'km/h')
  names(result$speed_labels) <- paste('car_speed_hist_0to120plus',1:25, sep = "_")

  return(result)
}
