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

  result <- filtering_agg(enriched_data,
                          date_range = date_range,
                          segments = segments,
                          modes = modes,
                          direction = direction,
                          weekdays = weekday)
  result$data <- result$data %>% unnest(cols = .data$segment_name)
  result$data$weekday <- ordered(result$data$weekday, levels = c("lundi","mardi","mercredi","jeudi",
                                                         "vendredi","samedi","dimanche"))

  # Aggregation
  if(length(aggregated_by)!=1){
    message('Invalid aggregation level: please select only one level.')
    aggregated_by = "weekday"
  }
  agg_level = check_options_graph(aggregated_by, c('segment_name','weekday','direction','mode'),'weekday')

  if(agg_level == 'direction'){
    result$data <- melt_direction_mode(result$data, modes = result$mode, direction = TRUE, agg_mode = TRUE)
  } else if (agg_level == 'mode') {
    result$data <- melt_direction_mode(result$data, modes = result$mode, direction = FALSE, agg_mode = FALSE)
  }

  traffic <- result$data %>%
    group_by_at(c(aggregated_by, 'hour')) %>%
    summarise(traffic_sum = sum(.data$traffic_sum))

  # Graph
  graph <- ggplot(traffic, aes(x = hour,
                               y = .data$traffic_sum,
                               color = as.factor(.data[[aggregated_by]]))) +
    geom_line() +
    labs(title = paste("Average traffic per", aggregated_by),
         subtitle = paste(
           paste("Mode:",paste(result$mode, collapse = ", ")),
           paste("\nDirection:",result$direction),
           paste("\nWeekdays:",paste(result$weekday, collapse = ", ")),
           paste("\nSegments:",paste(result$segment, collapse = ", ")),
           sep = ", ")) +
    xlab("Hour") +
    ylab(paste("Number of", paste(result$mode, collapse = " and "))) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_color_discrete(name = aggregated_by)


  return(graph)
}

#'Melt traffic dataframe to create mode and transportation columns to help graphic representation
#'
#'
#' @param data enriched data.frame containing all the data for all your sensors
#' @param modes Character vector. Different modes of transportation aggregated (heavy, car, bike, pedestrian) . Default: heavy & car
#' @param direction Boolean. Default to TRUE : direction is mandatory for all modes.
#' @param agg_mode Boolean. Should all modes be aggregated ? Default to FALSE.
#'
#' @return Molten DataFrame with one or two new columns : mode + transportation if required.
#' @export
#'
#' @import dplyr
#' @import reshape2
#' @importFrom tidyr unnest
#'
#' @keywords internal
#'
melt_direction_mode <- function(data, modes, direction = TRUE, agg_mode = FALSE){

  id_cols <- c('date','day','hour','weekday','segment_name')

  if(direction){
    modes <- apply(expand.grid(modes, c('lft','rgt')), 1, paste, collapse="_")
  }

  result <- data %>%
    unnest(cols = .data$segment_name) %>%
    select(all_of(c(id_cols, modes))) %>%
    melt(id.vars = id_cols,
         measures.vars = modes,
         value.name = "traffic_sum")

  if(direction){
    result <- result %>%
      separate(.data$variable, c('mode','direction'))
  } else {
    result <- result %>%
      rename(mode = .data$variable)
  }
  if(agg_mode){
    result <- result %>%
      group_by_at(c(id_cols ,'direction')) %>%
      summarise('traffic_sum' = sum(.data$traffic_sum))
  }

  return(result)
}
