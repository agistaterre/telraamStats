#' Availability and quality of sensors during a period through a heatmap.
#'
#' @description
#' Higher is the uptime average, higher is the quality of data.
#' A null uptime means that the sensor wasn't available during this period.
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. Example: c('2021-01-01','2022-01-01'). Full period if NULL.
#'
#' @return Graph showing availability and quality of sensors over the selected date range.
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr left_join
#' @importFrom tidyr replace_na
#' @importFrom paletteer scale_fill_paletteer_c
#'
#' @examples
#'  gg_availability(traffic)
gg_availability <- function(enriched_data,
                            date_range = NULL){
  if(length(date_range) > 1){
    enriched_data <- enriched_data %>%
      filter(dplyr::between(.data$day, date_range[1], date_range[2]))
  }

  # Compute uptime daily average, removing nighttime and uptime low values
  heatmap_data <- enriched_data %>%
    filter(dplyr::between(.data$hour,5,20)) %>%
    group_by(.data$segment_fullname, .data$day) %>%
    summarise(uptime_avg = mean(.data$uptime_quality))

  # Create a sequence of hours for the period to complete missing days
  period <- seq(min(enriched_data$day), max(enriched_data$day), by="days")
  period_df <- data.frame(
    'day' = rep(period, each=2),
    'segment_fullname' = rep(unique(enriched_data$segment_fullname), length(period))
  )

  # Complete with missing days/hours with an uptime equal to 0
  heatmap_data_full <- left_join(period_df, heatmap_data, by = c('segment_fullname','day')) %>%
    replace_na(list(uptime_avg = 0)) %>%
    mutate(segment_id = as.character(.data$segment_fullname))

  # Heatmap
  graph <- ggplot(heatmap_data_full,
                  aes(x=.data$day, y=.data$segment_fullname, fill=.data$uptime_avg)) +
    geom_tile() +
    scale_fill_paletteer_c("viridis::inferno",
                           name = "Average uptime value for daytime hours (between 5 a.m. and 8 p.m.)") +
    labs(x = "Date", y = "Segment", title = "Data availability by date and segment") +
    theme_bw() +
    theme(legend.position="bottom")

  return(graph)
}




