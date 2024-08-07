#' Clean data from inactivity periods and missing hours
#'
#' This function cleans the data by removing nighttime hours and periods of inactivity.
#' It combines the functionalities of `retrieve_missing_hours` and `replace_inactivity_period`.
#'
#' @param data data.frame containing all the data for all your sensors
#' @param date_range Date vector. Example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segment_id Character vector. Selected road segment, all if NULL (default).
#' @param threshold_uptime Numeric. Uptime threshold chosen. Default is 0.5.
#' @param successive_day Integer. Number of days chosen to define an inactivity period. Default is 2.
#' @param remove_data Logical. If TRUE, completely removes inactivity periods. If FALSE, replaces data with NA.
#' @param show_graph Logical. If TRUE, displays a graph of inactivity periods. Default is TRUE.
#' @return Cleaned data
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' traffic_cleaned <- retrieve_missing_data(traffic)
#' traffic_cleaned <- retrieve_missing_data(traffic,
#'                                          date_range = c('2022-07-01','2022-09-01'),
#'                                          segment_id = 9000001844,
#'                                          threshold_uptime = 0.3,
#'                                          successive_day = 1,
#'                                          remove_data = FALSE,
#'                                          show_graph = TRUE)

retrieve_missing_data <- function(data,
                                  date_range = NULL,
                                  segment_id = NULL,
                                  threshold_uptime = 0.5,
                                  successive_day = 2,
                                  remove_data = TRUE,
                                  show_graph = TRUE) {

  # Convert date column to datetime format
  data$date <- ymd_hms(data$date)

  # Filter data by segment_id if provided
  if (!is.null(segment_id)) {
    data <- data[data$segment_id %in% segment_id,]
  }

  # Filter data by date range if provided
  if (!is.null(date_range)) {
    data <- data[data$day >= date_range[1] & data$day <= date_range[2],]
  }

  # Check if there's data after filtering
  if (nrow(data) == 0) {
    stop("No data in the selected period")
  } else {
    # Remove inactivity periods
    data_without_inactivity_period <- replace_inactivity_period(data, successive_day, threshold_uptime, remove_data, show_graph)

    # Remove hours with no data
    data_clean <- retrieve_missing_hours(data_without_inactivity_period, threshold_uptime)
  }

  return(data_clean)
}

#' Remove nighttime hours from traffic data
#'
#' This function removes nighttime hours when sensors cannot capture information.
#' It filters the data to retain only the hours with reliable uptime.
#'
#' @param data data.frame containing all the data for all your sensors
#' @param threshold_uptime Numeric. Uptime threshold chosen. Default is 0.5.
#'
#' @return data without nighttime hours
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' traffic_without_nighttime <- retrieve_missing_hours(traffic)
#' traffic_without_nighttime <- retrieve_missing_hours(traffic, threshold_uptime = 0.3)

retrieve_missing_hours <- function(data, threshold_uptime = 0.5) {

  # Convert date column to datetime format
  data$date <- ymd_hms(data$date)

  # Determine season based on date
  data <- data %>%
    mutate(season = case_when(
      (month(.data$date) == 3 & day(.data$date) >= 20) | (month(.data$date) == 6 & day(.data$date) < 21) | (month(.data$date) %in% 4:5) ~ "Spring",
      (month(.data$date) == 6 & day(.data$date) >= 21) | (month(.data$date) == 9 & day(.data$date) < 23) | (month(.data$date) %in% 7:8) ~ "Summer",
      (month(.data$date) == 9 & day(.data$date) >= 23) | (month(.data$date) == 12 & day(.data$date) < 21) | (month(.data$date) %in% 10:11) ~ "Autumn",
      TRUE ~ "Winter"
    ))

  # Calculate condition for each segment, season, and hour
  df_season <- data %>%
    group_by(.data$segment_id, .data$season, .data$hour) %>%
    summarise(
      condition = mean(.data$car == 0 & .data$uptime <= threshold_uptime) <= 0.05,
      .groups = 'drop'
    )

  # Filter data based on the calculated condition
  data <- data %>%
    semi_join(df_season %>% filter(.data$condition), by = c("segment_id", "season", "hour"))

  return(data)
}

#' Replace or remove inactivity periods in traffic data
#'
#' This function identifies periods of sensor inactivity and either replaces them with NA or removes them.
#' An inactivity period is defined as a consecutive period where the uptime is below the chosen threshold
#' or if no vehicles are counted during this period.
#'
#' @param data data.frame containing all the data for all your sensors
#' @param successive_day Integer. Number of consecutive days to define an inactivity period. Default is 2.
#' @param threshold_uptime Numeric. Uptime threshold chosen. Default is 0.5.
#' @param remove_data Logical. If TRUE, removes inactivity periods. If FALSE, replaces data with NA.
#' @param show_graph Logical. If TRUE, displays a graph of inactivity periods. Default is TRUE.
#'
#' @return data with inactivity periods replaced or removed
#' @export
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#'
#' @examples
#' traffic_without_inactivity <- replace_inactivity_period(traffic)
#' traffic_without_inactivity <- replace_inactivity_period(traffic,
#'                                                         threshold_uptime = 0.3,
#'                                                         successive_day = 1,
#'                                                         remove_data = FALSE,
#'                                                         show_graph = TRUE)

replace_inactivity_period <- function(data, successive_day = 2, threshold_uptime = 0.5, remove_data = TRUE, show_graph = TRUE) {

  # Convert date column to datetime format
  data$date <- ymd_hms(data$date)

  # Function to identify inactive periods
  identify_inactive_periods <- function(df) {
    df <- df %>%
      mutate(
        is_inactive = (.data$uptime < threshold_uptime) | (.data$car == 0 & .data$uptime >= threshold_uptime),
        inactive_group = cumsum(c(0, diff(as.numeric(.data$is_inactive)) != 0))
      ) %>%
      group_by(.data$inactive_group) %>%
      mutate(
        period_start = first(.data$date),
        period_end = last(.data$date),
        period_length = as.numeric(difftime(.data$period_end, .data$period_start, units = "days"))
      ) %>%
      ungroup()

    return(df)
  }

  # Process the data
  processed_data <- data %>%
    group_by(.data$segment_id) %>%
    group_modify(~identify_inactive_periods(.x)) %>%
    ungroup() %>%
    mutate(should_remove = .data$is_inactive & .data$period_length >= successive_day)

  # Columns to process
  columns_to_process <- c("heavy", "car", "bike", "pedestrian",
                          "heavy_lft", "heavy_rgt", "car_lft", "car_rgt",
                          "bike_lft", "bike_rgt", "pedestrian_lft", "pedestrian_rgt")

  # Apply removal or replacement with NA
  if (remove_data) {
    result <- processed_data %>%
      filter(!.data$should_remove) %>%
      select(-"is_inactive", -"inactive_group", -"period_start", -"period_end", -"period_length", -"should_remove")
  } else {
    result <- processed_data %>%
      mutate(across(all_of(columns_to_process),
                    ~if_else(.data$should_remove, NA_real_, .))) %>%
      select(-"is_inactive", -"inactive_group", -"period_start", -"period_end", -"period_length", -"should_remove")

  }

  # Display graph if show_graph is TRUE
  if (show_graph) {
    graph_data <- processed_data %>%
      filter(.data$should_remove) %>%
      select(.data$segment_fullname, .data$period_start, .data$period_end, .data$period_length) %>%
      mutate(segment_fullname = as.factor(.data$segment_fullname))

    p <- ggplot(graph_data, aes(x = .data$period_start, xend = .data$period_end, y = .data$segment_fullname, yend = .data$segment_fullname)) +
      geom_segment(color = "red", size = 2) +
      labs(title = "Inactivity periods to be removed",
           x = "Date",
           y = "Segment")

    # Display the graph
    print(p)
  }

  return(result)
}

