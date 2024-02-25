#' Enrich traffic data with date features, names and uptime filters.
#' This function add day, weekday, hour, segment_name and full_name and uptime quality boolean.
#'
#' @param data Raw data frame from the Telraam API, imported through the package.
#'
#'
#' @return Same dataframe with additionnal informations.
#' @export
#'
#' @keywords internal
#'
enrich_traffic <- function(data){
  enriched_data <- enrich_dates(data)
  enriched_data <- enrich_special_days(enriched_data)
  enriched_data <- enrich_name(enriched_data)
  enriched_data <- enrich_uptime(enriched_data)
  return(enriched_data)
}


#' Enrich traffic data with date informations
#'
#' @param data Data frame containing the date column
#'
#'
#' @return Same dataframe with 3 additionnal columns : day, weekday and hour
#' @export
#'
#' @keywords internal
#'
enrich_dates <- function(data){
  enriched_data <- data %>%
    mutate(
      day = as.Date(.data$date),
      hour = hour(.data$date),
      weekday = strftime(.data$day,'%A')
    )
  return(enriched_data)
}


#' Enrich traffic data with segment name
#' Segment fullname is also added : it's the combination of segment's id and name.
#'
#' @param data Data frame containing a segment_id column
#'
#'
#' @return Same dataframe with two additionnal columns : segment name and full name
#' @export
#'
#' @keywords internal
#'
enrich_name <- function(data){
  enriched_data <- data %>%
    mutate(segment_name = lapply(.data$segment_id, get_segment_name)) %>%
    tidyr::unite("segment_fullname", .data$segment_id, .data$segment_name, sep = ' - ', remove = FALSE)
  return(enriched_data)
}


#' Enrich traffic data with uptime quality indication
#' If the uptime is lower than 0.5, uptime_quality will be FALSE, else TRUE
#'
#' @param data Data frame containing a uptime column
#'
#'
#' @return Same dataframe with an additionnal column indicating if the uptime is greater or lower than 0.5.
#' @export
#'
#' @keywords internal
#'
enrich_uptime <- function(data){
  enriched_data <- data %>%
    mutate(uptime_quality = (.data$uptime >= 0.5))
  return(enriched_data)
}


#' Enrich traffic data with french vacation and public holidays
#'
#' @param data Data frame containing a uptime column
#' @param vacations data frame containing the vacation dates
#' @param public_holidays data frame containing the public holidays dates
#'
#' @return Same dataframe with two additionnal boolean columns (TRUE if the day corresponds to vacation/public holiday)
#' @export
#'
#' @keywords internal
#'
enrich_special_days <- function(data, vacations = NULL, public_holidays = NULL){
  set_global_vars(vacations, public_holidays)
  enriched_data <- data %>%
    mutate(holiday = (.data$day %in% pkg.globals$public_holidays),
           vacation = lapply(.data$date, function(x){which_vacations(x, pkg.globals$vacations)}))
  return(enriched_data)
}
