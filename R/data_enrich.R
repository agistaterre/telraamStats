#' Enrich traffic data with date features, names and uptime filters.
#'
#' @description This function add day, weekday, hour, segment_name and full_name and uptime quality boolean.
#' This function is already used by the data retrievement process.
#'
#' @param data Raw data frame from the Telraam API, imported through the package.
#'
#' @return Same dataframe with additionnal informations : day, hour, weekday, holiday, vacation, segment_name, uptime_quality
#' @export
#'
#' @keywords internal
#'
#' @examples
#' enriched_traffic <- enrich_traffic(traffic[0:10,])
#' setdiff(colnames(enriched_traffic[0:10,]), colnames(traffic[0:10,]))
#'
enrich_traffic <- function(data){
  enriched_data <- enrich_dates(data)
  enriched_data <- enrich_special_days(enriched_data)
  enriched_data <- enrich_name(enriched_data)
  enriched_data <- enrich_uptime(enriched_data)
  return(enriched_data)
}


#' Enrich traffic data with date informations.
#'
#' @param data Data frame containing a `date` character column containing date + hour + timezone.
#'
#' @return Same dataframe with 3 additionnal columns : day, weekday and hour.
#' @export
#'
#' @importFrom lubridate hour
#'
#' @keywords internal
#'
#' @examples
#' df <- data.frame('date' = c("2022-02-18 08:00:00 CET","2022-02-18 09:00:00 CET"),
#'   stringsAsFactors = FALSE)
#' enrich_dates(df)
#'
enrich_dates <- function(data){

  # weekday labels
  weekday_labels <- c('monday','tuesday','wednesday',
                      'thursday','friday','saturday','sunday')
  names(weekday_labels) <- seq(1,7)

  enriched_data <- data %>%
    mutate(
      'day' = as.Date(.data$date),
      'hour' = hour(.data$date),
      'weekday' = strftime(.data$day,'%u')
    ) %>%
    mutate('weekday' = weekday_labels[.data$weekday])
  return(enriched_data)
}


#' Enrich traffic data with segment name
#'
#' @description
#' `segment_fullname` is also added : it's the combination of segment's id and name.
#' `segment_id` should be in the configuration file.
#'
#' @param data Data frame containing a `segment_id` column
#'
#'
#' @return Same dataframe with two additionnal columns : `segment_name` and `segment_fullname`
#' @export
#'
#' @importFrom tidyr unite
#'
#' @keywords internal
#'
#' @examples
#' df <- data.frame('segment_id' = c(9000002156, 9000001906))
#' enrich_name(df)
#'
enrich_name <- function(data){

  enriched_data <- data %>%
    mutate(segment_name = lapply(.data$segment_id, get_segment_name)) %>%
    unite("segment_fullname", .data$segment_id, .data$segment_name, sep = ' - ', remove = FALSE)
  return(enriched_data)
}


#' Enrich traffic data with uptime quality indication
#'
#' @description
#' If the uptime is lower than 0.5, `uptime_quality` will be FALSE, else TRUE
#'
#' @param data Data frame containing an `uptime` column
#'
#' @return Same dataframe with an additionnal column indicating
#' if the uptime is greater (TRUE) or lower (FALSE) than 0.5.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' df <- data.frame('uptime' = c(0.05, 0.95))
#' enrich_uptime(df)
#'
enrich_uptime <- function(data){
  enriched_data <- data %>%
    mutate(uptime_quality = (.data$uptime >= 0.5))
  return(enriched_data)
}


#' Enrich traffic data with french vacation and public holidays
#'
#' @param data Data frame containing a `day` and a `date`(day + hour + timezone) column
#' @param vacations Data frame containing the vacation dates
#' @param public_holidays Data frame containing the public holidays dates
#'
#' @return Same dataframe with two additionnal columns :
#' - holiday, boolean: TRUE if the day corresponds to public holiday, FALSE otherwise
#' - vacation, indicating the french vacation, "No vacation" otherwise.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' df <- data.frame('day' = as.Date(c("2022-02-18","2022-01-01")),
#'   'date' = c('2022-02-18 12:00:00 CET','2022-01-01 12:00:00 CET'))
#' enrich_special_days(df)
#'
enrich_special_days <- function(data, vacations = NULL, public_holidays = NULL){
  set_global_vars(vacations, public_holidays)
  enriched_data <- data %>%
    mutate(holiday = (.data$day %in% pkg.globals$public_holidays),
           vacation = lapply(.data$date, function(x){is_vacation(x, pkg.globals$vacations)}))
  return(enriched_data)
}
