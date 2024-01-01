#' Retrieves data associated with a sensor from the Telraam API
#'
#' Retrieves data associated with a sensor from the Telraam API.
#' The data is retrieved for a specified time period between \code{date1} and \code{date2} (inclusive).
#'
#' @param id_sensor Numeric. ID of the sensor
#' @param date1 Date. Start date "aaaa-mm-jj", must be of the date type.
#' @param date2 Date. End date "aaaa-mm-jj", must be of the date type.
#' @param key the api key (set by the set_telraam_token function)
#'
#'
#' @importFrom lubridate ymd_hms days
#' @importFrom purrr pmap
#' @importFrom httr POST add_headers
#' @importFrom dplyr filter bind_rows %>%
#' @export
#'
#'
retrieve_sensor <- function(id_sensor,date1,date2, key = get_telraam_token()){

  result <- data.frame()
  date2 <- date2 + days(1) # so that date2 is included
  dates <- seq_by_3_month(date1,date2) # for the iteration of the retrieving, because when we call the API, the period can not exceed 3 month for each call

  # calling of the API
  traffic_url <- paste(config::get(file = "inst/config.yml")$url,
                       '/reports/traffic', sep='')
  resTraffic_list <- pmap(list(dates$start, dates$end), ~ {
    resTraffic <- POST(traffic_url,
                       add_headers("X-Api-Key" = key),
                       body = paste0('{
                       "level": "segments",
                       "format": "per-hour",
                       "id": "', id_sensor, '",
                       "time_start": "', ..1, '",
                       "time_end": "', ..2, '"
                     }'))

    content <- resTraffic$content %>%
      rawToChar() %>%
      fromJSON()
    df <- content$report
    df$date <- ymd_hms(df$date, tz = df$timezone[1])
    df
  })

  result <- bind_rows(resTraffic_list)

  if (length(result$date)!=0){ # in case the download is empty
    result$date <- ymd_hms(result$date, tz = result$timezone[1]) # We change the class of date with a time difference of 2
  }
  return(result)
}


#' Generate sequence of intervals with three-month periods
#'
#' This function is used internally in the \code{retrieve_sensor} function to generate a sequence of intervals with three-month periods. It takes a start date (\code{date1}) and an end date (\code{date2}), and returns a data frame with two columns representing the start and end dates of each interval.
#'
#' @param date1 Date. Start date in "yyyy-mm-dd" format.
#' @param date2 Date. End date in "yyyy-mm-dd" format.
#'
#' @importFrom lubridate ymd
#'
#' @keywords internal
#'
seq_by_3_month <- function(date1, date2){
  if (date1==date2){
    return(data.frame(start = date1, end = date1))
  }else{
    date <- seq(from = date1, to = date2, by = "3 month")
    if (date[length(date)]!=date2){
      date <- c(date,date2)
    }
    return(data.frame(start = date[1:(length(date)-1)],
                      end   = date[2:length(date)]))
  }
}
