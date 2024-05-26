#' Retrieves data associated with a sensor from the Telraam API
#'
#' @description
#' Retrieves data associated with a sensor from the Telraam API.
#' The data is retrieved for a specified time period between \code{start_date} and \code{end_date} (inclusive).
#'
#' @param segment_name Character. Name of the segment, as specified in config.
#' @param start_date Date. Start date "aaaa-mm-jj", must be of the date type.
#' @param end_date Date. End date "aaaa-mm-jj", must be of the date type.
#' @param key the API key (set by the `set_telraam_token()` function)
#'
#' @return Dataframe from Telraam API, enriched with `enrich_traffic()` function.
#'
#' @importFrom lubridate ymd_hms days
#' @importFrom purrr pmap
#' @importFrom httr POST add_headers
#' @importFrom dplyr filter bind_rows %>%
#' @export
#'
#' @examples
#' \dontrun{ # This function requires a valid API key
#' period <- as.Date(c('2022-01-01', '2022-12-31'))
#' retrieve_sensor('RteVitre-06', period[1], period[2])
#' }
#'
retrieve_sensor <- function(segment_name,start_date,end_date, key = get_telraam_token()){

  result <- data.frame()
  end_date <- end_date + days(1) # so that end_date is included
  dates <- seq_by_3_month(start_date,end_date) # for the iteration of the retrieving, because when we call the API, the period can not exceed 3 month for each call

  # Get Segment_id
  if(!is.numeric(segment_name)){
    segment_id <- get_segments()[segment_name]
    if(is.na(names(segment_id))){
      stop("Segment name unknown")
    }
  }
  else{
    segment_id <- segment_name
  }

  # calling of the API
  config_file = get_config_path()
  traffic_url <- paste(config::get(file = config_file)$url,
                       '/reports/traffic', sep='')
  resTraffic_list <- pmap(list(dates$start, dates$end), ~ {
    resTraffic <- POST(traffic_url,
                       add_headers("X-Api-Key" = key),
                       body = paste0('{
                       "level": "segments",
                       "format": "per-hour",
                       "id": "', segment_id, '",
                       "time_start": "', ..1, '",
                       "time_end": "', ..2, '"
                     }'))

    content <- resTraffic$content %>%
      rawToChar() %>%
      fromJSON()
    df <- content$report
    if (length(df)>0){
      df$date <- format(ymd_hms(df$date, tz = df$timezone[1]), "%Y-%m-%d %H:%M:%S %Z")
      df <- enrich_traffic(df)
    }
    df
  })

  result <- bind_rows(resTraffic_list)

  if (length(result) > 0){ # in case the download is empty
    result$date <- format(ymd_hms(result$date, tz = result$timezone[1]),"%Y-%m-%d %H:%M:%S %Z") # We change the class of date with a time difference of 2
  } else {
    message("There is no data for the selected period.")
  }
  return(result)
}


#' Generate sequence of intervals with three-month periods
#'
#' @description
#' This function is used internally in the \code{retrieve_sensor} function to generate a sequence of intervals with three-month periods. It takes a start date (\code{start_date}) and an end date (\code{end_date}), and returns a data frame with two columns representing the start and end dates of each interval.
#'
#' @param start_date Date. Start date in "yyyy-mm-dd" format.
#' @param end_date Date. End date in "yyyy-mm-dd" format.
#'
#' @return Dataframe with a `start` and an `end` columns, each row represents 3 months period.
#'
#' @importFrom lubridate ymd
#' @export
#'
#' @keywords internal
#'
#' @examples
#' seq_by_3_month(as.Date('2023-11-01'),as.Date('2024-11-01'))
#'
seq_by_3_month <- function(start_date, end_date){
  if (start_date==end_date){
    return(data.frame(start = start_date, end = start_date))
  }else{
    date <- seq(from = start_date, to = end_date, by = "3 month")
    if (date[length(date)]!=end_date){
      date <- c(date,end_date)
    }
    return(data.frame(start = date[1:(length(date)-1)],
                      end   = date[2:length(date)]))
  }
}
