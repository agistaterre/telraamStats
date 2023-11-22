source("R/global_utils.R")

#' gg_na_heatmap
#' This function generates a heatmap representation of the proportion of missing values (NA) for different sensors over monthly periods during a selected time frame.
#'
#' @param data Data frame. A data frame containing road traffic data.
#' @param start Character. The start date in the format "yyyy-mm-dd".
#' @param end Character. The end date in the format "yyyy-mm-dd".
#' @param hours Vector. A vector containing hours on which you want to see the representation, e.g., c("05h", "18h").
#' @param list_weekday Vector. A vector containing weekdays in French, e.g., c("Monday", "Sunday").
#' @param pub_holidays Character. "YES" if you want to see results with public holidays included; "NO" if you want to exclude them from the representation; "ONLY" if you only want to see them in the result.
#' @param holidays Character. "YES" if you want to see results including holidays; "NO" if you want to exclude them from the representation; "ONLY" if you only want to see them in the result.
#'
#' @import ggplot2
#' @import lubridate
#' @importFrom dplyr summarize %>% mutate filter group_by right_join n
#' @importFrom stringr str_sub
#'
#' @return A ggplot graph representing the heatmap of NA proportions for different sensors over monthly periods.
#'
#' @export
#'
gg_na_heatmap <- function(data,
                          start = NULL,
                          end = NULL,
                          hours = "ALL",
                          list_weekday = "ALL",
                          pub_holidays = "YES",
                          holidays = "YES"){
  if (is.null(start)){
    start <- str_sub(min(data$date), 1, 10)
  }
  if (is.null(end)){
    end <- str_sub(max(data$date), 1, 10)
  }

  # create a month-year column
  data <- data %>% mutate(month = str_sub(date, 1, 7))
  labels_legend <- data %>% select(month) %>% distinct()


  # retreive all sensors id
  segment_id <- data$segment_id
  list_sensor <- distinct(data, segment_id)$segment_id

  # filter by date
  data <- data %>% filter(date >= start, date <= end)

  # filter by hours
  if (length(hours) == 1){
    if(hours != "ALL"){
      data <- data %>% filter(endsWith(str_sub(date, 12, 19), hours))
    }
  }else{
    data <- data %>% filter(str_sub(date, 12, 19) %in% hours)
  }

  # filter by weekdays
  if (!setequal(list_weekday, rep("ALL", length(list_weekday)))){

    data <- data %>%
      filter(weekdays(ymd_hms(date)) %in% list_weekday) %>%
      mutate(date = as.character(date))

  }else{
    data <- data %>% mutate(date = as.character(date))
  }



  # filter pub holidays
  if (pub_holidays == "YES"){

    data <- data

  }else if(pub_holidays == "NO"){

    data <- data %>% filter(period != "public holidays")

  }else if(pub_holidays == "ONLY"){

    data <- data %>% filter(period == "public holidays")

  }

  # filter holidays

  if (holidays == "YES"){

    data <- data

  }else if(holidays == "NO"){

    data <- data %>% filter(period != "holidays")

  }else if(holidays == "ONLY"){

    data <- data %>% filter(period == "holidays")

  }

  # summarize proportion of NA by month by sensor
  telraam_segments <- get_segments()
  result <- data.frame()
  for (sensor_id in list_sensor){
    segment_name = names(telraam_segments[telraam_segments==id_sensor])
    res <- data %>% filter(segment_id == sensor_id) %>%
      group_by(month) %>%
      summarize("prop_NA" = sum(is.na(.data$car))*100/n())
    res <- res %>%
      right_join(labels_legend, by = c("month" = "month")) %>%
      mutate(segment_id = as.factor(segment_name)) %>%
      mutate("prop_NA" = ifelse(is.na(.data$prop_NA), 100, .data$prop_NA))
    result <- rbind(result,res)
  }

  labels_legend <- labels_legend %>%
    mutate(month = ifelse(str_sub(month,6,8) != "06",str_sub(month,6,8), paste("06", "\n", str_sub(month,1,4))))
  labels_legend <- labels_legend$month

  ggplot(result, aes(month, segment_id, fill = .data$prop_NA)) +
    geom_tile(col ="grey") +
    scale_fill_gradient(low="chartreuse2",high= "firebrick2") +
    labs(fill = "Proportion de \nvaleurs manquantes") +
    scale_x_discrete(name = "Mois",
                     labels = labels_legend) +
    ylab("Capteurs")

}
