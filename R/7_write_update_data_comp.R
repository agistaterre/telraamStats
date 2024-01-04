#' Write or update the sensor data in the data folder
#'
#' This function writes or updates the sensor data in the data folder. It retrieves the data for the specified sensor between \code{start_date} and \code{end_date} (inclusive) using the \code{retrieve_sensor} function, and then converts certain columns to character strings before writing the data to a CSV file in the data folder.
#'
#' @param id_sensor Numeric. ID of the sensor
#' @param start_date Date. Start date "aaaa-mm-jj"
#' @param end_date Date. End date "aaaa-mm-jj"
#' @param vacations vacation periods, set by default on the french ones
#' @param public_holidays public holidays list, set by default on the french ones
#'
#'
#' @importFrom dplyr slice select mutate arrange filter %>%
#' @importFrom stringr str_sub
#'
#' @export
#'
write_update_data_comp <- function(id_sensor, start_date, end_date,
                                   vacations = NULL,
                                   public_holidays = NULL
                                  ){

  set_global_vars(vacations, public_holidays)

  # retrieve file name
  telraam_segments <- get_segments()
  name_segment <- names(telraam_segments[telraam_segments==id_sensor])
  comp_name <- paste(str_sub(name_segment,1,-4), "_comp", str_sub(name_segment,-3,-1), sep = "")
  file_name <- paste0("data/",comp_name,".RData")

  if (!file.exists(file_name)) {
    # If the file doesn't exist, create a new one and save the data
    data <- retrieve_sensor(id_sensor, start_date, end_date)
    data_update <- data
    # conversion from a numeric vector to a character string of car_speed_hist_0to70plus and car_speed_hist_0to120plus
    data_update$car_speed_hist_0to70plus <- sapply(data_update$car_speed_hist_0to70plus, function(x) paste(x, collapse = ", "))
    data_update$car_speed_hist_0to120plus <- sapply(data_update$car_speed_hist_0to120plus, function(x) paste(x, collapse = ", "))
  } else {
    # Preparation of the dataset
    data <- get(load(file_name))


    data_update <- data %>% slice((nrow(data)-805):nrow(data)) %>%
      select(-.data$imputation, -.data$period)
    new_data <- retrieve_sensor(id_sensor,start_date, end_date) %>%
      filter(!is.na(date)) %>%
      mutate(date = as.character(date))

    # conversion from a numeric vector to a character string of car_speed_hist_0to70plus and car_speed_hist_0to120plus
    new_data$car_speed_hist_0to70plus <- sapply(new_data$car_speed_hist_0to70plus, function(x) paste(x, collapse = ", "))
    new_data$car_speed_hist_0to120plus <- sapply(new_data$car_speed_hist_0to120plus, function(x) paste(x, collapse = ", "))

    # data on which we are going to impute
    data_update <- rbind(data_update, new_data)
    data <- data %>% slice(1:(nrow(data)-806))
  }
  # retrieve public holidays
  date_pub_hol <- filter_public_holidays(data_update, "ONLY") %>% select(date)
  date_pub_hol <- date_pub_hol[[1]]

  # retrieve vacations
  date_hol <- filter_vacation(data_update, "ONLY") %>% select(date)
  date_hol <- date_hol[[1]]
  date_hol <- date_hol[!date_hol %in% date_pub_hol]

  # retrieve open days
  date_od <- data_update %>% filter(!date %in% date_pub_hol & !date %in% date_hol) %>% select(date)
  date_od <- date_od[[1]]

  # impute for public_holidays
  data_pub_hol <- imp_na(data_update, "public holidays")
  data_pub_hol <- data_pub_hol %>% filter(date %in% date_pub_hol) %>% mutate(period = "public holidays")

  # impute for vacations
  data_hol <- imp_na(data_update, "holidays")
  data_hol <- data_hol %>% filter(date %in% date_hol) %>% mutate(period = "holidays")

  # impute for open days
  data_od <- imp_na(data_update, "open days")
  data_od <- data_od %>% filter(date %in% date_od) %>% mutate(period = "open days")

  # retrieve complete data for the update
  data_comp <- rbind(data_pub_hol, data_hol, data_od) %>% arrange(date)

  # combine data (old and new)

  if (!file.exists(file_name)) {
    data_comp <- data_comp[!duplicated(data_comp$date),]
    save(data_comp, file = file_name)
  } else {
    data_comp <- rbind(data, data_comp)
    if (!is.null(new_data)) {
      data_comp <- data_comp[!duplicated(data_comp$date),]
    }
    save(data_comp, file = file_name)
  }
}
