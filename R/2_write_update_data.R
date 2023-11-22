source("R/global_utils.R")

#' Write or update the sensor data in the data folder
#'
#' Writes or updates the sensor data in the data folder. It retrieves the data for the specified sensor between \code{date1} and \code{date2} (inclusive) using the \code{retrieve_sensor} function, and then converts certain columns to character strings before writing the data to a RData file in the data folder.
#'
#' @param id_sensor Numeric. ID of the sensor
#' @param date1 Date. Start date "aaaa-mm-jj"
#' @param date2 Date. End date "aaaa-mm-jj"
#'
#' @importFrom lubridate ymd
#'
#' @export
#'
write_update_data <- function(id_sensor, date1, date2){

  # Preparation of the dataset
  data <- retrieve_sensor(id_sensor,date1, date2)
  # conversion from a numeric vector to a character string of car_speed_hist_0to70plus and car_speed_hist_0to120plus
  data$car_speed_hist_0to70plus <- sapply(data$car_speed_hist_0to70plus, function(x) paste(x, collapse = ", "))
  data$car_speed_hist_0to120plus <- sapply(data$car_speed_hist_0to120plus, function(x) paste(x, collapse = ", "))

  telraam_segments <- get_segments()
  name_segment <- names(telraam_segments[telraam_segments==id_sensor])
  file_name <- paste0("data/",name_segment,".RData")

  if (!is.null(data)){
    if (file.exists(file_name)){
      cleaning <- get(load(file_name))
      data <- rbind(cleaning,data)
      data <- data[!duplicated(data$date),] # if some lines are repeated they are eliminated
    }
    save(data, file = file_name)
  }
}
