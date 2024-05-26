#' Write or update the sensor data in the data folder
#'
#' Writes or updates the sensor data in the data folder.
#' It retrieves the data for the specified sensor between \code{start_date}
#' and \code{end_date} (inclusive) using the \code{retrieve_sensor} function,
#' and then converts certain columns to character strings before writing the data
#' to a RData file in the `data` folder (if `create_directory = TRUE`),
#' to a temporary folder otherwise.
#'
#' @param segment_name Character. Name of the segment, as specified in config.
#' @param start_date Date. Start date "aaaa-mm-jj"
#' @param end_date Date. End date "aaaa-mm-jj"
#' @param create_directory Boolean: Does the file need to be created in the project directory? Default to FALSE.
#'
#' @return Boolean: TRUE if the data is well saved/written, FALSE otherwise (no data for example)
#'
#' @importFrom lubridate ymd
#'
#' @export
#'
#' @examples
#' \dontrun{ # This function requires a valid API key
#' period <- as.Date(c('2022-01-01', '2022-12-31'))
#' write_update_data('RteVitre-06', period[1], period[2])
#' }
#'
write_update_data <- function(segment_name, start_date, end_date, create_directory = FALSE){

  # Preparation of the dataset
  data <- retrieve_sensor(segment_name,start_date, end_date)

  # Handle the case where there is no data
  if(nrow(data) == 0){
    message("No data for this period")
    result = FALSE
  }

  # conversion from a numeric vector to a character string of car_speed_hist_0to70plus and car_speed_hist_0to120plus
  data$car_speed_hist_0to70plus <- sapply(data$car_speed_hist_0to70plus, function(x) paste(x, collapse = ", "))
  data$car_speed_hist_0to120plus <- sapply(data$car_speed_hist_0to120plus, function(x) paste(x, collapse = ", "))



  if (!is.null(data)){
    if(!create_directory){
      folder_path = paste(tempdir(), "/", sep = "")
    } else if(!dir.exists("data/")){
      dir.create("data/")
      folder_path = "data/"
    }
    file_name <- paste0(folder_path, segment_name, ".RData")
    if (file.exists(file_name)){
      cleaning <- get(load(file_name))
      data <- rbind(cleaning,data)
      data <- data[!duplicated(data$date),] # if some lines are repeated they are eliminated
    }
    save(data, file = file_name)
    result = TRUE
  }
  else {
    result = FALSE
  }
  return(result)
}
