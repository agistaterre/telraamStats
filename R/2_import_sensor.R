#' Imports data associated with a list of sensors
#'
#' Imports data associated with a given list of sensor names from .RData files contained in a data directory.
#' The main purpose of this function is to load the data saved with write update data.
#'
#' @param list_sensor A character vector specifying the names of sensors to import data for.
#'
#' @return A data.frame containing the imported data.
#'
#' @importFrom purrr map_dfr
#' @importFrom lubridate ymd_hms
#'
#'
#' @export
#'
import_sensor <- function(list_sensor){

  data <- data.frame()
  telraam_segments <- get_segments()
  selected_segments <- telraam_segments[list_sensor]
  data <- map_dfr(selected_segments, ~ {
    file <- paste0('data/', .x, '.RData')
    if (file.exists(file)) {
      # we select the data that we don't consider null (arbitrary choice)
      import <- load(file)
      import <- get(import)
      import <- import %>% filter(.data$uptime > 0.5,
                                           .data$heavy_lft + .data$car_lft + .data$pedestrian_lft + .data$bike_lft +
                                             .data$heavy_rgt + .data$car_rgt + .data$pedestrian_rgt + .data$bike_rgt >0)
      import$car_speed_hist_0to70plus <-  convert_string_to_list(import$car_speed_hist_0to70plus)
      import$car_speed_hist_0to120plus <- convert_string_to_list(import$car_speed_hist_0to120plus)
      import$date <- ymd_hms(import$date)


      import
    } else {
      NULL
    }
  })
  data
}

#' Convert a character string into a numeric vector
#'
#' @param vector Something in the shape "10,20,30"
#'
#' @return Numeric vector. Something in the shape c(10,20,30)
#'
#' @export
#'
#' @examples
#' convert_string_to_list("10,20,30")
#'
#' @keywords internal
#'
convert_string_to_list <- function(vector){
  convert <- as.list(vector)
  lapply(vector, function(x) {
    elements <- unlist(strsplit(x, ",\\s*"))
    numeric_elements <- as.numeric(elements)
    return(numeric_elements)
  })
}
