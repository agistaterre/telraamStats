#' Imports data associated with a list of sensors
#'
#' @description
#' Imports data associated with a given list of sensor names from .RData files contained in a data directory.
#' The main purpose of this function is to load the data saved with write update data.
#'
#' @param list_sensor A character vector specifying the names of sensors to import data for.
#'
#' @return A dataframe containing the imported data.
#'
#' @importFrom purrr map_dfr
#' @importFrom lubridate ymd_hms
#'
#' @export
#'
#' @examples
#' \dontrun{ # This example requires a valid API key
#' period <- as.Date(c('2022-01-01', '2022-12-31'))
#' write_update_data('RteVitre-06', period[1], period[2])
#' write_update_data('ParisArcEnCiel-05', period[1], period[2])
#' import_sensor(c('RteVitre-06', 'ParisArcEnCiel-05'))
#' }
import_sensor <- function(list_sensor){

  data <- data.frame()

  for(sensor in list_sensor){
    if(is.na(get_segments()[sensor])){
      stop(paste(sensor, "doesn't exist in your configuration file"))
    }
  }

  if(dir.exists('data/')){
    folder_path = 'data/'
  } else {
    folder_path = paste(tempdir(), '/', sep = "")
  }

  data <- map_dfr(list_sensor, ~ {
    file <- paste0(folder_path, .x, '.RData')
    if (file.exists(file)) {
      # we select the data that we don't consider null (arbitrary choice)
      import <- load(file)
      import <- get(import)
      import
    } else {
      message(paste('No data stored for', .x))
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
