pkg.globals <- new.env(parent = emptyenv())

#' Function to set up the global variables for public holidays an vacations, with the default
#' being the french dates from a governmental API.
#'
#' @param vacations data frame containing the vacation dates
#' @param public_holidays data frame containing the public holidays dates
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd_hms
#' @importFrom rlang .data
#'
#' @keywords internal
#'
set_global_vars <- function(vacations = NULL, public_holidays = NULL){

  if(is.null(vacations)){
    response <- GET(
      url = "https://data.education.gouv.fr/api/v2/catalog/datasets/fr-en-calendrier-scolaire/exports/json",
      query = list(refine = "location:Rennes",
                   exclude = "population:Enseignants")
    )

    pkg.globals$vacations <- content(response, as = "text") %>%
      fromJSON() %>%
      select("description", "start_date", "end_date") %>%
      mutate(
        start_date = ymd_hms(.data$start_date),
        end_date = ymd_hms(.data$end_date)
      )
  }
  else{
    pkg.globals$vacations <- vacations
  }
  if(is.null(public_holidays)){
    response2 <- GET("https://calendrier.api.gouv.fr/jours-feries/metropole.json")

    pkg.globals$public_holidays <- content(response2, as = "text") %>%
      fromJSON() %>%
      names() %>%
      ymd()
  }
  else{
    pkg.globals$public_holidays <- public_holidays
  }
}


#' Get Telraam segments into a named vector
#'
#' @description
#' Get Telraam segments info in yml file and transform them into a named vector
#'
#' @return Named vector with names and segment IDs, NULL if there is no configuration file
#' @importFrom stats setNames
#'
#' @keywords internal
#'
get_segments <- function(){
  file_path = "inst/config.yml"
  if(!file.exists(file_path)){
    segments <- NULL
  } else {
    segments <- config::get(file = "inst/config.yml")$segments
  }
  return(segments)
}


#' Get the name of a segment giving its id
#'
#' @param segment_id ID of segment, should be present in inst/config.yml
#'
#' @return Name of the segment, as specified in the configuration file, NULL otherwise.
#' @export
#'
#' @keywords internal
#'
get_segment_name <- function(segment_id){
  segments <- get_segments()
  if(is.null(segments)){
    return(NULL)
  }
  if(!segment_id %in% segments){
    message('This ID is unknown. Please update configuration file.')
    return(NULL)
  }
  return(names(segments)[segments==segment_id])
}


#' Filter by selected criteria and aggregating traffics.
#'
#' Not all criteria need to be filled in. Unfilled criteria are set by default so that no filtering is performed.
#'
#' @param data Traffic Data Frame
#' @param date_range Date vector, c("aaaa-mm-jj","aaaa-mm-jj")
#' @param segment Vector of character. Ids of desired segments.
#' @param direction Vector of character. Direction of the street (lft, right, both).
#' @param modes Vector of character. Type(s) of mobility: c("car","heavy","pedestrian","bike")
#' @param weekday Vector of character. Weekday(s) choosen.
#' @param hours Integer vector. Hours choosen, default to the all day.
#'
#' @return the filtered data with a new column "traffic" with aggregated data for specified direction/modes
#'
#' @export
#'
#' @importFrom dplyr filter %>%
#'
#' @keywords internal
#'
filter_agg <- function(data,
                          date_range = NULL,
                          segments = NULL,
                          direction = NULL,
                          modes  = NULL,
                          weekdays = NULL,
                          hours = NULL,
                          uptime_quality = TRUE){

  data$date <- as.POSIXct(data$date,
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "CET")

  data <- data %>% unnest(cols = .data$segment_name)
  data$weekday <- ordered(data$weekday,
                          levels = c('monday','tuesday','wednesday',
                                     'thursday','friday','saturday','sunday'))

  # Parameters check
  transportation_options = c('pedestrian','car','heavy','bike')
  modes = check_options_graph(modes, transportation_options, c('heavy','car'))
  directions_options = c('both','lft','rgt')
  directions = check_options_graph(direction, directions_options, c('both'))
  weekdays_options = c('monday','tuesday','wednesday','thursday','friday','saturday','sunday')
  weekdays = check_options_graph(weekdays, weekdays_options, weekdays_options)
  segments_options = unlist(unique(data$segment_name))
  segments = check_options_graph(segments, segments_options, segments_options)
  hours = check_options_graph(hours, 0:23, 0:23)

  # Filter on parameters
  if(length(date_range) > 1){
    data <- data %>%
      filter(dplyr::between(.data$day, as.Date(date_range[1]), as.Date(date_range[2])))
  }
  data <- melt_direction_mode(data) #Melt dataframe (one row per transportation mode + direction + ids)
  data <- data %>%
      filter(.data$mode %in% modes,
             .data$direction %in% directions,
             .data$segment_name %in% segments,
             .data$weekday %in% weekdays,
             .data$hour %in% hours
             )
  if(uptime_quality){
    data <- data %>% filter(.data$uptime_quality)
  }

  result <- list('data' = data,
                 'segment' = segments,
                 'mode' = modes,
                 'direction' = directions,
                 'weekday' = weekdays,
                 'hour' = hours)
  return(result)
}


#' Indicates if a date is in vacation period and if true, which vacation.
#' If the date is not in a vacation period, "No vacation" is returned.
#'
#' @param date Date (character format)
#' @param vacation Dataframe of vacations, same format as set_globals_vars output.
#'
#' @return Vacation description if the day is between two dates, "No vacation" otherwise.
#' @export
#'
#' @importFrom dplyr between
#'
#' @keywords internal
#'
is_vacation <- function(date, vacation){
  date <- as.POSIXct(date)
  vacation_test <- vacation %>%
    mutate(date = date,
           in_period = between(.data$date, .data$start_date, .data$end_date)) %>%
    filter(.data$in_period)
  if(nrow(vacation_test) > 0){
    vacation <- vacation_test$description
  }
  else {
    vacation <- "No vacation"
  }
  return(vacation)
}

#' Melt dataframe to obtain one row per hour/segment/transportation mode/direction
#' This format makes graphs with ggplot and filtering easier.
#'
#' @param data Traffic Data Frame
#'
#' @return DataFrame with one row per hour/segment/transportation mode/direction
#' @export
#'
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom tidyr unnest separate
#'
#' @keywords internal
#'
melt_direction_mode <- function(data){

  id_cols <- c('date','day','hour','weekday','holiday','vacation','segment_name')
  speed_cols <- c('v85','car_speed_hist_0to70plus','car_speed_hist_0to120plus')
  uptime_cols <- c('uptime','uptime_quality')

  # generate modes names with directions
  modes <- c('pedestrian','bike','heavy','car')
  modes <- c(modes,
             apply(expand.grid(modes, c('lft','rgt')), 1, paste, collapse="_"))

  # melt dataframe and create two new columns : direction and mode
  result <- data %>%
    select(all_of(c(id_cols, speed_cols, uptime_cols, modes))) %>%
    melt(id.vars = c(id_cols, speed_cols, uptime_cols),
         measures.vars = modes,
         value.name = "traffic_sum") %>%
    separate(.data$variable, c('mode','direction'), fill = "right") %>%
    mutate(direction = replace_na(.data$direction, "both"))

  return(result)
}
