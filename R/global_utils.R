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
#'@keywords internal
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
#' @return Named vector with names and segment IDs
#' @importFrom stats setNames
#'
#' @keywords internal
#'
get_segments <- function(){
  segments <- config::get(file = "inst/config.yml")$segments
  return(segments)
}


#' Get the name of a segment giving its id
#'
#' @param segment_id ID of segment, should be present in inst/config.yml
#'
#'
#' @return Name of the segment, as specified in the configuration file
#' @export
#'
#' @keywords internal
#'
get_segment_name <- function(segment_id){
  segments <- get_segments()
  if(!segment_id %in% segments){
    stop('This ID is unknown. Please update configuration file.')
  }
  return(names(segments)[segments==segment_id])
}


#' Select data within a specified date range
#'
#' @param data Data frame containing the date column
#' @param date_range A vector of two dates specifying the range
#'
#' @return Subset of the input data frame within the specified date range
#' @export
#' @importFrom lubridate ymd
#'
#' @keywords internal
#'
filter_date <- function(data, date_range){
  # if dates are exchanged
  start_date <- ymd(date_range[1])
  end_date <- ymd(date_range[2])
  if (start_date<end_date){
    start <- start_date
    end <- end_date
  } else {
    start <- end_date
    end <- start_date
  }
  # Filter according to dates
  matching_dates <- (data$date>=start & data$date<=end)
  return(data[matching_dates,])
}


#' Select data based on vacation criteria
#'
#' @param data Data frame containing the date column
#' @param vacation A character indicating the vacation selection criteria
#'
#'
#' @return Subset of the input data frame based on the vacation criteria
#' @export
#'
#'@keywords internal
#'
filter_vacation <- function(data, vacation){
  if (vacation=='YES'){
    return(data)
  }

  # Building of the filter
  matching_dates <- rep(FALSE,nrow(data))
  for (i in 1:nrow(pkg.globals$vacations)){
    temp <- (data$date>=pkg.globals$vacations$start_date[i] & data$date<=pkg.globals$vacations$end_date[i])
    matching_dates <- matching_dates + temp
  }
  matching_dates <- (matching_dates>0)

  if(vacation=="NO"){
    return(data[!matching_dates,])
  }
  if(vacation=="ONLY"){
    return(data[matching_dates,])
  }
}


#' Select data based on public holiday criteria
#'
#' @param data Data frame containing the date column
#' @param JF A character indicating the public holiday selection criteria
#'
#'
#' @return Subset of the input data frame based on the public holiday criteria
#' @export
#'
#' @keywords internal
#'
filter_public_holidays <- function(data, JF){
  if (JF=='YES'){
    return(data)
  }
  d <- substr(as.character(data$date),1,10)
  matching_dates <- rep(0,nrow(data))
  chara <- as.character(pkg.globals$public_holidays)
  for (ferie in chara){
    matching_dates <- matching_dates + (ferie==d)
  }
  matching_dates <- (matching_dates>0)

  if(JF=="NO"){
    return(data[!matching_dates,])
  } else if(JF=="ONLY"){
    return(data[matching_dates,])
  }
}


#' Filter by selected criteria.
#'
#' Not all criteria need to be filled in. Unfilled criteria are set by default so that no filtering is performed.
#'
#' @param data dat.frame. See the "importation" function in the '2-import.R' file
#' @param sensor character. Id of desired sensor between quotations
#' @param direction character. Direction of the street: " " or "_lft" or "_rgt"
#' @param mobility character. Type of mobility: c("car","heavy","pedestrian","bike")
#' @param date_range Date vector, c("aaaa-mm-jj","aaaa-mm-jj")
#' @param vac character. With, without, or only with vacation: "YES" or "NO" or "ONLY"
#' @param p_h character. With, without, or only with public holiday: "YES" or "NO" or "ONLY"
#' @param wkd character vector. Selected days of the week: c("1","2","3","4","5","6","7") starting with one for Monday. Make sure to use options(lubridate.week.start = 1) for this function to perform correctly.
#' @param vacations data.frame, containing 3 columns : desctiption, start_date and end_date
#' - the description column contains character vectors with the name of every vacations you are studying
#' - starting_date and ending_date are Date-Time vectors(POSIXct) in the format "year-month-day hour:minute:second", obtainable for example using ymd_hms from the lubridate package
#' @param public_holidays date vector, containing the public holidays dates in a "Year-month-day" format.
#'
#'
#' @return the filtered data
#'
#' @export
#'
#' @importFrom dplyr filter %>%
#' @importFrom lubridate wday
#' @importFrom purrr is_empty
#' @importFrom tibble tibble
#'
filtering <- function(data = NULL, sensor    = NULL, direction = ' ', mobility  = c("car","heavy","pedestrian","bike"),
                      date_range = NULL, vac  = NULL, p_h = NULL, wkd  = NULL, vacations = NULL, public_holidays = NULL
){

  set_global_vars(vacations, public_holidays)

  if (is_empty(data)|is.null(data)|is.null(sensor)|is.null(mobility)|is.null(direction)){
    return(tibble())
  }
  filtre <- data[ data$segment_id == sensor, ]
  S <- trimws(paste0(mobility,direction))
  filtre$total <- apply(filtre[,S], MARGIN = 1 ,FUN = sum)
  if (!is.null(wkd)){
    filtre <- filtre %>% filter(wday(date) %in% wkd)
  }
  if (!is.null(date_range)){
    filtre <- filtre %>% filter_date(date_range)
  }
  if (!is.null(vac)){
    filtre <- filtre %>% filter_vacation(vac)
  }
  if (!is.null(p_h)){
    filtre <- filtre %>% filter_public_holidays(p_h)
  }
  return(filtre)
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
#'
#' @return the filtered data with a new column "traffic" with aggregated data for specified direction/modes
#'
#' @export
#'
#' @importFrom dplyr filter %>%
#'
#' @keywords internal
#'
filtering_agg <- function(data,
                          date_range = NULL,
                          segments = NULL,
                          direction = NULL,
                          modes  = NULL,
                          weekdays = NULL){

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

  # Filter on parameters
  if(length(date_range) > 1){
    data <- data %>%
      filter(dplyr::between(.data$day, as.Date(date_range[1]), as.Date(date_range[2])))
  }
  if(length(segments) > 1){
    data <- data %>%
      filter(.data$segment_name %in% segments)
  }
  if(length(weekdays) < 7){
    data <- data %>%
      filter(.data$weekday %in% weekdays)
  }

  # Melt dataframe (one row per transportation mode + direction + ids)
  data <- melt_direction_mode(data)
  data <- data %>%
      filter(.data$mode %in% modes,
             .data$direction %in% directions)

  result <- list('data' = data,
                 'segment' = segments,
                 'mode' = modes,
                 'direction' = directions,
                 'weekday' = weekdays)
  return(result)
}


#' Indicates if a date is in vacation period and if true, which vacation.
#' If the date is not in a vacation period, "No vacation" is returned.
#'
#' @param date Date (character format)
#' @param vacation Dataframe of vacations, same format as set_globals_vars output.
#' TODO : this function could be more efficient.
#'
#' @return Vacation description if the day is between two dates, "No vacation" otherwise.
#' @export
#'
#' @importFrom dplyr between
#'
#' @keywords internal
#'
which_vacations <- function(date, vacation){
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

#' Check if options are available in the options list, replace by a default otherwise.
#'
#'@param options_selected List of characters. Selected options.
#'@param options_available List of characters. Valid options.
#'@param default List of characters. Default options.
#'
#'@return Options consistent with the possibilities
#'@export
#'
#'@keywords internal
#'
check_options_graph <- function(options_selected, options_available, default){
  unknown = setdiff(options_selected, options_available)
  if(is.null(options_selected)){
    options = default
  } else {
    if(length(unknown) > 0){
      if(length(unknown) == length(options_selected)){
        options = default # replace by default values
      } else {
        options = intersect(options_selected, options_available) # keep only available options
      }
    } else {
      options = options_selected
    }
  }
  return(options)
}

#' Melt dataframe to obtain one row per hour/segment/transportation mode/direction
#' This format makes graphs with ggplot and filtering easier.
#'
#'@param data Traffic Data Frame
#'
#'@return DataFrame with one row per hour/segment/transportation mode/direction
#'@export
#'
#' @import dplyr
#' @import reshape2
#' @importFrom tidyr unnest
#'
#'@keywords internal
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
    mutate(direction = replace_na(direction, "both"))

  return(result)
}
