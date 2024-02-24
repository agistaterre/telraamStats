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


#' Enrich traffic data with date features, names and uptime filters.
#' This function add day, weekday, hour, segment_name and full_name and uptime quality boolean.
#'
#' @param data Raw data frame from the Telraam API, imported through the package.
#'
#'
#' @return Same dataframe with additionnal informations.
#' @export
#'
#' @keywords internal
#'
enrich_traffic <- function(data){
  enriched_data <- enrich_dates(data)
  enriched_data <- enrich_name(enriched_data)
  enriched_data <- enrich_uptime(enriched_data)
  return(enriched_data)
}


#' Enrich traffic data with date informations
#'
#' @param data Data frame containing the date column
#'
#'
#' @return Same dataframe with 3 additionnal columns : day, weekday and hour
#' @export
#'
#' @keywords internal
#'
enrich_dates <- function(data){
  enriched_data <- data %>%
    mutate(
      day = as.Date(date),
      hour = hour(date),
      weekday = strftime(day,'%A')
    )
  return(enriched_data)
}


#' Enrich traffic data with segment name
#' Segment fullname is also added : it's the combination of segment's id and name.
#'
#' @param data Data frame containing a segment_id column
#'
#'
#' @return Same dataframe with two additionnal columns : segment name and full name
#' @export
#'
#' @keywords internal
#'
enrich_name <- function(data){
  enriched_data <- data %>%
    mutate(segment_name = lapply(segment_id, get_segment_name)) %>%
    unite(segment_fullname, segment_id, segment_name, sep = ' - ', remove = FALSE)
  return(enriched_data)
}


#' Enrich traffic data with uptime quality indication
#' If the uptime is lower than 0.5, uptime_quality will be FALSE, else TRUE
#'
#' @param data Data frame containing a uptime column
#'
#'
#' @return Same dataframe with an additionnal column indicating if the uptime is greater or lower than 0.5.
#' @export
#'
#' @keywords internal
#'
enrich_uptime <- function(data){
  enriched_data <- data %>%
    mutate(uptime_quality = (uptime >= 0.5))
  return(enriched_data)
}
