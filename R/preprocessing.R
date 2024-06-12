#' @description
#' A short description...
#' Retrieve hours with no data and replace incomplete data with NA,
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments Character vector. Selected road segment, all if NULL (default).
#' @param successive_day Integer. Number of day choosen. Default to 2
#' @param uptime_choice Real. Uptime choosen. Default to 0.5
#' 
#' @return enriched_data 
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' retrieve_missing_data(traffic)
#' retrieve_missing_data(traffic,
#'                       date_range = c('2022-07-01','2022-09-01'),
#'                       segment = 'RteVitre-06',
#'                       uptime_choice=0.3,
#'                       successive_day=1)

retrieve_missing_data<- function(enriched_data,
                                 date_range = NULL,
                                 segments = NULL,
                                 uptime_choice=0.5,
                                 successive_day=2)
{
  
  if(!is.null(segments))
  {enriched_data<-enriched_data[enriched_data$segment_name==segments,]}
  
  if(!is.null(date_range))
  {enriched_data<-enriched_data[enriched_data$day>=date_range[1] & enriched_data$day<= date_range[2],]}
  
  if(length(enriched_data$car)==0){stop("No data in the selectionned period")}
  
  else
  {
    #Hours with no data
    enriched_data<-retrieve_missing_hours(enriched_data,uptime_choice) 
    
    #Inactivity Period
    enriched_data <- enriched_data %>%
      mutate(
        heavy_NA = heavy,
        car_NA = car,
        bike_NA = bike,
        pedestrian_NA = pedestrian
      )   

    replace_inactivity_period(enriched_data,successive_day,uptime_choice)
  }
    return(enriched_data)
}

#' @description
#' A short description...
#' Retrieve hours with no data 
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param uptime_choice Real. Uptime choosen. Default to 0.5
#' 
#' @return enriched_data 
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' retrieve_missing_hours(traffic)
#' retrieve_missing_hours(traffic,
#'                         uptime_choice=0.3)
                       
retrieve_missing_hours<-function(enriched_data,uptime_choice)
{
  enriched_data$date <- ymd_hms(enriched_data$date) 
  enriched_data$season <- ifelse(month(enriched_data$date) %in% c(3,4,5), "Spring",
                               ifelse(month(enriched_data$date) %in% c(6,7,8), "Summer",
                                      ifelse(month(enriched_data$date) %in% c(9,10,11), "Autumn", "Winter")))

  df_season<-enriched_data %>% group_by(segment_id,season,hour) %>% summarise(condition=any(car!=0 & uptime>uptime_choice))

  enriched_data <- enriched_data %>% semi_join(df_season %>% filter(condition), by = c("segment_id","season", "hour"))

  return(enriched_data)
}

#' @description
#' A short description...
#' Replace incomplete data with NA,
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param successive_day Integer. Number of day choosen. Default to 2
#' @param uptime_choice Real. Uptime choosen. Default to 0.5
#' 
#' @return enriched_data 
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' replace_inactivity_period(traffic)
#' replace_inactivity_period(traffic,
#'                           uptime_choice=0.3,
#'                           successive_day=1)

replace_inactivity_period<-function (enriched_data,successive_day,uptime_choice)
{
  list_clear_data <- list()
  seg_id<-unique(enriched_data$segment_id)
  
  for(id in 1:length(seg_id))
  {
    df_segment<-enriched_data[enriched_data$segment_id==seg_id[id],]
    for(i in 1:length(df_segment$car))
    {
      j=i
      while ((df_segment$car[i]==0 | df_segment$uptime[i]<uptime_choice) & i<length(df_segment$car))
      {i<-i+1}
      
      diff_days<-abs(as.numeric(difftime(df_segment$day[i], df_segment$day[j], units = "days")))
      
      if(diff_days>successive_day)
      {
        df_segment <- df_segment %>%
          mutate_at(vars(heavy_NA, car_NA, bike_NA,pedestrian_NA), ~ ifelse(row_number() %in% j:i, NA,.))
      }
    }
    list_clear_data[[id]]<-df_segment
  }
  enriched_data<-list_clear_data[[1]]
  
  if(length(seg_id)>1)
  {
    for(i in 2:length(seg_id))
    {enriched_data<-rbind(enriched_data,list_clear_data[[i]])}
  }
  
  return(enriched_data)
}
