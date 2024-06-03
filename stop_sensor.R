stop_sensor<- function(enriched_data,
                       date_range = NULL,
                       weekday_choice = NULL,
                       hour_choice = NULL,
                       vacation_choice=NULL,
                       holiday_choice=NULL,
                       segments = NULL,
                       uptime_choice=0.5,
                       successive_day=2,
                       remove_data=FALSE)
{
  # Input parameter :
  #       - df_init : Initial Dataframe 
  #       - uptime_choice : Real which define the quality of the data below which we do not need the data
  #       - successive_day : Integer which define number of successives days above which we do not need the data
  #       - remove_data : Booleen which choose if we keep the row or not
  # Output parameter :
  #       - df_fin : Dataframe
  # Description :
  # Remove hours where we have no information as the night or less than 10% of information
  # Remove periods when the sensor did not work
  #
  # Packages :
  #       - lubridate
  #       - dplyr

  #Filtrer les données en fonction des demandes de l'utilisateur
  if(!is.null(segments))
    {enriched_data<-enriched_data[enriched_data$segment_name==segments,]}
    
  if(!is.null(date_range))
    {enriched_data<-enriched_data[enriched_data$day>=date[1] & enriched_data$day<= date[2],]}

  if(length(enriched_data$car)==0){print("Aucune donnée sur la période sélectionnée")}
  
  else
  {
  #Créer une colonne pour avoir les saisons
  enriched_data$date <- ymd_hms(enriched_data$date) #mettre au format date
  enriched_data$season <- ifelse(month(enriched_data$date) %in% c(3,4,5), "Spring",
                        ifelse(month(enriched_data$date) %in% c(6,7,8), "Summer",
                               ifelse(month(enriched_data$date) %in% c(9,10,11), "Autumn", "Winter")))
  
  
  # Remove hours with no information by season
  df_season<-enriched_data %>% group_by(segment_id,season,hour) %>% summarise(condition=any(car!=0 & uptime>uptime_choice))
  
  enriched_data <- enriched_data %>%
    semi_join(df_season %>% filter(condition), by = c("segment_id","season", "hour"))
  
  print(length(enriched_data$car))
  
  #Périodes d'inactivités
  rm<-c()
  rm_fin<-c()
  
  list_clear_data <- list()
  seg_id<-unique(enriched_data$segment_id)
  
  for(id in 1:length(seg_id))
  {
    df_segment<-enriched_data[enriched_data$segment_id==seg_id[id],]
    for(i in 1:length(df_segment$car))
      { j=i
        while ((df_segment$car[i]==0 | df_segment$uptime[i]<uptime_choice) & i<length(df_segment$car))
          {rm<-c(rm,i)
           i<-i+1}
  
        diff_days<-abs(as.numeric(difftime(df_segment$day[i], df_segment$day[j], units = "days")))
  
        if(diff_days>successive_day)
          {rm_fin<-c(rm_fin,rm)}
  
        rm<-c()}
  
        if (is.null(rm_fin))
          {df_segment<-df_segment}
  
        else
          { if(remove_data==TRUE)
              {df_segment<-df_segment[-rm_fin,]}
    
            else
              { colonnes_voulues<-c('heavy','car','bike','pedestrian','heavy_lft','heavy_rgt','car_rgt','car_lft','bike_lft','bike_rgt','pedestrian_lft','pedestrian_rgt')
                df_segment[rm_fin,colonnes_voulues]=NA}
          }
    list_clear_data[[id]]<-df_segment
  }
  df_fin<-list_clear_data[[1]]
  
  if(length(seg_id)>1)
    {for(i in 2:length(seg_id))
      {df_fin<-rbind(df_fin,list_clear_data[[i]])}}
  
  
  #Filtration des données avec les demandes de l'utilisateur
  
  if(!is.null(weekday_choice))
  {  
    df_fin$weekday<-tolower(df_fin$weekday)
    tolower(weekday_choice)
    df_fin<-df_fin %>% filter(weekday %in% weekday_choice) }

  if(!is.null(hour_choice))
  {df_fin<-df_fin %>% filter(hour %in% hour_choice)}
  
  if(!is.null(vacation_choice))
  {df_fin<-df_fin %>% filter(vacation %in% vacation_choice)}
  
  if(!is.null(holiday_choice))
  {df_fin<-df_fin %>% filter(holiday %in% holiday_choice)}
  
  return(df_fin)
  }
}
