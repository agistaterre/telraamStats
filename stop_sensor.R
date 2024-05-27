stop_sensor<- function(df_init,uptime_choice=0.1,successive_day=2,remove_data=FALSE)
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

  #Créer une colonne pour avoir les saisons
  df_init$date <- ymd_hms(df_init$date) #mettre au format date
  df_init$season <- ifelse(month(df_init$date) %in% c(3,4,5), "Spring",
                        ifelse(month(df_init$date) %in% c(6,7,8), "Summer",
                               ifelse(month(df_init$date) %in% c(9,10,11), "Autumn", "Winter")))
  
  
  # Remove hours with no information by season
  df_season<-df_init %>% group_by(segment_id,season,hour) %>% summarise(condition=any(car!=0 & uptime>uptime_choice))
  
  df_init <- df_init %>%
    semi_join(df_season %>% filter(condition), by = c("season", "hour"))
  
  rm<-c()
  rm_fin<-c()
  
  for(i in 1:length(df_init$car))
  { j=i
  while ((df_init$car[i]==0 | df_init$uptime[i]<uptime_choice) & i<length(df_init$car))
  {rm<-c(rm,i)
  i<-i+1}
  
  diff_days<-abs(as.numeric(difftime(df_init$day[i], df_init$day[j], units = "days")))
  
  if(diff_days>successive_day)
  {rm_fin<-c(rm_fin,rm)}
  
  rm<-c()}
  
  if (is.null(rm_fin))
  {df_init<-df_init}
  
  else
  { if(remove_data==TRUE)
    {df_init<-df_init[-rm_fin,]}
    
    else
    {df_init[rm_fin,7:18]=NA
    df_fin<-df_init}
  }
  return(df_fin)
}
