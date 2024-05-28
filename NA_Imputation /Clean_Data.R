# Filter hours by season with a threshold for missing data
filtered_hours_data <- function(data, threshold = 0.85) {
  
  nrow_start <- nrow(data)
  
  # Transform date column to date format
  data$date <- ymd_hms(data$date)
  
  # Add a column for the season for each month
  data$season <- ifelse(month(data$date) %in% c(3,4,5), "Spring",
                        ifelse(month(data$date) %in% c(6,7,8), "Summer",
                               ifelse(month(data$date) %in% c(9,10,11), "Autumn", "Winter")))
  
  # Group by hour of the day and season for all data and missing data
  group_all <- data %>% group_by(season, hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)
  group_missing <- data %>% filter(data$uptime < 0.5) %>% group_by(season, hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)
  
  # Merge the two dataframes and calculate the proportion of missing data
  group <- merge(group_all, group_missing, by = c("season", "hour"), all = TRUE) %>% rename(n_missing = n.y, n = n.x)
  group$prop_missing <- group$n_missing / group$n * 100
  
  # Create a first plot of the proportion of missing data per hour of the day per season without filtering
  p1 <- group %>% ggplot(aes(x = hour, y = prop_missing, color = season)) + geom_line() + geom_point() + 
    labs(title = "Proportion of missing data per hour \n of the day per season before filtering", x = "Hour of the day", y = "Proportion of missing data")
  
  # Filter the hours where the proportion of missing data is higher than the threshold
  hour_missing <- group %>% filter(prop_missing > threshold) %>% select(hour, season) %>% unique()
  
  # Keep the data where the hour is not in the list of hours with high proportion of missing data
  data <- data %>% filter(!(hour(date) %in% hour_missing$hour & season %in% hour_missing$season))
  
  # Count the number of missing data
  number_missing <- nrow(data %>% filter(data$uptime < 0.5))
  
  # Count the number of rows removed
  nrow_subtract <- n
  nrow_subtract <- nrow_start - nrow(data)
  
  # Create a second plot of the proportion of missing data per hour of the day per season after filtering
  # Group by hour of the day and season for all data and missing data
  group_all_filtered <- data %>% group_by(season, hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)
  group_missing_filtered <- data %>% filter(data$uptime < 0.5) %>% group_by(season, hour(date)) %>% summarise(n = n()) %>% rename(hour = `hour(date)`)
  
  # Merge the two dataframes and calculate the proportion of missing data
  group_filtered <- merge(group_all_filtered, group_missing_filtered, by = c("season", "hour"), all = TRUE) %>% rename(n_missing = n.y, n = n.x)
  group_filtered$prop_missing <- group_filtered$n_missing / group_filtered$n * 100
  
  p2 <- group_filtered %>% ggplot(aes(x = hour, y = prop_missing, color = season)) + geom_line() + geom_point() + 
    labs(title = "Proportion of missing data per hour \n of the day per season after filtering", x = "Hour of the day", y = "Proportion of missing data") + ylim(0,100)
  
  p <- patchwork::wrap_plots(p1, p2)
  
  # Return a list with the filtered data, the plot, the threshold, the number of missing data, and the number of rows removed
  list_return <- list(hour_filter = hour_missing, data = data, plot = p, threshold = threshold, number_missing = number_missing, nrow_subtract = nrow_subtract)
  return(list_return)
}

# Function to remove successive periods of missing data
substract_successive_NA <- function(data_raw, threshold = 24) {
  # Initialize a new column to store the successive periods of missing data
  data_raw$successive_missing <- 0
  
  # Select relevant columns and filter missing data
  data_false <- data_raw %>% filter(!uptime_quality) %>% arrange(date)
  data_true <- data_raw %>% filter(uptime_quality) %>% arrange(date)
  
  # Initialize index for the loop
  i <- 1
  
  while (i < nrow(data_false)) {
    # Count the number of successive missing data
    succ <- 1
    while ((i + succ <= nrow(data_false)) && (difftime(data_false$date[i + succ], data_false$date[i + succ - 1], units = "hours") <= 1)) {
      succ <- succ + 1
    }
    
    # Assign the value of succ to all successive rows found
    data_false$successive_missing[i:(i + succ - 1)] <- succ
    
    # Move the index forward by the size of the successive period found
    i <- i + succ
  }
  
  filtered_data_false <- data_false %>% filter(successive_missing < threshold)
  
  # Merge missing data
  data <- rbind(filtered_data_false, data_true) %>% arrange(date)
  
  return(data)
}

# Function to clean NA segments from a list or a single dataframe to an unique dataframe
Clean_NA_segments <- function(list_data, threshold_missing = 85, threshold_successive = 24) {
  
  # Check if the input data is a list
  if (!is.data.frame(list_data)) {
    # Initialize an empty dataframe to store cleaned segments
    segments_clear <- data.frame()
    
    # Iterate through each segment in the list
    for (i in 1:length(list_data)) {
      # Extract the current segment
      seg <- list_data[i]
      
      # Clean the data by removing successive NAs longer than the threshold
      data <- substract_successive_NA(data.frame(seg), threshold_successive)
      
      # Filter the data to remove hours with missing values above the threshold
      segments_clear <- rbind(segments_clear, filtered_hours_data(data, threshold_missing)$data)
    }
  } else {
    # If the input data is not a list, treat it as a single dataframe
    segments_clear <- substract_successive_NA(data.frame(list_data), threshold_successive)
    segments_clear <- filtered_hours_data(segments_clear, threshold_missing)$data
  }
  
  # Return the cleaned segments
  return(segments_clear)
}

# Function to clean NA segments from a list of dataframes to a list of dataframes 
Clean_NA_segments_list <- function(list_data, threshold_missing = 85, threshold_successive = 24) {
  # Initialize an empty list to store cleaned data
  list_clear_data <- list()
  
  # Iterate through each segment in the list
  for (i in 1:length(list_data)) {
    # Extract the current segment
    seg <- list_data[i]
    
    # Clean the data by removing successive NAs longer than the threshold
    data <- substract_successive_NA(data.frame(seg), threshold_successive)
    
    # Filter the data to remove hours with missing values above the threshold
    list_clear_data[[i]] <- filtered_hours_data(data, threshold_missing)
  }
  
  # Return the list of cleaned data
  return(list_clear_data)
}


NA_Type <- function(segments_clear,threshold = 50){
  #Identifier pour chaque données manquantes sont type 1 ou 2
  summarise_NA <- segments_clear %>% group_by(date) %>% summarise(n_missing = sum(uptime < 0.5)) %>% arrange(date)
  summarise_NA$prop_missing <- summarise_NA$n_missing / length(unique(segments_clear$segment_name)) * 100
  
  #Si plus de 50% des données sont manquantes, on considère que la donnée est de type 2 sinon de type 1 
  summarise_NA$type <- ifelse(summarise_NA$prop_missing > threshold, 2, 1)
  
  #Pour chaque date de data_clear on va ajouter le type 
  segments_clear <- left_join(segments_clear, summarise_NA %>% select(date, type), by = "date")
  segments_clear$type[which(segments_clear$uptime_quality == T)] <- NA
  return(segments_clear)
}


# Function to delete a specific segment within a date range
delete_segment <- function(data, name, start_date, end_date) {
  # Filter out rows where the segment name matches and the date is within the specified range
  data <- data %>% filter(!(segment_name == name & date >= start_date & date <= end_date))
  
  # Return the filtered data
  return(data)
}

