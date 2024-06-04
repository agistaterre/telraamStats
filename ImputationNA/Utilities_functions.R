

# Function to plot the seasonality of missing values for all sensors
plot_NA <- function(data) {
  # Filter data for rows with uptime_quality as FALSE (i.e., missing values)
  # Select only the date and segment_name columns
  data_long <- data %>%
    filter(!uptime_quality) %>%
    select(date, segment_name) %>%
    # Extract a numeric value from the segment_name (last two characters)
    mutate(value = as.numeric(substr(as.character(segment_name), nchar(as.character(segment_name)) - 1, nchar(as.character(segment_name)))),
           # Convert segment_name to a factor for plotting
           segment_name = as.factor(as.character(segment_name)))

  # Plot the data using ggplot2
  # x-axis: date, y-axis: extracted numeric value, color: segment_name
  data_long %>% ggplot(aes(x = date, y = value, color = segment_name)) +
    geom_point() +
    labs(title = "Seasonality of Missing Values for All Sensors",
         x = "Date",
         y = "Sensor Value",
         color = "Segment Name") +
    theme_minimal()
}

#On crée une pipeline pour tester les différentes stratégies d'imputation

Create_test_data <- function(data,num_missing=NA,seed = NA,prop_missing=NA){

  #Creer la seed si spécifié
  if(!is.na(seed)){set.seed(seed)}

  if(is.na(num_missing) & is.na(prop_missing)){
    stop("You must specify either the number of missing values or the proportion of missing values")
  }

  if(!is.na(num_missing) & !is.na(prop_missing)){
    stop("You must specify either the number of missing values or the proportion of missing values")
  }


  if(!is.na(prop_missing)){
    num_missing <- round(nrow(data)*prop_missing)
  }

  data <- data %>% na.omit() %>% filter(uptime_quality) %>% mutate(vehicle = car + heavy)


  #Dans un premier temps on peut créer nos valeurs manquante
  temoin <- data
  test <- data

  vec_sample <- sample(1:nrow(data),num_missing)
  test$vehicle[vec_sample] <- NA


  list(temoin=temoin,test=test)
}

