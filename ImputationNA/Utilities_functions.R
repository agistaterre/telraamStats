

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



Create_test_data_unif <- function(data,num_missing=NA,seed = NA,prop_missing=NA){

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

  vec_sample <- sample(1:nrow(data),num_missing,replace = F)
  test$vehicle[vec_sample] <- NA


  list(temoin=temoin,test=test)
}


#On crée une pipeline pour tester les différentes stratégies d'imputation
Create_test_data_suite <- function(data, num_missing = NA, seed = NA, prop_missing = NA, max_na_length = 72) {

  # Créer la seed si spécifié
  if (!is.na(seed)) {
    set.seed(seed)
  }

  if (is.na(num_missing) & is.na(prop_missing)) {
    stop("You must specify either the number of missing values or the proportion of missing values")
  }

  if (!is.na(num_missing) & !is.na(prop_missing)) {
    stop("You must specify either the number of missing values ou la proportion de missing values")
  }

  if (!is.na(prop_missing)) {
    num_missing <- round(nrow(data) * prop_missing)
  }



  data <- data %>% filter(uptime_quality) %>% mutate(vehicle = car + heavy)

   # Vérifier si le nombre total de NA demandés est plus que les lignes de données disponibles
  if (num_missing > nrow(data)) {
    stop("The number of missing values exceeds the number of available data points.")
  }

  # Dans un premier temps on peut créer nos valeurs manquantes
  temoin <- data
  test <- data

  # Créer des suites de NA avec une longueur maximale de 72
  na_lengths <- c()
  while (sum(na_lengths) < num_missing) {
    remaining_na <- num_missing - sum(na_lengths)
    na_lengths <- c(na_lengths, sample(1:min(max_na_length, remaining_na), 1,replace=T))
  }

  # Vérifier que nous n'avons pas dépassé le nombre de NA nécessaires
  na_lengths <- na_lengths[1:which(cumsum(na_lengths) >= num_missing)[1]]

  # Calculer les positions de début pour les séquences de NA
  start_positions <- sample(1:(nrow(test) - max(na_lengths) + 1), length(na_lengths))

  # Introduire les NA dans les données
  for (i in seq_along(start_positions)) {
    start <- start_positions[i]
    end <- min(nrow(test), start + na_lengths[i] - 1)
    test$vehicle[start:end] <- NA
  }

  list(temoin = temoin, test = test)
}
