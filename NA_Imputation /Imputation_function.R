# Function for time series imputation using the Prophet model
TimeSeries_Imputation <- function(data) {
  # Load the prophet library
  library(prophet)
  
  # Prepare the training set by creating 'ds' and 'y' columns
  train_set <- data %>% mutate(ds = date, y = ifelse(uptime_quality, car + heavy, NA)) %>% select(ds, y)
  # Prepare the test set by filtering out rows where uptime_quality is false
  test_set <- data %>% filter(!uptime_quality) %>% mutate(ds = date) %>% select(ds)
  
  # Prepare the holidays data for the Prophet model
  holidays <- data %>% filter(holiday) %>% mutate(lower_window = 0, upper_window = 1, vacation = as.character(vacation)) %>% 
    select(vacation, date, lower_window, upper_window) %>% rename(holiday = vacation, ds = date)
  
  # Train the Prophet model using the training set and holidays
  model <- prophet(train_set, holidays = holidays)
  # Predict values using the test set
  pred <- predict(model, test_set)
  
  # Adjust predictions on the training set to obtain the fitting curve
  ajust <- predict(model, train_set)
  fitting_curve <- plot(model, ajust)
  
  # Plot the components of the model
  components <- prophet_plot_components(model, pred)
  
  # Prepare the prediction data for plotting
  dpred <- pred %>% select(ds, yhat) %>% rename(y = yhat) %>% mutate(type = "pred", y = ifelse(y < 0, 0, y))
  # Prepare the real data for plotting
  dtrain <- train_set %>% na.omit() %>% select(ds, y) %>% mutate(type = "real")
  
  # Convert 'ds' columns to POSIXct format for both data frames
  dtrain$ds <- as.POSIXct(dtrain$ds)
  dpred$ds <- as.POSIXct(dpred$ds)
  
  # Create a ggplot of the input data, including both real and predicted values
  plot_inputdata <- ggplot() +
    geom_point(data = dtrain, aes(x = ds, y = y, color = type)) +
    geom_point(data = dpred, aes(x = ds, y = y, color = type)) +
    ggtitle(paste("Prédiction de la fréquentation du segment", data$segment_name[1])) +
    xlab("Date") +
    ylab("Nombre de véhicules") +
    theme(legend.position = "none")
  
  # Return a list containing the prediction, fitting curve, components, and plot
  return(list(prediction = dpred, fitting_curve = fitting_curve, components = components, Imputation = plot_inputdata))
}

# Function for time series imputation for a list of datasets using the Prophet model
TimeSeries_Imputation_list <- function(list_data) {
  # Load the prophet library
  library(prophet)
  list_pred <- list()
  
  # Loop through each dataset in the list
  for (i in 1:length(list_data)) {
    data <- list_data[[i]]$data
    
    # Prepare the training set by creating 'ds' and 'y' columns
    train_set <- data %>% mutate(ds = date, y = ifelse(uptime_quality, car + heavy, NA)) %>% select(ds, y)
    # Prepare the test set by filtering out rows where uptime_quality is false
    test_set <- data %>% filter(!uptime_quality) %>% mutate(ds = date) %>% select(ds)
    
    # Prepare the holidays data for the Prophet model
    holidays <- data %>% filter(holiday) %>% mutate(lower_window = 0, upper_window = 1, vacation = as.character(vacation)) %>% 
      select(vacation, date, lower_window, upper_window) %>% rename(holiday = vacation, ds = date)
    
    # Train the Prophet model using the training set and holidays
    model <- prophet(train_set, holidays = holidays)
    # Predict values using the test set
    pred <- predict(model, test_set)
    
    # Adjust predictions on the training set to obtain the fitting curve
    ajust <- predict(model, train_set)
    fitting_curve <- plot(model, ajust)
    
    # Plot the components of the model
    components <- prophet_plot_components(model, pred)
    
    # Prepare the prediction data for plotting
    dpred <- pred %>% select(ds, yhat) %>% rename(y = yhat) %>% mutate(type = "pred", y = ifelse(y < 0, 0, y))
    # Prepare the real data for plotting
    dtrain <- train_set %>% na.omit() %>% select(ds, y) %>% mutate(type = "real")
    
    # Convert 'ds' columns to POSIXct format for both data frames
    dtrain$ds <- as.POSIXct(dtrain$ds)
    dpred$ds <- as.POSIXct(dpred$ds)
    
    # Create a ggplot of the input data, including both real and predicted values
    plot_inputdata <- ggplot() +
      geom_point(data = dtrain, aes(x = ds, y = y, color = type)) +
      geom_point(data = dpred, aes(x = ds, y = y, color = type)) +
      ggtitle(paste("Prédiction de la fréquentation du segment", data$segment_name[1])) +
      xlab("Date") +
      ylab("Nombre de véhicules") +
      theme(legend.position = "none")
    
    # Create a list containing the prediction, fitting curve, components, plot, and model
    output <- list(prediction = dpred, fitting_curve = fitting_curve, components = components, Imputation = plot_inputdata, model = model)
    list_pred[[i]] <- output
  }
  
  # Return the list of predictions and plots
  return(list_pred)
}
