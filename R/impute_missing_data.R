#' Impute Missing Data for Traffic Analysis
#'
#' This function imputes missing data for traffic analysis using Random Forest models.
#' It can handle different types of transport (car, vehicle, heavy) and can impute all types simultaneously.
#'
#' @param data A data frame containing the traffic data.
#' @param sensors_name Character vector. Names of the sensors to include in the analysis. Default is NULL (all sensors).
#' @param transport_type Character. Type of transport to impute. Options are "car", "vehicle", "heavy", or "all". Default is "vehicle".
#'
#' @return A data frame with imputed values for the specified transport type(s) and a new column indicating whether the values were imputed or original.
#'
#' @details
#' The function requires specific columns to be present in the input data:
#' day_of_month, hour, weekday, month, year, vacation, season, week_number, segment_name, date, car and heavy.
#'
#' Vehicle is calculated as the sum of cars and heavy vehicles, providing a good estimate of the total traffic.

#' When transport_type is "all", the function imputes values for car, vehicle, and heavy.
#' For "heavy", it calculates values based on imputed car and vehicle data.
#'
#' @export
#'

#' @Importfrom yaml dplyr ranger

#' @examples
#' traffic_NA <- stop_sensor(traffic, successive_day = 2)
#' traffic_imputed <- impute_missing_data(traffic_NA, sensors_name = "RteVitre-06", transport_type = "vehicle")
#' table(traffic_imputed$imputed)



impute_missing_data <- function(data, sensors_name = NULL, transport_type = "vehicle") {

  #-- Check all the required elements --

  # Define valid transport types and required variables
  VALID_TRANSPORT_TYPES <- c("car", "vehicle", "heavy", "all")
  BASE_VARS <- c("day_of_month", "hour", "weekday", "month", "year", "vacation", "season", "week_number", "segment_name", "date")
  VEHICLE_VARS <- c("car", "heavy")


  # Validate input parameters
  if (!transport_type %in% VALID_TRANSPORT_TYPES) {
    stop(sprintf("Unrecognized transport type. Valid options are: %s", paste(VALID_TRANSPORT_TYPES, collapse = ", ")))
  }

  # Define required variables for the model
  required_vars <- c(BASE_VARS, VEHICLE_VARS)

  # Check if all required variables are present in the dataset
  missing_vars <- setdiff(required_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing required variables: %s", paste(missing_vars, collapse = ", ")))
  }

  # Filter the data by sensor name if specified
  if (!is.null(sensors_name)) {
    data <- data[data$segment_name %in% sensors_name, ]
  }


  # -- Define the model function --
  # Function to create and train model
  create_and_train_model <- function(data, target) {
    # Prepare data for Random Forest
    data_rf <- data %>%
      mutate(y = !!sym(target)) %>%
      select(y, all_of(BASE_VARS))

    # Split data into training and test sets
    is_train <- !is.na(data_rf$y)
    data_train <- data_rf[is_train, ] %>% mutate(imputed = "original")
    data_test <- data_rf[!is_train, ] %>% mutate(imputed = "imputed")

    # Train Random Forest model
    model_rf <- ranger(y ~ . - date, data = data_train, mtry = 8, min.node.size = 1)

    # Make predictions and combine results
    data_test$y <- predict(model_rf, data = data_test)$predictions
    result <- bind_rows(data_train, data_test)
    names(result)[names(result) == "y"] <- target

    return(result)
  }

  # -- Main function logic --

  # Store the original data
  original_data <- data

  # Impute data based on transport type
  if (transport_type == "heavy" || transport_type == "all") {
    # Impute 'vehicle' and 'car' separately
    data_vehicle <- create_and_train_model(data, "vehicle")
    data_car <- create_and_train_model(data, "car")

    # Calculate 'heavy' as max(0, vehicle - car)
    data_complete <- data_vehicle %>%
      select(vehicle, segment_name, date, imputed) %>%
      left_join(data_car %>% select(car, segment_name, date), by = c("segment_name", "date")) %>%
      mutate(
        heavy = pmax(0, vehicle - car),
        imputed = ifelse(imputed == "imputed" | is.na(car), "imputed", "original")
      )

    if (transport_type == "heavy") {
      data_complete <- data_complete %>% select(-vehicle, -car)
    }
  } else {
    # For 'car' and 'vehicle', use the original method
    data_complete <- create_and_train_model(data, transport_type)
  }

  # Add back any additional variables from the original data
  vars_to_add <- setdiff(colnames(original_data), colnames(data_complete))
  data_complete <- left_join(
    data_complete,
    original_data %>% select(segment_name, date, all_of(vars_to_add)),
    by = c("segment_name", "date")
  )


  # Sort the final dataset
  data_complete <- data_complete[order(data_complete$segment_name, data_complete$date), ]

  return(data_complete)
}
