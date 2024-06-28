#' Impute Missing Data for Traffic Analysis
#'
#' This function imputes missing data for traffic analysis using Random Forest models.
#' It can handle different types of transport (car, vehicle, heavy) and can impute all types simultaneously.
#'
#' @param data A data frame containing the traffic data.
#' @param sensors_id Character vector. Id of the sensors to include in the analysis. Default is NULL (all sensors).
#' @param transport_type Character. Type of transport to impute. Options are "car", "vehicle", "heavy", or "all". Default is "vehicle".
#'
#'
#' @return A data frame with imputed values for the specified transport type(s) and a new column indicating whether the values were imputed or original.
#'
#' @details
#' The function requires specific columns to be present in the input data:
#' day_of_month, hour, weekday, month, year, vacation, season, week_number, segment_id, date, car and heavy.
#'
#' Vehicle is calculated as the sum of cars and heavy vehicles, providing a good estimate of the total traffic.

#' When transport_type is "all", the function imputes values for car, vehicle, and heavy.
#'
#' @export
#'

#' @Importfrom yaml dplyr ranger lubridate

#' @examples
#' traffic_NA <- stop_sensor(traffic, successive_day = 2)
#' traffic_imputed <- impute_missing_data(traffic_NA, sensors_name = "RteVitre-06", transport_type = "vehicle")
#' table(traffic_imputed$imputed)


# 1. Function to validate and preprocess the input data
validate_and_preprocess_data <- function(data, transport_type, sensors_id ) {
  # Define constants
  VALID_TRANSPORT_TYPES <- c("car", "vehicle", "heavy", "all")
  BASE_VARS <- c("day_of_month", "hour", "weekday", "month", "year", "vacation", "week_number", "segment_id", "date")
  VEHICLE_VARS <- c("car", "heavy")

  # Validate input parameters
  if (!transport_type %in% VALID_TRANSPORT_TYPES) {
    stop(sprintf("Unrecognized transport type. Valid options are: %s", paste(VALID_TRANSPORT_TYPES, collapse = ", ")))
  }

  # Define required variables for the model
  required_vars <- c(BASE_VARS, VEHICLE_VARS)

  # Check if date column is present
  if (!"date" %in% colnames(data)) {
    stop("The 'date' column is missing from the dataset.")
  }

  # Add missing date-related columns using lubridate if they're not present
  if (!"day_of_month" %in% colnames(data)) {
    data <- data %>% mutate(day_of_month = day(date))
  }
  if (!"hour" %in% colnames(data)) {
    data <- data %>% mutate(hour = hour(date))
  }
  if (!"month" %in% colnames(data)) {
    data <- data %>% mutate(month = month(date))
  }
  if (!"year" %in% colnames(data)) {
    data <- data %>% mutate(year = year(date))
  }
  if (!"week_number" %in% colnames(data)) {
    data <- data %>% mutate(week_number = week(date))
  }
  if (!"weekday" %in% colnames(data)) {
    data <- data %>% mutate(weekday = wday(date))
  }

  # Check if all required variables are present in the dataset
  missing_vars <- setdiff(required_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing required variables: %s", paste(missing_vars, collapse = ", ")))
  }

  # Filter data by segment name if specified
  if (!is.null(sensors_id)) {
    data <- data[data$segment_id %in% sensors_id, ]
  }

  # Convert data types to the correct format
  data <- data %>%
    mutate(
      day_of_month = as.numeric(day_of_month),
      hour = as.numeric(hour),
      weekday = as.factor(weekday),
      month = as.factor(month),
      year = as.numeric(year),
      vacation = ifelse(is.list(vacation), as.factor(unlist(vacation)), as.factor(vacation)),
      week_number = as.numeric(week_number),
      segment_id = if (is.factor(segment_id)) {
        factor(as.character(segment_id), levels = levels(segment_id))
      } else if (is.list(segment_id)) {
        factor(sapply(segment_id, as.character), levels = unique(sapply(segment_id, as.character)))
      } else {
        factor(as.character(segment_id), levels = unique(as.character(segment_id)))
      }
        )

  # Calculate vehicle if not present
  if (!"vehicle" %in% colnames(data)) {
    data <- data %>%
      mutate(vehicle = car + heavy)
    }

  return(data)
}


# 2. Function to create and train the model
create_and_train_model <- function(data, target, base_vars,threshold_uptime) {
  # Prepare data for Random Forest
  data_rf <- data %>%
    mutate(y = ifelse(uptime < threshold_uptime,NA,!!sym(target))) %>% select(-!!sym(target))

  # Split data into training and test sets
  is_train <- !is.na(data_rf$y)
  data_train <- data_rf[is_train, ] %>% mutate(imputed = "original")
  data_test <- data_rf[!is_train, ] %>%  mutate(imputed = "imputed")

  # Remove NA from training data
  data_train_clean <- data_train %>%
    select(y,all_of(base_vars)) %>%
    na.omit()

  # Train Random Forest model
  model_rf <- ranger(y ~ . - date, data = data_train_clean, mtry = 8, min.node.size = 1)


  # Prepare test data, keeping track of removed rows
  data_test_clean <- data_test %>%
    select(all_of(base_vars))
  rows_to_predict <- complete.cases(data_test_clean)

  # Make predictions only for complete cases
  predictions <- predict(model_rf, data = data_test_clean[rows_to_predict, ])$predictions

  # Assign predictions back to the original test data frame
  data_test$y <- NA
  data_test$y[rows_to_predict] <- predictions
  data_test$imputed <- ifelse(is.na(data_test$y), "not imputed", "imputed")

  # Combine results
  result <- bind_rows(data_train, data_test)
  names(result)[names(result) == "y"] <- target

  return(result)
}

# 3. Main function logic
impute_missing_data <- function(data, sensors_name = NULL, transport_type = "vehicle",threshold_uptime=0.5) {

  # Validate and preprocess the input data
  data <- validate_and_preprocess_data(data, transport_type, sensors_name)

  # Define constants
  BASE_VARS <- c("day_of_month", "hour", "weekday", "month", "year", "vacation", "week_number", "segment_id", "date")

  # Impute data based on transport type
  if ( transport_type == "all") {
    # Impute 'vehicle' and 'car' separately
    data_vehicle <- create_and_train_model(data, "vehicle", BASE_VARS,threshold_uptime)
    data_car <- create_and_train_model(data, "car", BASE_VARS,threshold_uptime)
    data_heavy <- create_and_train_model(data, "heavy", BASE_VARS,threshold_uptime)

     # Calculate 'heavy' as max(0, vehicle - car)
    data_complete <- data_vehicle %>%
      select(vehicle, segment_id, date ,imputed) %>%
      left_join(data_car %>% select(car, segment_id, date), by = c("segment_id", "date")) %>%
      left_join(data_heavy %>% select(heavy, segment_id, date), by = c("segment_id", "date")) %>%
      left_join(data %>% select(-car,-vehicle,-heavy), by = c("segment_id", "date"))

  } else {
    # For 'car' and 'vehicle' or 'heavy' , use the original method
    data_complete <- create_and_train_model(data, transport_type, BASE_VARS,threshold_uptime)
  }

  # Sort the final dataset
  data_complete <- data_complete %>%
    arrange(segment_id, date)

  return(data_complete)
}




















