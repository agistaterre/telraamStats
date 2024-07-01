#' Impute Missing Data for Traffic Analysis
#'
#' This function imputes missing data for traffic analysis using Random Forest models.
#' It can handle different types of transport (car, vehicle, heavy) and can impute all types simultaneously.
#'
#' @param data A data frame containing the traffic data.
#' @param sensors_id Character vector. Id of the sensors to include in the analysis. Default is NULL (all sensors).
#' @param transport_type Character. Type of transport to impute. Options are "car", "vehicle", "heavy", or "all". Default is "vehicle".
#' @param BASE_VARS TBD
#'
#' @return A data frame with imputed values for the specified transport type(s) and a new column indicating whether the values were imputed or original.
#'
#' @details
#' The function requires specific columns to be present in the input data:
#' day_of_month, hour, weekday, month, year, vacation, week_number, segment_id, date, car, heavy, and uptime.
#'
#' Vehicle is calculated as the sum of cars and heavy vehicles providing a good estimate of the total traffic.
#'
#' When transport_type is "all", the function imputes values for car, vehicle, and heavy separately.
#'
#' The function uses the uptime column to determine which data points need imputation. Values with uptime below the threshold_uptime are considered missing and are imputed.
#'
#'Time interval could be hourly or quarterly. The function will automatically detect the time interval based on the data and add a minute variable if its quarterly.
#' @export
#'
#' @importFrom ranger ranger
#' @importFrom lubridate day hour month year week wday minute
#'
#' @examples
#' traffic_imputed <- impute_missing_data(traffic,
#'   sensors_name = "RteVitre-06",
#'   transport_type = "vehicle",
#'   threshold_uptime = 0.5)
#' table(traffic_imputed$imputed)
validate_and_preprocess_data <-
  function(data,
           transport_type,
           sensors_id,
           BASE_VARS) {
    # Define constants
    VALID_TRANSPORT_TYPES <- c("car", "vehicle", "heavy", "all")
    VEHICLE_VARS <- c("car", "heavy")

    # Validate input parameters
    if (!transport_type %in% VALID_TRANSPORT_TYPES) {
      stop(sprintf(
        "Unrecognized transport type. Valid options are: %s",
        paste(VALID_TRANSPORT_TYPES, collapse = ", ")
      ))
    }

    # Check if date column is present
    if (!"date" %in% colnames(data)) {
      stop("The 'date' column is missing from the dataset.")
    }

    # Add missing date-related columns using lubridate if they're not present
    if (!"day_of_month" %in% colnames(data)) {
      data <- data %>% mutate(day_of_month = day(.data$date))
    }
    if (!"hour" %in% colnames(data)) {
      data <- data %>% mutate(hour = hour(.data$date))
    }
    if (!"month" %in% colnames(data)) {
      data <- data %>% mutate(month = month(.data$date))
    }
    if (!"year" %in% colnames(data)) {
      data <- data %>% mutate(year = year(.data$date))
    }
    if (!"week_number" %in% colnames(data)) {
      data <- data %>% mutate(week_number = week(.data$date))
    }
    if (!"weekday" %in% colnames(data)) {
      data <- data %>% mutate(weekday = wday(.data$date))
    }

    # Add minute column if interval is "quarterly"
    if (data$interval[1] == "quarterly") {
      data <- data %>% mutate(minute = minute(.data$date))
    }


    # Define required variables for the model
    required_vars <- c(BASE_VARS, VEHICLE_VARS)

    # Check if all required variables are present in the dataset
    missing_vars <- setdiff(required_vars, colnames(data))
    if (length(missing_vars) > 0) {
      stop(sprintf(
        "Missing required variables: %s",
        paste(missing_vars, collapse = ", ")
      ))
    }

    # Filter data by segment name if specified
    if (!is.null(sensors_id)) {
      data <- data[data$segment_id %in% sensors_id,]
    }

    # Convert data types to the correct format
    data <- data %>%
      mutate(
        day_of_month = as.numeric(.data$day_of_month),
        hour = as.numeric(.data$hour),
        weekday = as.factor(.data$weekday),
        month = as.factor(.data$month),
        year = as.numeric(.data$year),
        vacation = ifelse(
          is.list(.data$vacation),
          as.factor(unlist(.data$vacation)),
          as.factor(.data$vacation)
        ),
        week_number = as.numeric(.data$week_number),
        segment_id = if (is.factor(.data$segment_id)) {
          factor(as.character(.data$segment_id),
                 levels = levels(.data$segment_id))
        } else if (is.list(.data$segment_id)) {
          factor(sapply(.data$segment_id, as.character), levels = unique(sapply(.data$segment_id, as.character)))
        } else {
          factor(as.character(.data$segment_id), levels = unique(as.character(.data$segment_id)))
        }
      )

    # Calculate vehicle if not present
    if (!"vehicle" %in% colnames(data)) {
      data <- data %>%
        mutate(vehicle = .data$car + .data$heavy)
    }

    return(data)
  }


#' TODO
#'
#' @param data TBD
#' @param target TBD
#' @param base_vars TBD
#' @param threshold_uptime TBD
#'
#' @return TBD
#'
#' @export
#'
#' @importFrom stats complete.cases na.omit predict
#'
#' @examples
#' 1+1
create_and_train_model <-
  function(data, target, base_vars, threshold_uptime) {
    # Prepare data for Random Forest
    data_rf <- data %>%
      mutate(y = ifelse(.data$uptime < threshold_uptime,
                        NA,!!sym(target))) %>% select(-!!sym(target))

    # Split data into training and test sets
    is_train <- !is.na(data_rf$y)
    data_train <- data_rf[is_train,]
    data_test <- data_rf[!is_train,]

    # Remove NA from training data
    data_train_clean <- data_train %>%
      select(.data$y, all_of(base_vars)) %>%
      na.omit()

    # Train Random Forest model
    model_rf <-
      ranger(y ~ . - date,
             data = data_train_clean,
             mtry = 8,
             min.node.size = 1)


    # Prepare test data, keeping track of removed rows
    data_test_clean <- data_test %>%
      select(all_of(base_vars))
    rows_to_predict <- complete.cases(data_test_clean)


    # Make predictions only for complete cases
    predictions <-
      predict(model_rf, data = data_test_clean[rows_to_predict,])$predictions

    # Assign predictions back to the original test data frame
    data_test$y <- NA
    data_test$y[rows_to_predict] <- predictions

    # Combine results
    result <- bind_rows(
      data_train %>% mutate(imputed = "original"),
      data_test %>% mutate(imputed = "imputed")
    )

    names(result)[names(result) == "y"] <- target

    return(result)
  }

#' TODO
#'
#' @param data TBD
#' @param sensors_name TBD
#' @param transport_type TBD
#' @param threshold_uptime TBD
#'
#' @return TBD
#'
#' @export
#'
#' @importFrom dplyr left_join %>% select arrange
#'
#' @examples
#' 1+1
impute_missing_data <-
  function(data,
           sensors_name = NULL,
           transport_type = "vehicle",
           threshold_uptime = 0.5) {
    # Define constants
    BASE_VARS <-
      c(
        "day_of_month",
        "hour",
        "weekday",
        "month",
        "year",
        "vacation",
        "week_number",
        "segment_id",
        "date"
      )

    # Add minute column if interval is "quarterly"
    if (data$interval[1] == "quarterly") {
      BASE_VARS <- c(BASE_VARS, "minute")
    }

    # Validate and preprocess the input data
    data <-
      validate_and_preprocess_data(data, transport_type, sensors_name, BASE_VARS)

    # Impute data based on transport type
    if (transport_type == "all") {
      # Impute 'vehicle' and 'car' separately
      data_vehicle <-
        create_and_train_model(data, "vehicle", BASE_VARS, threshold_uptime)
      data_car <-
        create_and_train_model(data, "car", BASE_VARS, threshold_uptime)
      data_heavy <-
        create_and_train_model(data, "heavy", BASE_VARS, threshold_uptime)

      # Calculate 'heavy' as max(0, vehicle - car)
      data_complete <- data_vehicle %>%
        select(.data$vehicle,
               .data$segment_id,
               .data$date,
               .data$imputed) %>%
        left_join(
          data_car %>% select(.data$car, .data$segment_id, .data$date),
          by = c("segment_id", "date")
        ) %>%
        left_join(
          data_heavy %>% select(.data$heavy, .data$segment_id, .data$date),
          by = c("segment_id", "date")
        ) %>%
        left_join(
          data %>% select(-.data$car, -.data$vehicle, -.data$heavy),
          by = c("segment_id", "date")
        )

    } else {
      # For 'car' and 'vehicle' or 'heavy' , use the original method
      data_complete <-
        create_and_train_model(data, transport_type, BASE_VARS, threshold_uptime)
    }

    # Sort the final dataset
    data_complete <- data_complete %>%
      arrange(.data$segment_id, .data$date)

    return(data_complete)
  }
