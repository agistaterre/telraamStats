#' Validate and preprocess data for traffic analysis
#'
#' This function validates and preprocesses the input data for traffic analysis.
#'
#' @param data A data frame containing the traffic data.
#' @param transport_type Character. Type of transport to impute.
#' @param sensors_id Character vector. Id of the sensors to include in the analysis.
#' @param base_vars Character vector. Base variables required for the analysis.
#'
#' @return A preprocessed data frame ready for imputation.
#'
#' @importFrom lubridate day hour month year week wday minute
#' @importFrom dplyr mutate %>%
#'
#' @keywords internal
#' @export
#'
#' @examples
#' data <- validate_and_preprocess_data(data = traffic,
#'                                      transport_type = "car",
#'                                      sensors_id = 9000001844,
#'                                      base_vars = c("day_of_month", "hour", "weekday",
#'                                                   "month", "year", "vacation",
#'                                                   "week_number", "segment_id", "date"))

validate_and_preprocess_data <- function(data,
                                         transport_type,
                                         sensors_id,
                                         base_vars,
                                         add_vars = NULL,
                                         threshold_uptime = 0.5) {
  # Define constants
  valid_transport_types <- c("car", "vehicle", "heavy", "all")
  vehicle_vars <- c("car", "heavy")

  # Add missing additional variables to the model
  if (!is.null(add_vars)) {
    if (!is.character(add_vars)) {
      stop("add_vars must be a character vector")
    }
    if ((any(add_vars %in% base_vars))) {
      stop(
        "add_vars must be different from base_vars : day_of_month,hour,weekday,month,year,vacation,week_number,segment_id "
      )
    }
    if (!all(add_vars %in% colnames(data))) {
      stop("add_vars must be present in the data")
    }

    base_vars <- c(base_vars, add_vars)
  }

  # Add minute column if interval is "quarterly"
  if (data$interval[1] == "quarterly") {
    base_vars <- c(base_vars, "minute")
    data <- data %>% mutate(minute = minute(.data$date))

  }

  # Validate input parameters
  if (!transport_type %in% valid_transport_types) {
    stop(sprintf(
      "Unrecognized transport type. Valid options are: %s",
      paste(valid_transport_types, collapse = ", ")
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

  # Define required variables for the model
  required_vars <- c(base_vars, vehicle_vars)

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
    data <- data[data$segment_id %in% sensors_id, ]
  }

  # Convert data types to the correct format
  data <- data %>%
    mutate(
      day_of_month = as.numeric(.data$day_of_month),
      hour = as.numeric(.data$hour),
      weekday = as.factor(.data$weekday),
      month = as.factor(.data$month),
      year = as.numeric(.data$year),
      vacation = if (is.factor(.data$vacation)) {
        factor(as.character(.data$vacation),
               levels = unique(as.character(.data$vacation)))
      } else if (is.list(.data$vacation)) {
        factor(sapply(.data$vacation, as.character), levels = unique(sapply(.data$vacation, as.character)))
      } else {
        factor(as.character(.data$vacation), levels = unique(as.character(.data$vacation)))
      },
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
  if (!"vehicle" %in% colnames(data) &&
      transport_type %in% c("vehicle", "all")) {
    data <- data %>%
      mutate(vehicle = .data$car + .data$heavy)
  }

  # Prepare data for model training
  if (transport_type != "all") {
    data <- data %>%
      mutate(y = ifelse(.data$uptime < threshold_uptime,
                        NA, !!sym(transport_type))) %>% select(-!!sym(transport_type))
  }
  else {
    data <- data %>%
      mutate(
        vehicle = ifelse(.data$uptime < threshold_uptime, NA, .data$car + .data$heavy),
        car = ifelse(.data$uptime < threshold_uptime, NA, .data$car),
        heavy = ifelse(.data$uptime < threshold_uptime, NA, .data$heavy)
      )
  }

  return(data)
}


#' Create and train Random Forest model for traffic imputation
#'
#' This function creates and trains a Random Forest model for imputing missing traffic data.
#'
#' @param data_rf A preprocessed data frame containing traffic data.
#' @param target Character. The target variable to impute.
#' @param base_vars Character vector. Base variables used for prediction.
#' @param threshold_uptime Numeric. Threshold for uptime to determine missing values.
#' @param min.node.size Integer or NULL. Minimum node size for the Random Forest model. If NULL, the default value of 5 for regression is used.
#' @param mtry Integer or NULL. Number of variables randomly sampled as candidates at each split. If NULL, the default value (number of variables / 3 for regression) is used.
#'
#' @return A data frame with imputed values and imputation flags.
#'
#' @importFrom dplyr select mutate bind_rows %>%
#' @importFrom stats complete.cases na.omit predict
#' @importFrom ranger ranger
#'
#' @keywords internal
#' @export
#'
#' @examples
#' base_vars <- c("day_of_month", "hour", "weekday", "month", "year",
#'                "vacation", "week_number", "segment_id", "date")
#' data <- validate_and_preprocess_data(data = traffic,
#'                                      transport_type = "car",
#'                                      sensors_id = 9000001844,
#'                                      base_vars = base_vars)
#' data <- create_and_train_model(data = data,
#'                                target = "car",
#'                                base_vars = base_vars,
#'                                threshold_uptime = 0.5)


create_and_train_model <-
  function(data_rf,
           target,
           base_vars,
           threshold_uptime,
           min.node.size = NULL,
           mtry = NULL,
           num_trees = 500) {
    # Split data into training and test sets
    is_train <- !is.na(data_rf$y)
    data_train <- data_rf[is_train, ]
    data_imput <- data_rf[!is_train, ]

    # Remove NA from training data
    data_train_clean <- data_train %>%
      select(.data$y, all_of(base_vars)) %>%
      na.omit()

    # Train Random Forest model
    model_rf <-
      ranger(
        y ~ . - date,
        data = data_train_clean,
        mtry = mtry,
        min.node.size = min.node.size,
        num.trees = num_trees
      )


    # Prepare test data, keeping track of removed rows
    data_imput_clean <- data_imput %>%
      select(all_of(base_vars))
    rows_to_predict <- complete.cases(data_imput_clean)


    #Make sure that the data is not empty
    if (sum(rows_to_predict) == 0) {
      stop("No data to impute")
    }

    # Make predictions only for complete cases
    predictions <-
      predict(model_rf, data = data_imput_clean[rows_to_predict, ])$predictions

    # Assign predictions back to the original test data frame
    data_imput$y <- NA
    data_imput$y[rows_to_predict] <- predictions

    # Combine results
    result <- bind_rows(
      data_train %>% mutate(imputed = "original"),
      data_imput %>% mutate(imputed = "imputed")
    )

    names(result)[names(result) == "y"] <- target

    return(result)
  }

#' Impute missing data for traffic analysis
#'
#' This function imputes missing data for traffic analysis using Random Forest models.
#'
#' @param data A data frame containing the traffic data.
#' @param sensors_id Character vector. Id of the sensors to include in the analysis. Default is NULL (all sensors).
#' @param transport_type Character. Type of transport to impute. Options are "car", "vehicle", "heavy", or "all". Default is "vehicle".
#' @param threshold_uptime Numeric. Threshold for uptime to determine missing values. Default is 0.5.
#' @param add_vars Character vector. Additional variables used for prediction. Default is NULL.
#' @param min.node.size Integer or NULL. Minimum node size for the Random Forest model. If NULL, the default value of 5 for regression is used.
#' @param mtry Integer or NULL. Number of variables randomly sampled as candidates at each split. If NULL, the default value (number of variables / 3 for regression) is used.
#' @param num_trees Integer. Number of trees to grow in the Random Forest model. Default is 500.
#'
#' @return A data frame with imputed values for the specified transport type and a new column indicating whether the values were imputed or original.
#'
#' @importFrom dplyr left_join %>% select arrange
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
#' Time interval could be hourly or quarterly. The function will automatically detect the time interval based on the data and add a minute variable if it's quarterly.
#'
#' @export
#' @examples \dontrun{
#'  traffic_clean <- retrieve_missing_data(traffic,remove_data = TRUE,successive_day = 2)
#'  traffic_imputed <- impute_missing_data(traffic,
#'    sensors_id = 9000001844,
#'    transport_type = "vehicle",
#'    threshold_uptime = 0.5)
#'  table(traffic_imputed$imputed)
#'}

impute_missing_data <-
  function(data,
           sensors_id = NULL,
           transport_type = "vehicle",
           threshold_uptime = 0.5,
           add_vars = NULL,
           min.node.size = NULL,
           mtry = NULL,
           num_trees = 500) {
    # Define constants
    base_vars <-
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

    # Validate and preprocess the input data
    data <-
      validate_and_preprocess_data(
        data = data,
        transport_type = transport_type,
        sensors_id = sensors_id,
        base_vars = base_vars,
        add_vars = add_vars,
        threshold_uptime = threshold_uptime
      )


    # Impute data based on transport type
    if (transport_type == "all") {
      # Impute 'vehicle' and 'car' ,'heavy' separately
      data_vehicle <- data %>% rename(y = .data$vehicle)
      data_vehicle <-
        create_and_train_model(
          data_vehicle,
          "vehicle",
          base_vars,
          threshold_uptime,
          min.node.size = min.node.size,
          mtry = mtry,
          num_trees = num_trees
        )
      data_car <- data %>% rename(y = .data$car)
      data_car <-
        create_and_train_model(
          data_car,
          "car",
          base_vars,
          threshold_uptime,
          min.node.size = min.node.size,
          mtry = mtry,
          num_trees = num_trees
        )

      data_heavy <- data %>% rename(y = .data$heavy)
      data_heavy <-
        create_and_train_model(
          data_heavy,
          "heavy",
          base_vars,
          threshold_uptime,
          min.node.size = min.node.size,
          mtry = mtry,
          num_trees = num_trees
        )

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
          data %>% select(-.data$car,-.data$vehicle,-.data$heavy),
          by = c("segment_id", "date")
        )

    } else {
      # For 'car' ,'vehicle' or 'heavy' , use the original method
      data_complete <-
        create_and_train_model(
          data,
          transport_type,
          base_vars,
          threshold_uptime,
          min.node.size = min.node.size,
          mtry = mtry,
          num_trees = num_trees
        )
    }

    # Sort the final dataset
    data_complete <- data_complete %>%
      arrange(.data$segment_id, .data$date)

    return(data_complete)
  }

#' Fine-tune Random Forest parameters for traffic data imputation
#'
#' This function fine-tunes Random Forest parameters using Out-Of-Bag (OOB) validation to find the best hyperparameters for imputing missing traffic data.
#'
#' @param data A data frame containing the traffic data.
#' @param target_col The name of the target column to impute. Default is "vehicle".
#' @param sensors_id An optional vector of sensor IDs to filter the data. Default is NULL (all sensors).
#' @param mtry_range A vector of mtry values to test.
#' @param min_n_range A vector of min.node.size values to test.
#' @param num_trees Integer. Number of trees to grow in the forest (default: 500).
#' @param add_vars An optional vector of additional variables to include in the model. Default is NULL.
#' @param threshold_uptime Numeric. The minimum uptime threshold for data inclusion (default: 0.5).
#'
#' @return A list containing the best parameters and metrics.
#'
#' @import ranger
#' @import dplyr
#' @import utils
#'
#' @export
#' @examples \dontrun{
#' fine_tune_impute_missing_data(data = traffic, target_col = "vehicle",
#'                         mtry_range = c(2, 4, 6, 8),
#'                         min_n_range = c(1, 5, 10, 20),
#'                         threshold_uptime = 0.5)
#' }

fine_tune_impute_missing_data <-
  function(data,
           target_col = "vehicle",
           sensors_id = NULL,
           mtry_range,
           min_n_range,
           num_trees = 500,
           add_vars = NULL,
           threshold_uptime = 0.5) {
    # Define base variables
    base_vars <- c(
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

    # Validate and preprocess the input data
    data <- validate_and_preprocess_data(
      data = data,
      transport_type = target_col,
      sensors_id = sensors_id,
      base_vars = base_vars,
      add_vars = add_vars,
      threshold_uptime = threshold_uptime
    )

    #remove date from base_vars
    base_vars <- base_vars[!base_vars %in% c("date")]

    #Create the training data
    data <- data %>%
      select(all_of(base_vars), "y") %>% na.omit()


    # Create the grid of hyperparameters to search
    rf_grid <- expand.grid(mtry = mtry_range,
                           min_n = min_n_range)

    # Function to train and evaluate a single model
    evaluate_model <- function(mtry, min_n) {
      tryCatch({
        model <- ranger(
          formula = y ~ .,
          data = data,
          num.trees = num_trees,
          mtry = mtry,
          min.node.size = min_n,
          importance = 'impurity',
          oob.error = TRUE
        )
        return(tibble(
          mtry = mtry,
          min_n = min_n,
          "RMSE" = sqrt(model$prediction.error)
        ))
      }, error = function(e) {
        warning(paste(
          "Error in model with mtry =",
          mtry,
          "and min_n =",
          min_n,
          ":",
          e$message
        ))
        return(NULL)
      })
    }

    # Evaluate all combinations
    results <-
      purrr::map2_dfr(rf_grid$mtry, rf_grid$min_n, evaluate_model)

    # Check if all RMSE values are the same
    if (length(unique(results$RMSE)) == 1) {
      warning("All RMSE values are identical. Returning the first set of parameters.")
      best_params <- results[1,]
    } else {
      # Find best parameters
      best_params <- results %>%
        arrange("RMSE") %>%
        slice(1)
    }

    return(list(best_params = best_params, all_results = results))
  }
