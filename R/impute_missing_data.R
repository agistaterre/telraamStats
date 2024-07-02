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
#' @examples
#' data <- validate_and_preprocess_data(data = traffic,
#'                                      transport_type = "car",
#'                                      sensors_id = 9000001844,
#'                                      base_vars = c("day_of_month","hour","weekday","month","year","vacation","week_number","segment_id","date"))
#'

validate_and_preprocess_data <-
  function(data,
           transport_type,
           sensors_id,
           base_vars) {
    # Define constants
    valid_transport_types <- c("car", "vehicle", "heavy", "all")
    vehicle_vars <- c("car", "heavy")

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

    # Add minute column if interval is "quarterly"
    if (data$interval[1] == "quarterly") {
      data <- data %>% mutate(minute = minute(.data$date))
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


#' Create and train Random Forest model for traffic imputation
#'
#' This function creates and trains a Random Forest model for imputing missing traffic data.
#'
#' @param data A preprocessed data frame containing traffic data.
#' @param target Character. The target variable to impute.
#' @param base_vars Character vector. Base variables used for prediction.
#' @param threshold_uptime Numeric. Threshold for uptime to determine missing values.
#'
#' @return A data frame with imputed values and imputation flags.
#'
#' @importFrom dplyr select mutate bind_rows %>%
#' @importFrom stats complete.cases na.omit predict
#' @importFrom ranger ranger
#'
#'
#' @examples
#' data <- validate_and_preprocess_data(data = traffic,
#'                                      transport_type = "car",
#'                                      sensors_id = 9000001844,
#'                                      base_vars = c("day_of_month","hour","weekday","month","year","vacation","week_number","segment_id","date") )
#'
#' data <- create_and_train_model(data = data,
#'                             target = "car",
#'                             base_vars = c("day_of_month","hour","weekday","month","year","vacation","week_number","segment_id","date"),
#'                             threshold_uptime = 0.5)
#'
#'
create_and_train_model <-
  function(data, target, base_vars, threshold_uptime) {
    # Prepare data for Random Forest
    data_rf <- data %>%
      mutate(y = ifelse(.data$uptime < threshold_uptime,
                        NA,!!sym(target))) %>% select(-!!sym(target))

    # Split data into training and test sets
    is_train <- !is.na(data_rf$y)
    data_train <- data_rf[is_train,]
    data_imput <- data_rf[!is_train,]

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
    data_imput_clean <- data_imput %>%
      select(all_of(base_vars))
    rows_to_predict <- complete.cases(data_imput_clean)


    #Make sure that the data is not empty
    if (sum(rows_to_predict) == 0) {
      stop("No data to impute")
    }

    # Make predictions only for complete cases
    predictions <-
      predict(model_rf, data = data_imput_clean[rows_to_predict,])$predictions

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
#'Time interval could be hourly or quarterly. The function will automatically detect the time interval based on the data and add a minute variable if its quarterly.
#'
#' @examples
#' traffic_clean <- retrieve_missing_data(traffic)
#'  traffic_imputed <- impute_missing_data(traffic_clean,
#'    sensors_id = 9000001844,
#'    transport_type = "vehicle",
#'    threshold_uptime = 0.5)
#'  table(traffic_imputed$imputed)
#'

impute_missing_data <-
  function(data,
           sensors_id = NULL,
           transport_type = "vehicle",
           threshold_uptime = 0.5) {
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

    # Add minute column if interval is "quarterly"
    if (data$interval[1] == "quarterly") {
      base_vars <- c(base_vars, "minute")
    }

    # Validate and preprocess the input data
    data <-
      validate_and_preprocess_data(data, transport_type, sensors_id, base_vars)

    # Impute data based on transport type
    if (transport_type == "all") {
      # Impute 'vehicle' and 'car' separately
      data_vehicle <-
        create_and_train_model(data, "vehicle", base_vars, threshold_uptime)
      data_car <-
        create_and_train_model(data, "car", base_vars, threshold_uptime)
      data_heavy <-
        create_and_train_model(data, "heavy", base_vars, threshold_uptime)

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
      # For 'car' ,'vehicle' or 'heavy' , use the original method
      data_complete <-
        create_and_train_model(data, transport_type, base_vars, threshold_uptime)
    }

    # Sort the final dataset
    data_complete <- data_complete %>%
      arrange(.data$segment_id, .data$date)

    return(data_complete)
  }
