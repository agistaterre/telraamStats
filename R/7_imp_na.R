# this function could use some rewriting and tidyeval syntaxe

#' Impute Missing Data using Linear Interpolation
#'
#' This function uses a linear interpolation method to fill in missing data in the road traffic data.
#' The interpolation is done over the day if no more than 4 hours of data in a row are missing,
#' and then over the days (grouped by the day of the week) if no more than 4 consecutive weeks
#' are missing.
#'
#' @param data Data frame. A data frame containing road traffic data.
#' @param period Character. The period to impute data from: "open days" for open days,
#' "public holidays" for public holidays, and "holidays" for holidays.
#'
#' @return A data frame with imputed data and a new column named "imputation" indicating
#' whether each data point was original (0), imputed (1), or NA (2).
#'
#' @importFrom dplyr mutate arrange filter all_of %>%
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom lubridate ymd
#' @importFrom zoo na.approx
#'
#' @export
#'
imp_na <- function(data, period = "open days"){

  # retrieve complete data
  data <- prep_comp_data(data)

  # separate datetime in date and time

  date <- data$date
  data <- mutate_date_time(data, date, "days", "hours")

  days <- data$days
  hours <- data$hours

  # collect period (days)
  ## public holidays
  date_pub_hol <- filter_public_holidays(data, "ONLY") %>%
    select(days) %>%
    distinct()
  date_pub_hol <- date_pub_hol[[1]]

  ## holidays
  date_hol <- filter_vacation(data, "ONLY") %>%
    select(days)%>%
    distinct()
  date_hol <- date_hol[[1]]

  ## open days
  date_od <- data%>%
    filter (!days %in% date_pub_hol, !days %in% date_hol) %>%
    select(days)%>%
    distinct()
  date_od <- date_od[[1]]

  speed_var_70 <- paste0("v",1:8,"_0to70")
  speed_var_120 <- paste0("v",1:25,"_0to120")

  #filter period
  if (period == "open days"){
    periode <- date_od
  }else if (period == "holidays"){
    periode <- date_hol
  }

  if (period == "public holidays"){

    periode <- date_pub_hol

    # imputation for all variable

    for (variable in c("heavy_lft", "heavy_rgt", "car_lft", "car_rgt", "bike_lft",
                       "bike_rgt", "pedestrian_lft","pedestrian_rgt", "v85",
                       speed_var_70, speed_var_120)){

      # create a data frame with days in lines and hours in columns
      donnee <- data %>%
        arrange(date) %>%
        filter (days %in% periode) %>%
        select(days, hours, all_of(variable)) %>%
        pivot_wider(names_from = hours, values_from = all_of(variable))

      names(donnee) <- c("days", "six_am", "seven_am", "eight_am", "nine_am", "ten_am",
                         "eleven_am", "midday", "one_pm", "two_pm", "three_pm", "four_pm",
                         "five_pm", "six_pm")

      # we use a linear interpolation and don't impute if we have more than four successive NA
      # first we impute in line and next in column

      for (var in c(2:14)){
        if (is.na(donnee[1, var])){
          donnee[1, var] <- mean(donnee[[var]], na.rm = TRUE)
        }else if(is.na(donnee[nrow(donnee), var])){
          donnee[nrow(donnee), var] <- mean(donnee[[var]], na.rm = TRUE)
        }
      }

      donnee <- replace(donnee, -1, t(na.approx(t(donnee[, -1]), maxgap = 3, na.rm = FALSE)))

      for (var in c(2:ncol(donnee))){
        donnee[, var] <- na.approx(donnee[, var], maxgap = 3, na.rm = FALSE)
      }

      # bring the data back in the original format
      imp_var <- donnee %>%
        pivot_longer(!days, names_to = "hours", values_to = variable) %>%
        select(all_of(variable))

      # replace NA by imupation
      index <- which(data$days %in% periode)
      data[index, variable] <- imp_var
    }


  }else{
    # imputation for all variable
    for (variable in c("heavy_lft", "heavy_rgt", "car_lft", "car_rgt", "bike_lft",
                       "bike_rgt", "pedestrian_lft","pedestrian_rgt", "v85",
                       speed_var_70, speed_var_120)){

      # create a data frame with days in lines and hours in columns
      donnee <- data %>% arrange(date) %>%
        filter (str_sub(date, 1, 10) %in% periode) %>%
        select(days, hours, all_of(variable)) %>%
        pivot_wider(names_from = hours, values_from = all_of(variable))

      names(donnee) <- c("days", "six_am", "seven_am", "eight_am", "nine_am", "ten_am",
                         "eleven_am", "midday", "one_pm", "two_pm", "three_pm", "four_pm",
                         "five_pm", "six_pm")

      # we use a linear interpolation and don't impute if we have more than four successive NA
      # first we impute in line and next in column
      final_df <- data.frame()
      for (day in c('monday','tuesday','wednesday',
                    'thursday','friday','saturday','sunday')){
        df <- donnee %>% filter(weekdays(ymd(days)) == day)
        df_complete <- df

        if(nrow(df != 0)){
          for (var in c(2:14)){
            if (is.na(donnee[1, var])){
              donnee[1, var] <- mean(donnee[[var]], na.rm = TRUE)
            }else if(is.na(donnee[nrow(donnee), var])){
              donnee[nrow(donnee), var] <- mean(donnee[[var]], na.rm = TRUE)
            }
          }

          df_complete <- replace(df_complete, -1, t(na.approx(t(df_complete[, -1]), maxgap = 3, na.rm = FALSE)))

          for (var in c(2:ncol(df))){
            df_complete[, var] <- na.approx(df_complete[, var], maxgap = 3, na.rm = FALSE)
          }
          final_df <- rbind(final_df, df_complete) %>%
            arrange(days)}
      }

      # bring the data back in the original format
      imp_var <- final_df %>%
        pivot_longer(!days, names_to = "hours", values_to = variable) %>%
        select(all_of(variable))

      # replace NA by imupation
      index <- which(data$days %in% periode)
      data[index, variable] <- imp_var
    }

  }

  # preparation of the data
  car_lft <- data$car_lft
  car_rgt <- data$car_rgt
  heavy_lft <- data$heavy_lft
  heavy_rgt <- data$heavy_rgt
  bike_lft <- data$bike_lft
  bike_rgt <- data$bike_rgt
  pedestrian_lft <- data$pedestrian_lft
  pedestrian_rgt <- data$pedestrian_rgt
  v1_0to70 <- data$v1_0to70
  v2_0to70 <- data$v2_0to70
  v3_0to70 <- data$v3_0to70
  v4_0to70 <- data$v4_0to70
  v5_0to70 <- data$v5_0to70
  v6_0to70 <- data$v6_0to70
  v7_0to70 <- data$v7_0to70
  v8_0to70 <- data$v8_0to70
  v1_0to120 <- data$v1_0to120
  v2_0to120 <- data$v2_0to120
  v3_0to120 <- data$v3_0to120
  v4_0to120 <- data$v4_0to120
  v5_0to120 <- data$v5_0to120
  v6_0to120 <- data$v6_0to120
  v7_0to120 <- data$v7_0to120
  v8_0to120 <- data$v8_0to120
  v9_0to120 <- data$v9_0to120
  v10_0to120 <- data$v10_0to120
  v11_0to120 <- data$v11_0to120
  v12_0to120 <- data$v12_0to120
  v13_0to120 <- data$v13_0to120
  v14_0to120 <- data$v14_0to120
  v15_0to120 <- data$v15_0to120
  v16_0to120 <- data$v16_0to120
  v17_0to120 <- data$v17_0to120
  v18_0to120 <- data$v18_0to120
  v19_0to120 <- data$v19_0to120
  v20_0to120 <- data$v20_0to120
  v21_0to120 <- data$v21_0to120
  v22_0to120 <- data$v22_0to120
  v23_0to120 <- data$v23_0to120
  v24_0to120 <- data$v24_0to120
  v25_0to120 <- data$v25_0to120

  # rebuild data
  data_complete <- data %>%
    mutate(car = car_lft + car_rgt,
           heavy = heavy_lft + heavy_rgt,
           bike = bike_lft + bike_rgt,
           pedestrian = pedestrian_lft + pedestrian_rgt,
           car_speed_hist_0to70plus = paste(v1_0to70, v2_0to70, v3_0to70,
                                            v4_0to70, v5_0to70, v6_0to70,
                                            v7_0to70, v8_0to70, sep = ","),
           car_speed_hist_0to120plus = paste(v1_0to120, v2_0to120, v3_0to120,
                                             v4_0to120, v5_0to120, v6_0to120,
                                             v7_0to120, v8_0to120, v9_0to120,
                                             v10_0to120, v11_0to120, v12_0to120,
                                             v13_0to120, v14_0to120, v15_0to120,
                                             v16_0to120, v17_0to120, v18_0to120,
                                             v19_0to120, v20_0to120, v21_0to120,
                                             v22_0to120, v23_0to120, v24_0to120,
                                             v25_0to120, sep = ","))

  # we used intermediate variables to impute our data, now we delete them
  data_complete <- data_complete %>%
    select(-c(all_of(speed_var_70), all_of(speed_var_120), days, hours))

  # transform uptime for ease graphs NA if its missing data, FALSE if its original data and TRUE
  # if its impute data

  data_complete[is.na(data_complete$car), "imputation"] <- NA
  data_complete[!is.na(data_complete$uptime), "imputation"] <- FALSE
  data_complete[is.na(data_complete$uptime) & !is.na(data_complete$car), "imputation"] <- TRUE

  return(data_complete)
}


#' Add day and hour from date
#'
#' @param data A data frame containing the input data.
#' @param date column date of data
#' @param days name of new column
#' @param hours name of new column
#'
#'
#' @importFrom dplyr mutate %>%
#' @importFrom rlang :=
#'
#' @noRd
#'
mutate_date_time <- function(data, date, days, hours) {

data <- data %>%
  mutate({{days}} := str_sub({{date}}, 1 ,10),
         {{hours}} := str_sub({{date}}, 12, 19))
}
