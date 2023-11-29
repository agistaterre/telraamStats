# If your Telraam token isn't in your environment variables
# please fill the argument of set_telraam_token function
if (!api_state()) {
  set_telraam_token('yourTelraamToken')
}

# Select segments in Châteaubourg city with most complete data for 2022
segments <- c(9000001844, 9000002453)
period <- c('2022-01-01', '2022-12-31')

# Get Data from API
traffic <- data.frame()
for (segment in segments) {
  traffic_tmp <-
    retrieve_sensor(segment, as.Date(period[1]), as.Date(period[2]))
  traffic <- rbind(traffic, traffic_tmp)
}

usethis::use_data(traffic, compress = 'xz', overwrite = T)
