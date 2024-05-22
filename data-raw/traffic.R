# If your Telraam token isn't in your environment variables
# please fill the argument of set_telraam_token function
if (!get_api_state()) {
  set_telraam_token('yourTelraamToken')
}

# Select segments in Châteaubourg city with most complete data for 2022
segments <- c("RteVitre-06" = 9000001844, "ParisArcEnCiel-05" = 9000002453)
period <- as.Date(c('2022-01-01', '2022-12-31'))

# Get Data from API
traffic <- data.frame()
for (segment in segments) {
  print(segment)
  traffic_tmp <-
    retrieve_sensor(segment, period[1], period[2])
  traffic <- rbind(traffic, traffic_tmp)
}

usethis::use_data(traffic, compress = 'xz', overwrite = T)
