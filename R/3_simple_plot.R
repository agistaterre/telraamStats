#'  Number of cars and Heavies as a function of time
#'
#' @param data data.frame containing all the data for all your sensors
#' @param sensor numeric, Id of the chosen sensor
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01')
#'
#' @return graph showing the evolution of the number of cars and heavies over the selected date range for the chosen sensor.
#' @export
#'
simple_plot <- function(data, sensor, date_range){
  data <- filtering(data, sensor = sensor,  date_range = date_range, mobility = c('car','heavy'))

  # the returned graphic
  ggplot(data)+
    aes(x=date, y=.data$total) +
    geom_line(color="black")+
    geom_smooth(method='gam', formula=y ~ s(x, bs = "cs"),color="#B1D62E", size=2) + # trend
    ylab("Nombre de voitures et de poids lourds")+
    xlab("Date")+
    ggtitle("Nombre de voitures et de poids lourd en fonction du temps")+ # legend
    theme_bw() + # set the design of the chart
    theme(panel.background = element_rect(fill = "#F5F5F5"), # background color
          panel.grid = element_line(color = "#E3E3E3"), # grid color
          panel.border = element_rect(color = "#E3E3E3", size=2)) # border color and size
}
