#' Visualize speed proportion evolution with trafic
#'
#' Graphical representation (raw and smoothed curves) of the proportion of vehicles above 10, 20, 30 and 40 km/h per vehicles/hour
#'
#' @param ... data and parameters to filter on, see \link{filtering}
#'
#' @import ggplot2
#' @importFrom dplyr arrange %>%
#' @importFrom mgcv gam
#' @importFrom stats embed predict
#' @importFrom CPAT DE.test
#'
#' @return A list containing the curves, the result of the Darling-Erdös test performed on smoothed curves, and the processed data used for the curves
#'
#'
#' @export
#'
plot_speed <- function(...){
  data <- do.call(filtering, list(...)) %>% arrange(.data$total)
  label <- c("plus de 40km/h", "plus de 30km/h", "plus de 20km/h", "plus de 10km/h")

  if (is.null(data)){
    return(NULL)
  }

  data_speed <- do.call(rbind,
                        lapply(data$car_speed_hist_0to70plus,
                               function(i) {
                                 speed <- unlist(i)
                                 speed_cumsum <- cumsum(speed)
                                 c(speed_cumsum[1], speed_cumsum[2], speed_cumsum[3], speed_cumsum[4])
                               }
                        )
  ) %>% data.frame()
  colnames(data_speed) <- label

  count_car <- embed(data$total, 50) %>% apply(1, mean)
  # Sliding average calculation
  speed10 <- embed(data_speed$`plus de 10km/h`, 50) %>% apply(1, function(x) 100 - mean(x))
  speed20 <- embed(data_speed$`plus de 20km/h`, 50) %>% apply(1, function(x) 100 - mean(x))
  speed30 <- embed(data_speed$`plus de 30km/h`, 50) %>% apply(1, function(x) 100 - mean(x))
  speed40 <- embed(data_speed$`plus de 40km/h`, 50) %>% apply(1, function(x) 100 - mean(x))

  # Curve smoothing
  gam1 <- gam(speed10 ~ s(count_car, bs = "cs")) %>% predict(type='response')
  gam2 <- gam(speed20 ~ s(count_car, bs = "cs")) %>% predict(type='response')
  gam3 <- gam(speed30 ~ s(count_car, bs = "cs")) %>% predict(type='response')
  gam4 <- gam(speed40 ~ s(count_car, bs = "cs")) %>% predict(type='response')

  # the Darling-Erdös Test is performed on smoothed curves
  DE_test <- c(count_car[DE.test(gam4)$estimate],
               count_car[DE.test(gam3)$estimate],
               count_car[DE.test(gam2)$estimate],
               count_car[DE.test(gam1)$estimate])

  k <- length(count_car)

  # the final data table to be drawn
  data_plot <- data.frame("count" = rep(count_car, 4),
                          "percentage" = c(speed10, speed20, speed30, speed40),
                          "legend" = c(rep(label[1], k), rep(label[2], k), rep(label[3], k), rep(label[4], k)))

  # Preparing x axis indexing
  max_car <- max(data_plot$count)
  if(max_car<100){
    absi <- seq(0,100,10)
  }else{
    if(max_car<500){
      absi <- seq(0,500,50)

    }else{
      absi <- seq(0,floor(max_car/100)*100,100)

    }
  }

  chart <- ggplot(data_plot)+
    aes(x=.data$count, y=.data$percentage, color = .data$legend, group=.data$legend)+
    geom_line(color="black")+ geom_smooth(method='gam', formula=y ~ s(x, bs = "cs"))+
    labs(x="Nombre de vehicules sur une tranche horaire", y = "Pourcentage de vehicule depassant la vitesse donnees")+
    ggtitle("Evolution de la vitesse de conduite selon le nombre d'usagers") +
    scale_x_continuous(breaks=c(absi), labels=c(absi)) +
    labs(fill = "")+
    theme_bw()+
    theme(panel.background = element_rect(fill = "#F5F5F5"), # background color
          panel.grid = element_line(color = "#E3E3E3"), # grid color
          panel.border = element_rect(color = "#E3E3E3", size = 2)) # border color and size

  return(list(chart=chart,
              DE_test = DE_test,
              data_plot=data_plot))
}
