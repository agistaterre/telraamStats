#' Plot decomposed data
#'
#' @param data Data frame containing the data
#' @param sensor1 The segment ID for sensor 1
#' @param sensor2 The segment ID for sensor 2
#' @param hour_x The hour to filter on
#' @param direction1 The direction for sensor 1
#' @param direction2 The direction for sensor 2
#' @param mobility A character vector indicating the mobility types to include in the filter
#' @param norm A character indicating whether to normalize the results
#' @param vacations vacations period for filtering function
#' @param public_holidays public holidays period for filtering function
#'
#'
#' @import ggplot2
#'
#' @return A list of plots including trend, seasonal, random, peaks, and correlation
#'
#' @export
#'
plot_deseas <- function(data, sensor1,sensor2, hour_x, direction1, direction2, mobility, norm, vacations=NULL, public_holidays=NULL){
  decompose_2 <- decompose_2_fct(data, sensor1,sensor2, hour_x, direction1, direction2, mobility, norm, vacations=NULL, public_holidays=NULL)
  if (is.null(decompose_2)){
    return(NULL)
  }
  trend_random <- decompose_2[['trend_random']]
  seas <- decompose_2[['seas']]

  if (norm=="YES"){
    put_ticks <- labs(x = "Date", y= " ") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  } else {
    put_ticks  <- labs(x = "Date", y="Variation en nombre de vehicules")
  }

  graphiques <- list(trend = NULL, seasonal = NULL,random = NULL,
                     peaks = decompose_2$peaks, correl = decompose_2$correl, export_d = decompose_2$export_d)
  # seasonal
  graphiques$seasonal <- ggplot(seas) + geom_line(aes(x=date,y=.data$seas,col=.data$sensor),size=1)+
    scale_x_continuous(breaks = 1:7,
                       labels = c('monday','tuesday','wednesday',
                                  'thursday','friday','saturday','sunday')) +
    ggtitle('Seasonal')+
    put_ticks+
    theme_bw()+
    theme(panel.background = element_rect(fill = "#F5F5F5"),
          panel.grid = element_line(color = "#E3E3E3"),
          panel.border = element_rect(color = "#E3E3E3", size = 2))
  # trend
  graphiques$trend <- ggplot(trend_random, aes(x = date)) +
    geom_line(aes(y=.data$base,col=.data$sensor), alpha = 0.4) +
    geom_line(aes(y=.data$trend,col=.data$sensor),size=1) +
    ggtitle('Trend')+
    put_ticks+
    theme_bw()+
    theme(panel.background = element_rect(fill = "#F5F5F5"),
          panel.grid = element_line(color = "#E3E3E3"),
          panel.border = element_rect(color = "#E3E3E3", size = 2))
  # random
  graphiques$random <- ggplot(trend_random) +
    geom_line(aes(x=date,y=.data$random,col=.data$sensor),size=1)+
    ggtitle('Random') +
    put_ticks+
    theme_bw()+
    theme(panel.background = element_rect(fill = "#F5F5F5"),
          panel.grid = element_line(color = "#E3E3E3"),
          panel.border = element_rect(color = "#E3E3E3", size = 2))


  return(graphiques)
}
