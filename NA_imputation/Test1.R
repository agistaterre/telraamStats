pacman::p_load(tidyverse,)

data_net <- read.csv2("~/Desktop/Stage CREM/Code R/data/Donnee_brute.csv")


load("~/Desktop/Stage CREM/Code R/ApplyShiny/data/BoulevardLiberte-18.RData")


data$date <- as.Date(data$date, format = "%Y-%m-%d %H:%M:%S")
summary(data)


remotes::install_github('KetsiaGuichard/telraamStats')
library(telraamStats)



nrow(data[which(data$uptime < 0.5),])
nrow(data)
nrow(data[which(data$uptime < 0.5),]) / nrow(data) * 100


library(lubridate)
#Transform class data$date to date with lubridate
data$date <- ymd_hms(data$date)


#all data bellow 19h and above 5h
data2 <- data %>% filter(hour(date) < 19 & hour(date) > 5)


nrow(data2[which(data$uptime < 0.5),])
nrow(data2)
nrow(data2[which(data$uptime < 0.5),]) / nrow(data2) * 100
