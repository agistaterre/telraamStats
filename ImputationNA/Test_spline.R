# Charger les bibliothèques nécessaires
library(ggplot2)
library(prophet)
library(dplyr)
library(zoo)

# Générer la séquence de dates
ds = seq(as.Date("2017-01-01"), as.Date("2019-09-27"), by = "days")

# Définir la fonction y. Vous pouvez changer cette fonction selon vos besoins.
x = seq(1, length(ds), 1)
y= x^2*sin(x)

# Créer le data frame initial avec les dates et les valeurs de y
data = data.frame(ds = x, y = y[1:length(ds)])

# Tracer les données initiales
plot(data$ds, data$y, type = "l", main = "Données initiales")

# Introduire des trous de longueur croissante (1 à 72) à des positions différentes
set.seed(123) # Pour reproductibilité
for (i in 1:72) {
  start = sample(1:(nrow(data) - i), 1)
  end = start + round(i/2,0)
  data$y[start:end] = NA
}

plot(data$ds, data$y, type = "l", main = "Données avec valeurs manquantes")

# Imputation des données manquantes par interpolation locale avec spline
data_spline <- data
data_spline$y <- na.spline(data_spline$y)
plot(data_spline$ds, data_spline$y, type = "l", main = "Données imputées avec spline cubic ")

# Imputation des données manquantes par interpolation globale
data_global <- data
data_global$y <- na.approx(data_global$y)
plot(data_global$ds, data_global$y, type = "l", main = "Données imputées avec spline linear")


#Imputation des données manquantes par interpolation moyenne mobile
data_ma <- data
data_ma$y <- na.locf(data_ma$y)
plot(data_ma$ds, data_ma$y, type = "l", main = "Données imputées avec moyenne mobile")

#Imputation des données manquantes par interpolation moyenne mobile avec
data_ma2 <- data
data_ma2 <- impute_rolling_mean(data_ma2, 10)
plot(data_ma2$ds,data_ma2$y, type = "l", main = "Données imputées avec moyenne mobile")


# Fonction pour créer et enregistrer plusieurs graphiques montrant le comportement des imputations
plot_imputation <- function(data, step_size) {
  par(mfrow=c(3, 1))

  for (i in seq(1, 50, by = step_size)) {
    data_temp <- data
    z = 0
    for (j in 1:i) {
      start = sample(1:(nrow(data_temp) - z - 1), 1)
      end = start + z
      if(end <= nrow(data_temp)) {
        data_temp$y[start:end] = NA
      }
      z = z + 1
    }

    # Imputation des données manquantes par interpolation locale avec spline
    data_spline_temp <- data_temp
    data_spline_temp$y <- na.spline(data_spline_temp$y)

    # Imputation des données manquantes par interpolation globale
    data_global_temp <- data_temp
    data_global_temp$y <- na.approx(data_global_temp$y)

    # Tracer les données imputées
    plot(data_temp$ds, data_temp$y, type = "l", main = paste("Données avec", i, "valeurs manquantes"))
    lines(data_spline_temp$ds, data_spline_temp$y, col = "blue", lwd = 2)
    lines(data_global_temp$ds, data_global_temp$y, col = "red", lwd = 2)

    legend("topright", legend = c("Données originales", "Spline local", "Interpolation globale"),
           col = c("black", "blue", "red"), lty = 1, lwd = 2)
  }
}

# Créer et enregistrer les graphiques
plot_imputation(data, step_size = 5)
