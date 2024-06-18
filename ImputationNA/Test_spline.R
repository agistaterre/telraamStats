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

# Chargement des bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(zoo)
library(imputeTS)

# Génération d'un jeu de données avec des valeurs manquantes
set.seed(123) # Pour rendre la génération reproductible

# Générer une série temporelle quotidienne sur une année avec une tendance et une saisonnalité
dates <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2021-12-31"), by = "day")
values <- 50 + 0.1 * as.numeric(dates - as.Date("2021-01-01")) + 10 * sin(2 * pi * (as.numeric(dates - as.Date("2021-01-01")) / 365 * 12))

# Ajouter des valeurs manquantes aléatoires
values_missing <- values
missing_indices <- sample(1:length(values), size = 0.2 * length(values)) # 20% de valeurs manquantes
values_missing[missing_indices] <- NA

# Création du dataframe
data_missing <- data.frame(ds = dates, y = values_missing)

# Affichage du jeu de données avec les valeurs manquantes
ggplot(data_missing, aes(x = ds, y = y)) +
  geom_line(color = "blue") +
  geom_point(data = data_missing %>% filter(is.na(y)), aes(x = ds, y = y), color = "red") +
  ggtitle("Données avec valeurs manquantes") +
  xlab("Date") +
  ylab("Valeurs")

# Imputation des données manquantes avec spline cubique
data_imputed_spline <- data_missing
data_imputed_spline$y <- na.spline(data_missing$y)

# Imputation des données manquantes avec méthode linéaire
data_imputed_linear <- data_missing
data_imputed_linear$y <- na.approx(data_missing$y)

# Affichage des données imputées
G1 <- ggplot(data_missing, aes(x = ds, y = y)) +
  geom_line(color = "blue") +
  geom_point(data = data_missing %>% filter(is.na(y)), aes(x = ds, y = y), color = "red") +
  ggtitle("Données avec valeurs manquantes") +
  xlab("Date") +
  ylab("Valeurs")

G2 <- ggplot(data_imputed_spline, aes(x = ds, y = y)) +
  geom_line(color = "green") +
  ggtitle("Données imputées (Spline cubique)") +
  xlab("Date") +
  ylab("Valeurs")

G3 <- ggplot(data_imputed_linear, aes(x = ds, y = y)) +
  geom_line(color = "purple") +
  ggtitle("Données imputées (Linéaire)") +
  xlab("Date") +
  ylab("Valeurs")

library(gridExtra)
grid.arrange(G1, G2, G3, ncol = 1)


# Chargement des bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(zoo)
pacman::p_load(imputeTS)

# Génération d'un jeu de données avec des valeurs manquantes
set.seed(123) # Pour rendre la génération reproductible

# Générer une série temporelle quotidienne sur une année avec une tendance et une saisonnalité
dates <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2021-12-31"), by = "day")
values <- 50 + 0.1 * as.numeric(dates - as.Date("2021-01-01")) + 10 * sin(2 * pi * (as.numeric(dates - as.Date("2021-01-01")) / 365 * 12))

# Ajouter des valeurs manquantes aléatoires
values_missing <- values
missing_indices <- sample(1:length(values), size = 0.2 * length(values)) # 20% de valeurs manquantes
values_missing[missing_indices] <- NA

# Création du dataframe
data_missing <- data.frame(ds = dates, y = values_missing)

# Affichage du jeu de données avec les valeurs manquantes
ggplot(data_missing, aes(x = ds, y = y)) +
  geom_line(color = "blue") +
  geom_point(data = data_missing %>% filter(is.na(y)), aes(x = ds, y = y), color = "red") +
  ggtitle("Données avec valeurs manquantes") +
  xlab("Date") +
  ylab("Valeurs")

# Imputation des données manquantes avec spline cubique
data_imputed_spline <- data_missing
data_imputed_spline$y <- na.spline(data_missing$y)

# Imputation des données manquantes avec méthode linéaire
data_imputed_linear <- data_missing
data_imputed_linear$y <- na.interpolation(data_missing$y, option = "linear")

# Affichage des données imputées
G1 <- ggplot(data_missing, aes(x = ds, y = y)) +
  geom_line(color = "blue") +
  geom_point(data = data_missing %>% filter(is.na(y)), aes(x = ds, y = y), color = "red") +
  ggtitle("Données avec valeurs manquantes") +
  xlab("Date") +
  ylab("Valeurs")

G2 <- ggplot(data_imputed_spline, aes(x = ds, y = y)) +
  geom_line(color = "green") +
  ggtitle("Données imputées (Spline cubique)") +
  xlab("Date") +
  ylab("Valeurs")

G3 <- ggplot(data_imputed_linear, aes(x = ds, y = y)) +
  geom_line(color = "purple") +
  ggtitle("Données imputées (Linéaire)") +
  xlab("Date") +
  ylab("Valeurs")

library(gridExtra)
grid.arrange(G1, G2, G3, ncol = 1)










library(ggplot2)
library(dplyr)
library(zoo)

# Créer des données de test avec des valeurs manquantes
set.seed(123)
ds <- seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 100)
y <- sin(1:100 / 10) + rnorm(100, 0, 0.1)
y[c(10, 20, 30, 40, 50)] <- NA # Introduire des valeurs manquantes

data_missing <- data.frame(ds, y)

# Afficher les données originales avec valeurs manquantes
ggplot(data_missing, aes(x = ds, y = y)) +
  geom_point(color = "red") +
  ggtitle("Données avec valeurs manquantes") +
  xlab("Date") +
  ylab("Valeurs")


# Imputer les données manquantes avec les fenêtres glissantes
window_size <- 5

data_imputed_rolling <- data_missing %>%
  mutate(y_imputed = na.locf(zoo::rollapply(y, width = window_size, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")))

# Afficher les données imputées
ggplot(data_imputed_rolling, aes(x = ds, y = y_imputed)) +
  geom_point(color = "blue") +
  ggtitle("Données imputées (Rolling Window)") +
  xlab("Date") +
  ylab("Valeurs")




# Comparer les données originales et imputées
ggplot() +
  geom_point(data = data_missing, aes(x = ds, y = y), color = "blue") +
  geom_point(data = data_imputed_rolling, aes(x = ds, y = y_imputed), color = "red") +
  ggtitle("Comparaison des données originales et imputées (Rolling Window)") +
  xlab("Date") +
  ylab("Valeurs") +
  theme_minimal()



# Installer et charger le package imputeTS
install.packages("imputeTS")
library(imputeTS)

# Générer une série temporelle avec des valeurs manquantes
set.seed(123)
data <- ts(rnorm(100), frequency = 12)
data[c(5, 20:25, 33, 50:60, 70)] <- NA  # Introduire des valeurs manquantes

# Afficher la série temporelle avec des valeurs manquantes
plot(data, main = "Série temporelle avec valeurs manquantes", col = "red")

# Appliquer la méthode de Kalman pour l'imputation des valeurs manquantes
data_imputed <- na_kalman(data, model = "StructTS")

# Afficher la série temporelle après imputation
plot(data_imputed, main = "Série temporelle après imputation avec Kalman", col = "blue")

# Comparer les deux graphiques
par(mfrow = c(2, 1))
plot(data, main = "Série temporelle avec valeurs manquantes", col = "red")
plot(data_imputed, main = "Série temporelle après imputation avec Kalman", col = "blue")
par(mfrow = c(1, 1))
# Charger les librairies nécessaires
library(ggplot2)

# Générer les données sinus
set.seed(123)
x <- seq(0, 2 * pi, length.out = 100)
y <- sin(x)

# Introduire des valeurs manquantes
missing_indices <- sample(1:100, 10)
y[missing_indices] <- NA

# Fonction pour imputer les valeurs manquantes par moyenne mobile et identifier les voisins
impute_ma <- function(y, window_size = 4) {
  y_imputed <- y
  neighbors_list <- list()
  half_window <- window_size %/% 2

  for (i in seq_along(y)) {
    if (is.na(y[i])) {
      # Trouver les indices des points voisins
      neighbors <- c((i-half_window):(i-1), (i+1):(i+half_window))
      neighbors <- neighbors[neighbors > 0 & neighbors <= length(y)]

      # Calculer la moyenne des points voisins non manquants
      y_imputed[i] <- mean(y[neighbors], na.rm = TRUE)

      # Enregistrer les voisins pour visualisation
      neighbors_list[[i]] <- neighbors
    }
  }
  return(list(y_imputed = y_imputed, neighbors_list = neighbors_list))
}

# Imputer les valeurs manquantes
imputed_result <- impute_ma(y)
y_imputed <- imputed_result$y_imputed
neighbors_list <- imputed_result$neighbors_list

# Créer un data frame pour ggplot
data <- data.frame(
  x = x,
  y = y,
  y_imputed = y_imputed,
  color = "Original Points"
)

# Marquer les points imputés en rouge et leurs voisins en vert
for (i in missing_indices) {
  data$color[i] <- "Imputed Points"
  if (!is.null(neighbors_list[[i]])) {
    data$color[neighbors_list[[i]]] <- "Neighbor Points"
  }
}

# Visualisation
ggplot(data, aes(x = x)) +
  geom_line(aes(y = y), color = "blue") +
  geom_point(aes(y = y, color = color), size = 2) +
  geom_point(data = subset(data, !is.na(y) & !is.na(y_imputed) & color == "Neighbor Points"),
             aes(y = y), color = "green", size = 2) +
  geom_point(data = subset(data, is.na(y)), aes(y = y_imputed), color = "red", size = 2) +
  scale_color_manual(values = c("Original Points" = "black", "Neighbor Points" = "green", "Imputed Points" = "red")) +
  labs(title = "Imputation des Données par Moyenne Mobile pour une fenêtre de 4 points",
       x = "X",
       y = "Y (sin(x))",
       color = "Legend") +
  theme_minimal()
