# Charger les bibliothèques nécessaires
library(ggplot2)
library(prophet)
library(dplyr)

# Générer la séquence de dates
ds = seq(as.Date("2017-01-01"), as.Date("2019-09-27"), by = "days")

# Définir la fonction y. Vous pouvez changer cette fonction selon vos besoins.
x = seq(1, length(ds), 1)
#y = x * sin(x^2)
y= x^2*sin(x)

# Créer le data frame initial avec les dates et les valeurs de y
data = data.frame(ds = ds, y = y)

# Tracer les données initiales
plot(data$ds, data$y, type = "l", main = "Données initiales")

# Introduire des trous aléatoires dans les données (entre 1 et 30 valeurs consécutives manquantes)
set.seed(123) # Pour reproductibilité
for (i in 1:10) {
  start = sample(1:(nrow(data) - 30), 1)
  end = start + sample(1:50, 1)
  data$y[start:end] = NA
}

# Récupérer les dates des valeurs supprimées
data_na = data[is.na(data$y),]

# Tracer les données avec valeurs manquantes
plot(data$ds, data$y, type = "l", main = "Données avec valeurs manquantes")

# Construire et ajuster le modèle Prophet
model <- prophet(data.frame(ds = data$ds, y = data$y),seasonality.prior.scale=100,n.changepoints = 25)

# Prédire les valeurs sur les dates initiales
ajust <- predict(model, data.frame(ds = data$ds))
fitting_curve <- plot(model, ajust)
print(fitting_curve)

# Prédire les valeurs manquantes
pred <- predict(model, data_na)

# Préparer les données pour l'affichage
dpred <- pred %>%
  select(ds, yhat) %>%
  mutate(type = "pred") %>%
  rename(y = yhat)

dtrain <- data %>%
  na.omit() %>%
  select(ds, y) %>%
  mutate(type = "real")

# Convertir les colonnes ds en format POSIXct
dtrain$ds <- as.POSIXct(dtrain$ds)
dpred$ds <- as.POSIXct(dpred$ds)

# Combiner les données réelles et prédites
data_pred <- rbind(dtrain, dpred) %>%
  arrange(ds)

# Afficher l'imputation en traçant les données réelles et les données imputées
ggplot(data_pred, aes(x = ds, y = y)) +
  geom_line() +
  geom_point(aes(color = type)) +
  ggtitle("Imputation de données manquantes") +
  xlab("Date") +
  ylab("Valeurs") +
  theme(legend.position = "none")

#add_changepoints_to_plot(fitting_curve, model, threshold = 0.5)

#Cross validation
cv <- cross_validation(model, initial = 730, period =  180, horizon = 90  ,units = "days")
print(cv)

#Performance metrics
performance_metrics(cv)

plot_cross_validation_metric(cv, metric = "mape")
plot_cross_validation_metric(cv, metric = "rmse")
plot_cross_validation_metric(cv, metric = "mae")
plot_cross_validation_metric(cv, metric = "coverage")



#Imputation avec Spline Cubique

data_spline <- na.approx(data, na.rm = FALSE)

# Afficher l'imputation en traçant les données réelles et les données imputées
ggplot(data_spline, aes(x = ds, y = y)) +
  geom_line() +
  ggtitle("Imputation de données manquantes avec Spline Cubique") +
  xlab("Date") +
  ylab("Valeurs")



