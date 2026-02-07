install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("tidyverse")  

library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)

# Nettoyage de l'environnement
rm(list = ls())

# Chargement des bibliothèques
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)

###############################################
# 1. PARAMÈTRES GÉNÉRAUX
###############################################

# Tickers Yahoo qui fonctionnent
indices_FR <- c(
  CAC40 = "^FCHI",          # CAC 40 standard
  SBF120 = "^SBF120"        # SBF 120
)

indices_US <- c(
  SP500 = "^GSPC",          # S&P 500
  SP500_TR = "^SP500TR"     # S&P 500 Total Return
)

# Regroupement
tickers <- c(indices_FR, indices_US)

start_date <- "2014-01-01"
end_date   <- "2024-12-31"

###############################################
# 2. EXTRACTION DES PRIX AJUSTÉS
###############################################

# Téléchargement des données
getSymbols(
  tickers,
  src   = "yahoo",
  from  = start_date,
  to    = end_date,
  auto.assign = TRUE
)

# Vérification des objets créés
print(ls())

# Extraction des prix de clôture ajustés
prices_list <- list()

for(ticker in names(tickers)) {
  # Vérifier si l'objet existe
  if(exists(ticker)) {
    prices_list[[ticker]] <- Ad(get(ticker))
  } else {
    message(paste("Avertissement :", ticker, "n'a pas été téléchargé"))
  }
}

# Combiner toutes les séries
prices <- do.call(merge, prices_list)
colnames(prices) <- names(tickers)

# Suppression des lignes avec NA
prices <- na.omit(prices)

###############################################
# 3. CALCUL DES RENDEMENTS LOG
###############################################

# Rendements logarithmiques quotidiens
returns <- diff(log(prices))
returns <- na.omit(returns)

###############################################
# 4. MATRICE VARIANCE-COVARIANCE
###############################################

# Matrice variance-covariance empirique
Sigma <- cov(returns)

# Vérification des dimensions
print(paste("Dimensions de Sigma :", dim(Sigma)[1], "x", dim(Sigma)[2]))

###############################################
# 5. PARAMÈTRES POUR MARKOWITZ
###############################################

# Nombre d'actifs
N <- ncol(returns)

# Rendements moyens (quotidiens)
mu <- colMeans(returns)

# Conversion en rendements annuels (approximatif, 252 jours)
mu_annual <- mu * 252
Sigma_annual <- Sigma * 252

###############################################
# 6. FRONTIÈRE D'EFFICIENCE (MARKOWITZ)
###############################################

# Utiliser les paramètres annuels pour la frontière
mu_f <- mu_annual
Sigma_f <- Sigma_annual

# Séquence de rendements cibles
target_returns <- seq(
  min(mu_f),
  max(mu_f),
  length.out = 100
)

# Matrices utiles
ones <- rep(1, N)
Sigma_inv <- solve(Sigma_f)

# Constantes analytiques
A <- t(ones) %*% Sigma_inv %*% ones
B <- t(ones) %*% Sigma_inv %*% mu_f
C <- t(mu_f) %*% Sigma_inv %*% mu_f
D <- A * C - B^2

# Calcul de la variance minimale pour chaque rendement cible
frontier <- data.frame(
  risk = numeric(length(target_returns)),
  return = target_returns
)

for (i in seq_along(target_returns)) {
  R <- target_returns[i]
  # Variance du portefeuille efficient
  var_p <- (A * R^2 - 2 * B * R + C) / D
  frontier$risk[i] <- sqrt(var_p)
}

###############################################
# 7. PORTEFEUILLE DE VARIANCE MINIMALE
###############################################

w_minvar <- (Sigma_inv %*% ones) / as.numeric(A)

minvar_return <- sum(w_minvar * mu_f)
minvar_risk   <- sqrt(t(w_minvar) %*% Sigma_f %*% w_minvar)

###############################################
# 8. GRAPHIQUE : FRONTIÈRE D'EFFICIENCE
###############################################

# Création du graphique
ggplot(frontier, aes(x = risk, y = return)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(
    aes(x = minvar_risk, y = minvar_return),
    color = "red", size = 3
  ) +
  labs(
    title = "Frontière d'efficience de Markowitz",
    subtitle = paste("Période :", start_date, "à", end_date),
    x = "Risque annuel (écart-type)",
    y = "Rendement annuel espéré"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 12)
  )
