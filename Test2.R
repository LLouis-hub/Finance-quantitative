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
# 2. EXTRACTION DES PRIX AVEC GESTION DES NA
###############################################

# Fonction pour télécharger avec gestion robuste des erreurs
download_ticker <- function(ticker_symbol, ticker_name) {
  tryCatch({
    cat(paste("Téléchargement de", ticker_name, "...\n"))
    
    # Téléchargement avec réessai automatique
    data <- getSymbols(
      ticker_symbol,
      src = "yahoo",
      from = start_date,
      to = end_date,
      auto.assign = FALSE,
      warnings = FALSE
    )
    
    # Récupération du prix ajusté
    price <- Ad(data)
    colnames(price) <- ticker_name
    
    return(price)
  }, error = function(e) {
    message(paste("Erreur pour", ticker_name, ":", e$message))
    return(NULL)
  })
}

# Téléchargement de tous les tickers
prices_list <- list()

for(i in seq_along(tickers)) {
  ticker_symbol <- tickers[i]
  ticker_name <- names(tickers)[i]
  
  price_data <- download_ticker(ticker_symbol, ticker_name)
  
  if(!is.null(price_data)) {
    prices_list[[ticker_name]] <- price_data
  }
}

# Vérifier qu'on a au moins 2 séries
if(length(prices_list) < 2) {
  stop("Pas assez de données téléchargées. Essayez d'autres tickers.")
}

# Combiner toutes les séries
prices <- do.call(merge, prices_list)

# Inspection des NA
cat("\n=== INSPECTION DES DONNÉES ===\n")
cat("Dimensions avant nettoyage:", dim(prices), "\n")
cat("Nombre total de NA:", sum(is.na(prices)), "\n")
cat("NA par colonne:\n")
print(colSums(is.na(prices)))

# STRATÉGIE 1: Supprimer les lignes avec NA (si peu de NA)
# prices_clean <- na.omit(prices)

# STRATÉGIE 2: Imputation des NA (mieux pour les séries financières)
# Remplir les NA par la dernière valeur non-NA (méthode "locf")
prices_clean <- na.locf(prices, na.rm = FALSE)

# STRATÉGIE 3: Imputation linéaire (alternative)
# prices_clean <- na.approx(prices, na.rm = FALSE)

# Vérification finale
cat("\nDimensions après nettoyage:", dim(prices_clean), "\n")
cat("NA restants:", sum(is.na(prices_clean)), "\n")

# Visualisation des séries
plot(prices_clean, main = "Séries de prix ajustés")
grid()

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
ggplot(frontier, aes(x = return, y = risk)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(
    aes(x = minvar_return, y = minvar_risk),
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
