
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)

###############################################
# 1. PARAMÈTRES GÉNÉRAUX
###############################################

# Liste unique de tickers Yahoo (à adapter avec notre liste finale)
# exemple avec cette liste
indices_FR <- c(
  CAC40_TR = "^CACGR",
  SBF120   = "^SBF120"
)

indices_US <- c(
  SP500_TR = "^SP500TR"
)

# Regroupement
indices_all <- c(indices_FR, indices_US)
tickers <- unname(indices_all)  ### permet de garder uniquement "^SP500TR" et non plus :" SP500_TR = "^SP500TR""


start_date <- "2014-01-01"
end_date   <- "2024-12-31"

###############################################
# 2. EXTRACTION DES PRIX AJUSTÉS
###############################################

getSymbols(
  tickers,
  src   = "yahoo",
  from  = start_date,
  to    = end_date,
  auto.assign = TRUE
)

# Extraction des prix de clôture ajustés
prices <- do.call(merge, lapply(tickers, function(x) Ad(get(x))))

colnames(prices) <- tickers

###############################################
# 3. CALCUL DES RENDEMENTS LOG
###############################################

# Rendements log-normés journaliers
returns <- diff(log(prices))

# Suppression des NA initiaux
returns <- na.omit(returns)

###############################################
# 4. MATRICE VARIANCE-COVARIANCE
###############################################

# Matrice variance-covariance empirique
Sigma <- cov(returns)

# Vérification dimensions
dim(Sigma)

###############################################
# 5. PARAMÈTRES POUR MARKOWITZ
###############################################

# Nombre d'actifs
N <- ncol(returns)

# Rendements moyens (journaliers)
mu <- colMeans(returns)

###############################################
# 6. FRONTIÈRE D’EFFICIENCE (MARKOWITZ)
###############################################

# Séquence de rendements cibles
target_returns <- seq(
  min(mu),
  max(mu),
  length.out = 100
)

# Matrices utiles
ones <- rep(1, N)
Sigma_inv <- solve(Sigma)

# Constantes analytiques
A <- t(ones) %*% Sigma_inv %*% ones
B <- t(ones) %*% Sigma_inv %*% mu
C <- t(mu)   %*% Sigma_inv %*% mu
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

minvar_return <- sum(w_minvar * mu)
minvar_risk   <- sqrt(t(w_minvar) %*% Sigma %*% w_minvar)

###############################################
# 8. GRAPHIQUE : FRONTIÈRE D’EFFICIENCE
###############################################

ggplot(frontier, aes(x = risk, y = return)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(
    aes(x = minvar_risk, y = minvar_return),
    color = "red", size = 3
  ) +
  labs(
    title = "Frontière d'efficience de Markowitz",
    x = "Risque (écart-type)",
    y = "Rendement espéré"
  ) +
  theme_minimal()
