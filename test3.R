# Nettoyage de l'environnement
rm(list = ls())

# Chargement des bibliothèques
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(quadprog)   # Pour optimisation quadratique avec contraintes

###############################################
# 1. TÉLÉCHARGEMENT DES DONNÉES
###############################################

# Tickers qui fonctionnent bien
tickers <- c(
  CAC40 = "^FCHI",
  SP500 = "^GSPC",
  DAX = "^GDAXI",          # Indice allemand
  NIKKEI = "^N225"         # Indice japonais
  EM = "EEM"            
)

start_date <- "2019-01-01"  # Période plus courte pour éviter les NA
end_date <- "2024-12-31"

# Fonction de téléchargement robuste
download_data <- function(ticker_list) {
  prices_list <- list()
  
  for(i in seq_along(ticker_list)) {
    ticker_name <- names(ticker_list)[i]
    ticker_symbol <- ticker_list[i]
    
    tryCatch({
      cat("Téléchargement de", ticker_name, "...\n")
      
      data <- getSymbols(
        ticker_symbol,
        src = "yahoo",
        from = start_date,
        to = end_date,
        auto.assign = FALSE,
        warnings = FALSE
      )
      
      prices_list[[ticker_name]] <- Ad(data)
      
    }, error = function(e) {
      message("Erreur pour ", ticker_name, ": ", e$message)
    })
  }
  
  # Combiner les données
  if(length(prices_list) > 0) {
    prices <- do.call(merge, prices_list)
    colnames(prices) <- names(prices_list)
    prices <- na.locf(prices)  # Remplissage des NA
    prices <- na.omit(prices)   # Suppression des NA restants
    return(prices)
  } else {
    stop("Aucune donnée téléchargée")
  }
}

# Téléchargement
prices <- download_data(tickers)

###############################################
# 2. CALCUL DES PARAMÈTRES
###############################################

# Rendements logarithmiques quotidiens
returns <- diff(log(prices))
returns <- na.omit(returns)

# Nombre d'actifs
N <- ncol(returns)
asset_names <- colnames(returns)

# Paramètres annuels (252 jours de trading)
mu <- colMeans(returns) * 252
Sigma <- cov(returns) * 252

cat("=== PARAMÈTRES ANNUELS ===\n")
cat("Rendements attendus (% par an):\n")
print(round(mu * 100, 2))

cat("\nVolatilités annuelles (%):\n")
print(round(sqrt(diag(Sigma)) * 100, 2))

cat("\nMatrice de corrélation:\n")
print(round(cor(returns), 3))

###############################################
# 3. FONCTION D'OPTIMISATION AVEC CONTRAINTES
###############################################

# Fonction pour le portefeuille minimum variance (contraint)
min_variance_portfolio <- function(mu, Sigma) {
  N <- length(mu)
  
  # Problème: minimiser w'Σw sous contraintes w >= 0 et sum(w) = 1
  Dmat <- 2 * Sigma
  dvec <- rep(0, N)
  
  # Contraintes: Amat^T w >= bvec
  Amat <- t(rbind(
    rep(1, N),    # sum(w) = 1
    rep(1, N),    # sum(w) <= 1 (transformé en >= -1)
    diag(N)       # w_i >= 0
  ))
  
  bvec <- c(1, 1, rep(0, N))
  meq <- 1  # première contrainte est une égalité (sum(w) = 1)
  
  tryCatch({
    solution <- solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
    w <- solution$solution
    names(w) <- names(mu)
    
    # Normaliser pour s'assurer que sum(w) = 1 exactement
    w <- w / sum(w)
    
    return(list(
      weights = w,
      return = sum(w * mu),
      risk = sqrt(t(w) %*% Sigma %*% w),
      solution = solution
    ))
  }, error = function(e) {
    message("Erreur dans solve.QP: ", e$message)
    # Solution de repli: portefeuille équipondéré
    w_eq <- rep(1/N, N)
    names(w_eq) <- names(mu)
    return(list(
      weights = w_eq,
      return = sum(w_eq * mu),
      risk = sqrt(t(w_eq) %*% Sigma %*% w_eq)
    ))
  })
}

# Fonction pour portefeuille avec rendement cible
target_return_portfolio <- function(target_return, mu, Sigma) {
  N <- length(mu)
  
  # Problème: minimiser w'Σw 
  # Sous contraintes:
  # 1. sum(w) = 1
  # 2. w'mu = target_return
  # 3. w_i >= 0
  
  Dmat <- 2 * Sigma
  dvec <- rep(0, N)
  
  # Contraintes
  Amat <- t(rbind(
    rep(1, N),    # sum(w) = 1 (égalité)
    mu,           # w'mu = target_return (égalité)
    diag(N)       # w_i >= 0
  ))
  
  bvec <- c(1, target_return, rep(0, N))
  meq <- 2  # deux premières contraintes sont des égalités
  
  tryCatch({
    solution <- solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
    w <- solution$solution
    
    # Normaliser
    w <- w / sum(w)
    names(w) <- names(mu)
    
    return(w)
  }, error = function(e) {
    message("Erreur pour target_return = ", target_return, ": ", e$message)
    return(rep(NA, N))
  })
}

###############################################
# 4. CALCUL DE LA FRONTIÈRE EFFICIENTE
###############################################

# Calcul du portefeuille minimum variance
mvp <- min_variance_portfolio(mu, Sigma)

cat("\n")
cat(rep("=", 60), "\n", sep = "")
cat("PORTEFEUILLE MINIMUM VARIANCE (SANS VENTE À DÉCOUVERT)\n")
cat(rep("=", 60), "\n", sep = "")
cat("Poids (%):\n")
for(i in 1:N) {
  cat(sprintf("%-10s: %6.2f%%\n", names(mu)[i], mvp$weights[i] * 100))
}
cat(sprintf("\nRendement attendu: %6.2f%%\n", mvp$return * 100))
cat(sprintf("Risque (volatilité): %6.2f%%\n", mvp$risk * 100))

# Gamme de rendements cibles réalisables
min_return <- mvp$return
max_return <- max(mu)

cat("\nGamme de rendements réalisables:\n")
cat(sprintf("Minimum (MVP): %.2f%%\n", min_return * 100))
cat(sprintf("Maximum (actif le plus risqué): %.2f%%\n", max_return * 100))

# Générer des rendements cibles
n_points <- 30
target_returns <- seq(min_return, max_return, length.out = n_points)

# Stockage des résultats
frontier_results <- list(
  risks = numeric(n_points),
  returns = numeric(n_points),
  weights = matrix(NA, nrow = n_points, ncol = N)
)
colnames(frontier_results$weights) <- asset_names

# Calcul pour chaque rendement cible
cat("\nCalcul de la frontière efficiente...\n")
for(i in 1:n_points) {
  if(i %% 5 == 0) cat("Point", i, "/", n_points, "\n")
  
  w <- target_return_portfolio(target_returns[i], mu, Sigma)
  
  if(!any(is.na(w))) {
    frontier_results$weights[i, ] <- w
    frontier_results$risks[i] <- sqrt(t(w) %*% Sigma %*% w)
    frontier_results$returns[i] <- sum(w * mu)
  }
}

# Filtrer les points valides
valid_points <- !is.na(frontier_results$risks)
frontier_results$risks <- frontier_results$risks[valid_points]
frontier_results$returns <- frontier_results$returns[valid_points]
frontier_results$weights <- frontier_results$weights[valid_points, , drop = FALSE]

###############################################
# 5. PRÉPARATION DES DONNÉES POUR VISUALISATION
###############################################

# Données pour la frontière efficiente
frontier_df <- data.frame(
  risk = frontier_results$risks * 100,
  return = frontier_results$returns * 100,
  type = "Sans vente à découvert"
)

# Données pour les actifs individuels
assets_df <- data.frame(
  asset = names(mu),
  risk = sqrt(diag(Sigma)) * 100,
  return = mu * 100
)

# Données pour le portefeuille minimum variance
mvp_df <- data.frame(
  risk = mvp$risk * 100,
  return = mvp$return * 100,
  label = "Portefeuille\nMinimum Variance"
)

###############################################
# 6. VISUALISATION PRINCIPALE
###############################################

p1 <- ggplot() +
  # Frontière efficiente
  geom_line(data = frontier_df,
            aes(x = risk, y = return),
            color = "darkblue", linewidth = 1.5, alpha = 0.8) +
  
  # Portefeuille minimum variance
  geom_point(data = mvp_df,
             aes(x = risk, y = return),
             color = "red", size = 5, shape = 17) +
  
  geom_text(data = mvp_df,
            aes(x = risk, y = return, label = label),
            vjust = -1.5, hjust = 0.5, color = "red", fontface = "bold") +
  
  # Actifs individuels
  geom_point(data = assets_df,
             aes(x = risk, y = return),
             color = "darkgreen", size = 4) +
  
  geom_text(data = assets_df,
            aes(x = risk, y = return, label = asset),
            vjust = -1, hjust = 0.5, size = 4, fontface = "bold") +
  
  # Mise en forme
  labs(
    title = "Frontière efficiente de Markowitz - Pas de vente à découvert",
    subtitle = paste("Période:", start_date, "à", end_date),
    x = "Risque annuel (écart-type, %)",
    y = "Rendement annuel attendu (%)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Ajustement des échelles
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1))

print(p1)

###############################################
# 7. VISUALISATION DES POIDS
###############################################

# Création du dataframe des poids
if(nrow(frontier_results$weights) > 0) {
  weights_df <- as.data.frame(frontier_results$weights)
  
  # Ajout des métadonnées
  weights_df$portfolio_return <- frontier_results$returns * 100
  weights_df$portfolio_risk <- frontier_results$risks * 100
  weights_df$point_id <- 1:nrow(weights_df)
  
  # Transformer en format long pour ggplot
  weights_long <- data.frame()
  
  for(i in 1:N) {
    asset_name <- asset_names[i]
    temp_df <- data.frame(
      point_id = weights_df$point_id,
      asset = asset_name,
      weight = weights_df[[asset_name]] * 100,  # Conversion en %
      portfolio_return = weights_df$portfolio_return,
      portfolio_risk = weights_df$portfolio_risk
    )
    weights_long <- rbind(weights_long, temp_df)
  }
  
  # Garder seulement les points avec poids significatifs (> 0.1%)
  weights_long <- weights_long[weights_long$weight > 0.1, ]
  
  if(nrow(weights_long) > 0) {
    # Graphique des poids
    p2 <- ggplot(weights_long, aes(x = portfolio_risk, y = weight, color = asset)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2, alpha = 0.7) +
      labs(
        title = "Évolution des poids le long de la frontière efficiente",
        x = "Risque du portefeuille (%)",
        y = "Poids dans le portefeuille (%)",
        color = "Actif"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right",
        panel.grid.major = element_line(color = "gray90")
      ) +
      scale_color_brewer(palette = "Set1")
    
    print(p2)
  } else {
    cat("\nAucun poids significatif (> 0.1%) trouvé pour le graphique.\n")
  }
}

###############################################
# 8. CALCUL DE PORTEFEUILLES SPÉCIFIQUES
###############################################

cat("\n")
cat(rep("=", 60), "\n", sep = "")
cat("PORTEFEUILLES SPÉCIFIQUES\n")
cat(rep("=", 60), "\n", sep = "")

# Fonction pour afficher un portefeuille
display_portfolio <- function(target_pct) {
  target <- target_pct / 100
  w <- target_return_portfolio(target, mu, Sigma)
  
  if(any(is.na(w))) {
    cat(sprintf("\nPortefeuille à %.1f%% - Non réalisable\n", target_pct))
    return(NULL)
  }
  
  cat(sprintf("\nPortefeuille à rendement cible = %.1f%%\n", target_pct))
  cat("Poids (%):\n")
  
  for(i in 1:N) {
    if(w[i] > 0.001) {
      cat(sprintf("%-10s: %6.2f%%\n", asset_names[i], w[i] * 100))
    }
  }
  
  actual_return <- sum(w * mu)
  risk <- sqrt(t(w) %*% Sigma %*% w)
  sharpe <- actual_return / risk
  
  cat(sprintf("\nRendement réalisé: %6.2f%%\n", actual_return * 100))
  cat(sprintf("Risque: %6.2f%%\n", risk * 100))
  cat(sprintf("Ratio de Sharpe: %6.2f\n", sharpe))
  
  return(list(weights = w, return = actual_return, risk = risk))
}

# Calcul de quelques portefeuilles
portfolios <- list()

# Portefeuille à 8%
portfolios$pct8 <- display_portfolio(8)

# Portefeuille à 10%
portfolios$pct10 <- display_portfolio(10)

# Portefeuille à 12% (si réalisable)
portfolios$pct12 <- display_portfolio(12)

###############################################
# 9. TABLEAU RÉCAPITULATIF
###############################################

cat("\n")
cat(rep("=", 60), "\n", sep = "")
cat("TABLEAU RÉCAPITULATIF\n")
cat(rep("=", 60), "\n", sep = "")

# Création du tableau
summary_table <- data.frame(
  Actif = asset_names,
  Rendement = round(mu * 100, 2),
  Volatilite = round(sqrt(diag(Sigma)) * 100, 2),
  MVP = round(mvp$weights * 100, 2)
)

# Ajout des portefeuilles spécifiques si disponibles
if(!is.null(portfolios$pct8)) {
  summary_table$Port_8pct <- round(portfolios$pct8$weights * 100, 2)
}

if(!is.null(portfolios$pct10)) {
  summary_table$Port_10pct <- round(portfolios$pct10$weights * 100, 2)
}

if(!is.null(portfolios$pct12)) {
  summary_table$Port_12pct <- round(portfolios$pct12$weights * 100, 2)
}

print(summary_table)

###############################################
# 10. VÉRIFICATION DES CONTRAINTES
###############################################

cat("\n")
cat(rep("=", 60), "\n", sep = "")
cat("VÉRIFICATION DES CONTRAINTES\n")
cat(rep("=", 60), "\n", sep = "")

verify_constraints <- function(w, name) {
  cat(sprintf("\n%s:\n", name))
  cat(sprintf("  Somme des poids: %.6f\n", sum(w)))
  cat(sprintf("  Poids minimum: %.6f\n", min(w)))
  cat(sprintf("  Poids maximum: %.6f\n", max(w)))
  cat(sprintf("  Nombre de poids négatifs: %d\n", sum(w < -1e-10)))
  cat(sprintf("  Respecte sum(w)=1: %s\n", abs(sum(w) - 1) < 1e-6))
  cat(sprintf("  Respecte w>=0: %s\n", all(w >= -1e-10)))
}

verify_constraints(mvp$weights, "Portefeuille Minimum Variance")

if(!is.null(portfolios$pct8)) {
  verify_constraints(portfolios$pct8$weights, "Portefeuille 8%")
}

if(!is.null(portfolios$pct10)) {
  verify_constraints(portfolios$pct10$weights, "Portefeuille 10%")
}

###############################################
# 11. EXPORT DES RÉSULTATS
###############################################

# Sauvegarde des données
results <- list(
  parameters = list(
    assets = asset_names,
    mu = mu,
    Sigma = Sigma,
    period = paste(start_date, "à", end_date)
  ),
  minimum_variance_portfolio = mvp,
  efficient_frontier = frontier_results,
  specific_portfolios = portfolios
)

# Créer un dossier pour les résultats
if(!dir.exists("results")) {
  dir.create("results")
}

saveRDS(results, "results/markowitz_results.rds")
write.csv(summary_table, "results/portfolio_summary.csv", row.names = FALSE)
write.csv(frontier_df, "results/efficient_frontier.csv", row.names = FALSE)

# Graphiques supplémentaires
ggsave("results/efficient_frontier.png", p1, width = 10, height = 8, dpi = 300)

if(exists("p2")) {
  ggsave("results/portfolio_weights.png", p2, width = 12, height = 8, dpi = 300)
}

cat("\n")
cat(rep("=", 60), "\n", sep = "")
cat("ANALYSE TERMINÉE AVEC SUCCÈS\n")
cat(rep("=", 60), "\n", sep = "")
cat("Résultats sauvegardés dans le dossier 'results/':\n")
cat("1. markowitz_results.rds - Tous les résultats (R format)\n")
cat("2. portfolio_summary.csv - Tableau récapitulatif\n")
cat("3. efficient_frontier.csv - Points de la frontière\n")
cat("4. efficient_frontier.png - Graphique principal\n")
if(exists("p2")) {
  cat("5. portfolio_weights.png - Graphique des poids\n")
}
