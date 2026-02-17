# Installation des packages (à exécuter une seule fois)
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("dplyr")  # Optionnel mais utile

# Chargement des bibliothèques
library(readxl)
library(ggplot2)

# 1. IMPORTATION DES DONNÉES
cat("=== IMPORTATION DES DONNÉES ===\n")
data <- read_excel(file.choose())
cat("\nStructure des données importées :\n")
str(data)

# 2. NETTOYAGE DES DONNÉES
cat("\n=== NETTOYAGE DES DONNÉES ===\n")

# Vérifier les noms des colonnes pour mieux les identifier
print(names(data))

# Convertir les virgules en points pour les colonnes numériques (colonnes 2 et 3)
# Note: Il est préférable d'utiliser les noms des colonnes plutôt que les indices
data[[2]] <- as.numeric(gsub(",", ".", as.character(data[[2]])))
data[[3]] <- as.numeric(gsub(",", ".", as.character(data[[3]])))

# Vérification après conversion
cat("\nAperçu des données après conversion :\n")
head(data)

# 3. GESTION DES VALEURS MANQUANTES
cat("\n=== GESTION DES VALEURS MANQUANTES ===\n")
cat("Nombre de valeurs manquantes avant nettoyage :\n")
print(colSums(is.na(data)))

# Supprimer les lignes avec NA
data_clean <- na.omit(data)
cat("\nDimensions après suppression des NA :\n")
print(dim(data_clean))
# Vérifiez vos rendements moyens
print(mu)  # Devrait montrer des valeurs négatives

# Graphique des prix
par(mfrow = c(2, 1))
plot(data_clean[[2]], type = "l", main = "Prix Actif 1")
plot(data_clean[[3]], type = "l", main = "Prix Actif 2")


# 4. CALCUL DES RENDEMENTS
cat("\n=== CALCUL DES RENDEMENTS ===\n")
# S'assurer qu'il y a assez de données
if(nrow(data_clean) > 1) {
 # Inverser l'ordre des lignes (du plus ancien au plus récent)
data_clean <- data_clean[nrow(data_clean):1, ]

# Recalculer les rendements
returns <- data.frame(
  r1 = diff(log(data_clean[[2]])),
  r2 = diff(log(data_clean[[3]]))
)
  # Graphique des rendements
plot(returns$r1, type = "l", main = "Rendements Actif 1")
abline(h = 0, col = "red")
     
  # Supprimer les éventuelles valeurs manquantes
  returns <- na.omit(returns)
  
  cat("Aperçu des rendements calculés :\n")
  head(returns)
  
  cat("\nStatistiques descriptives des rendements :\n")
  summary(returns)
  
  # 5. STATISTIQUES DES RENDEMENTS
  cat("\n=== STATISTIQUES DES RENDEMENTS ===\n")
  mu <- colMeans(returns)
  Sigma <- cov(returns)
  
  cat("Rendements moyens :\n")
  print(mu)
  cat("\nMatrice de variance-covariance :\n")
  print(Sigma)
  
  # 6. CONSTRUCTION DE LA FRONTIÈRE EFFICIENTE
  cat("\n=== CONSTRUCTION DE LA FRONTIÈRE EFFICIENTE ===\n")
  
  # Séquence de poids pour le portefeuille
  w1 <- seq(0, 1, length.out = 1000)
  w2 <- 1 - w1
  
  # Calcul du rendement et du risque pour chaque portefeuille
  portfolio_return <- w1 * mu[1] + w2 * mu[2]
  portfolio_sd <- sqrt(
    w1^2 * Sigma[1,1] +
      w2^2 * Sigma[2,2] +
      2 * w1 * w2 * Sigma[1,2]
  )
  
  # 7. VISUALISATION
  cat("\n=== CRÉATION DU GRAPHIQUE ===\n")
  
  # Création du dataframe pour le graphique
  df_plot <- data.frame(
    Volatility = portfolio_sd,
    Return = portfolio_return
  )
  
  # Identification du portefeuille avec le meilleur ratio de Sharpe (optionnel)
  risk_free_rate <- 0  # Taux sans risque (à ajuster si nécessaire)
  sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_sd
  optimal_idx <- which.max(sharpe_ratio)
  
  # Création du graphique
  p <- ggplot(df_plot, aes(x = Volatility, y = Return)) +
    geom_line(color = "blue", linewidth = 1) +
    # Ajouter le point du portefeuille optimal (optionnel)
    geom_point(data = df_plot[optimal_idx,], 
               aes(x = Volatility, y = Return), 
               color = "red", size = 3) +
    labs(
      title = "Frontière Efficiente - Modèle de Markowitz",
      subtitle = paste("Données:", nrow(data_clean), "observations"),
      x = "Risque (Volatilité)",
      y = "Rendement Espéré",
      caption = "Point rouge: Portefeuille avec le meilleur ratio de Sharpe"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "gray50")
    )
  
  # Afficher le graphique
  print(p)
  
  # 8. INFORMATION SUR LE PORTEFEUILLE OPTIMAL
  cat("\n=== PORTEFEUILLE OPTIMAL (max Sharpe) ===\n")
  cat("Poids du premier actif:", round(w1[optimal_idx], 3), "\n")
  cat("Poids du second actif:", round(w2[optimal_idx], 3), "\n")
  cat("Rendement attendu:", round(portfolio_return[optimal_idx], 6), "\n")
  cat("Volatilité:", round(portfolio_sd[optimal_idx], 6), "\n")
  cat("Ratio de Sharpe:", round(sharpe_ratio[optimal_idx], 3), "\n")
  
} else {
  stop("Pas assez de données après nettoyage pour calculer les rendements")
}

# 9. VÉRIFICATIONS FINALES
cat("\n=== VÉRIFICATIONS ===\n")
cat("Répertoire de travail actuel :\n")
print(getwd())
cat("\nFichiers dans le répertoire :\n")
print(list.files())
