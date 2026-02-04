packages <- c("quantmod", "tidyverse", "PerformanceAnalytics", "xts", "ggplot2", "corrplot")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(ggplot2)
library(corrplot)

# ==================== LISTE DES ETF ==================== (tout ca on ne prend plus car ETF mais garder si jamais) 

etf_list <- list()

# 1. Géographique / Régions
etf_list$geographique <- c(
  "VGK",   # Europe
  "IVV",   # US Large Cap (S&P 500)
  "EEM",   # Marchés émergents
  "AAXJ",  # Asie ex-Japon
  "EWJ"    # Japon
)

# 2. Secteurs
etf_list$secteurs <- c(
  "MOO",   # Agrobusiness
  "XLF",   # Finance
  "VFH",   # Financial Services
  "XLK",   # Technologie
  "VGT",   # Information Technology
  "XLV",   # Santé
  "VHT",   # Health Care
  "XLE",   # Énergie
  "VDE",   # Energy
  "XLB",   # Matériaux
  "VAW",   # Materials
  "XLI",   # Industrie
  "VIS"    # Industrials
)

# 3. Capitalisation
etf_list$capitalisation <- c(
  "ITOT",  # Total US Market
  "VTI",   # Total Stock Market
  "SCHB",  # Broad US Market
  "SPTM"   # Total Market
)

# 4. Facteurs / Styles
etf_list$facteurs <- c(
  "VTV",   # Value US
  "IWD",   # Russell 1000 Value
  "VUG",   # Growth US
  "IWF",   # Russell 1000 Growth
  "RPG",   # Pure Growth
  "VYM",   # High Dividend Yield
  "QUAL",  # Quality
  "SPHQ",  # S&P 500 Quality
  "USMV",  # Minimum Volatility
  "VFMV",  # US Minimum Volatility
  "MTUM"   # Momentum
)

# Tous les ETF uniques (sans doublons)
tous_etf <- unique(unlist(etf_list))
cat("Nombre total d'ETF uniques:", length(tous_etf), "\n")

# ==================== TÉLÉCHARGEMENT DES DONNÉES ====================

# Fonction pour télécharger les données avec gestion d'erreurs
get_etf_data <- function(symbols, from = "2014-01-01", to = "2024-01-01") {
  data_list <- list()
  
  for(sym in symbols) {
    cat("Téléchargement de", sym, "...\n")
    tryCatch({
      # Télécharger les données
      getSymbols(sym, from = from, to = to, auto.assign = FALSE) -> temp_data
      
      # Extraire les prix de clôture ajustés
      if(!is.null(temp_data) && nrow(temp_data) > 0) {
        close_col <- paste0(sym, ".Adjusted")
        if(close_col %in% colnames(temp_data)) {
          data_list[[sym]] <- temp_data[, close_col]
          colnames(data_list[[sym]]) <- sym
        }
      }
    }, error = function(e) {
      cat("Erreur avec", sym, ":", e$message, "\n")
    })
    
    # Pause pour éviter de surcharger Yahoo Finance
    Sys.sleep(0.5)
  }
  
  # Combiner tous les prix en un seul xts
  if(length(data_list) > 0) {
    all_prices <- do.call(merge, data_list)
    return(na.omit(all_prices))
  } else {
    return(NULL)
  }
}

# Télécharger les données (limité à 10 ETF pour éviter les limites)
etf_a_telecharger <- tous_etf[1:min(10, length(tous_etf))]
prix_etf <- get_etf_data(etf_a_telecharger)

# ==================== ANALYSE DE BASE ====================

if(!is.null(prix_etf) && ncol(prix_etf) > 1) {
  
  # 1. Aperçu des données
  cat("\n=== APERÇU DES DONNÉES ===\n")
  print(head(prix_etf))
  cat("\nDimensions:", dim(prix_etf), "\n")
  cat("Période:", index(prix_etf[1]), "à", index(prix_etf[nrow(prix_etf)]), "\n")
  
  # 2. Calcul des rendements
  rendements <- na.omit(Return.calculate(prix_etf, method = "log"))
  
  # 3. Statistiques descriptives
  cat("\n=== STATISTIQUES DES RENDEMENTS ===\n")
  stats_rendements <- data.frame(
    Moyenne = apply(rendements, 2, mean, na.rm = TRUE) * 252,  # Annualisé
    Volatilite = apply(rendements, 2, sd, na.rm = TRUE) * sqrt(252),
    Sharpe = apply(rendements, 2, function(x) mean(x, na.rm = TRUE)/sd(x, na.rm = TRUE)) * sqrt(252),
    Min = apply(rendements, 2, min, na.rm = TRUE),
    Max = apply(rendements, 2, max, na.rm = TRUE)
  )
  
  print(round(stats_rendements, 4))
  
  # 4. Corrélations
  cat("\n=== MATRICE DE CORRÉLATION ===\n")
  correlations <- cor(rendements, use = "pairwise.complete.obs")
  print(round(correlations, 3))
  
  # ==================== VISUALISATIONS ====================
  
  # 5. Graphique des prix normalisés
  prix_normalises <- prix_etf
  for(i in 1:ncol(prix_etf)) {
    prix_normalises[,i] <- prix_etf[,i] / as.numeric(prix_etf[1,i])
  }
  
  # Convertir en dataframe pour ggplot
  prix_df <- fortify(prix_normalises)
  prix_long <- prix_df %>%
    pivot_longer(cols = -Index, names_to = "ETF", values_to = "Prix")
  
  ggplot(prix_long, aes(x = Index, y = Prix, color = ETF)) +
    geom_line(linewidth = 1) +
    labs(title = "Performance des ETF (normalisés à 1)",
         x = "Date", y = "Prix normalisé") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 6. Heatmap des corrélations
  corrplot(correlations, method = "color", type = "upper",
           title = "Corrélations entre ETF", mar = c(0, 0, 1, 0))
  
  # 7. Rendements cumulés
  rendements_cumules <- cumprod(1 + rendements) - 1
  
  rend_cum_df <- fortify(rendements_cumules)
  rend_cum_long <- rend_cum_df %>%
    pivot_longer(cols = -Index, names_to = "ETF", values_to = "Rendement")
  
  ggplot(rend_cum_long, aes(x = Index, y = Rendement, color = ETF)) +
    geom_line(linewidth = 1) +
    labs(title = "Rendements cumulés des ETF",
         x = "Date", y = "Rendement cumulé") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_y_continuous(labels = scales::percent)
  
  # 8. Boxplot des rendements
  rend_long <- fortify(rendements) %>%
    pivot_longer(cols = -Index, names_to = "ETF", values_to = "Rendement")
  
  ggplot(rend_long, aes(x = ETF, y = Rendement, fill = ETF)) +
    geom_boxplot() +
    labs(title = "Distribution des rendements quotidiens",
         x = "", y = "Rendement") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::percent)
  
  # ==================== EXPORT DES DONNÉES ====================
  
  # Créer un rapport résumé
  rapport <- list(
    "ETF_Liste" = etf_list,
    "Stats_Rendements" = stats_rendements,
    "Correlations" = correlations,
    "Prix" = prix_etf,
    "Rendements" = rendements
  )
  
  # Sauvegarder les données
  saveRDS(rapport, "etf_analysis_report.rds")
  write.csv(as.data.frame(prix_etf), "etf_prices.csv")
  write.csv(stats_rendements, "etf_statistics.csv")
  
  cat("\n=== ANALYSE TERMINÉE ===\n")
  cat("Données sauvegardées dans :\n")
  cat("- etf_analysis_report.rds (objet R)\n")
  cat("- etf_prices.csv (prix historiques)\n")
  cat("- etf_statistics.csv (statistiques)\n")
  
} else {
  cat("Impossible de télécharger les données. Vérifiez votre connexion internet.\n")
}

# ==================== FONCTIONS UTILITAIRES ====================

# Fonction pour analyser un sous-ensemble d'ETF
analyser_categorie <- function(categorie) {
  if(categorie %in% names(etf_list)) {
    cat("\n=== ANALYSE DE LA CATÉGORIE:", toupper(categorie), "===\n")
    cat("ETF dans cette catégorie:\n")
    print(etf_list[[categorie]])
    
    # Filtrer les données disponibles
    etf_disponibles <- intersect(etf_list[[categorie]], colnames(prix_etf))
    
    if(length(etf_disponibles) > 1) {
      sous_prix <- prix_etf[, etf_disponibles]
      sous_rendements <- na.omit(Return.calculate(sous_prix))
      
      # Afficher les corrélations
      cat("\nCorrélations intra-catégorie:\n")
      cor_intra <- cor(sous_rendements, use = "pairwise.complete.obs")
      print(round(cor_intra, 3))
      
      return(sous_rendements)
    }
  }
  return(NULL)
}

# Exemple: analyser la catégorie géographique
if(!is.null(prix_etf)) {
  resultats_geo <- analyser_categorie("geographique")
}

# Fonction pour comparer deux ETF spécifiques
comparer_etf <- function(etf1, etf2) {
  if(all(c(etf1, etf2) %in% colnames(prix_etf))) {
    comparatif <- prix_etf[, c(etf1, etf2)]
    comparatif_normalise <- comparatif
    comparatif_normalise[,1] <- comparatif[,1] / as.numeric(comparatif[1,1])
    comparatif_normalise[,2] <- comparatif[,2] / as.numeric(comparatif[1,2])
    
    plot(comparatif_normalise, main = paste("Comparaison", etf1, "vs", etf2),
         legend.loc = "topleft", col = c("blue", "red"))
  }
}

# Exemple: comparer VTI et IVV
if(!is.null(prix_etf)) {
  comparer_etf("VTI", "IVV")
}
