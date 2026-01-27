
install.packages("quantmod")
library(quantmod)

### "importations d'un ticker
# Importer les données d'un ticker
getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2024-01-01")

# Afficher les données
head(AAPL)
print(AAPL)

# Structure des données
str(AAPL)
colnames(AAPL)

# Extraire les colonnes spécifiques
Cl(AAPL)  # Prix de clôture
Op(AAPL)  # Prix d'ouverture
Hi(AAPL)  # Plus haut
Lo(AAPL)  # Plus bas
Vo(AAPL)  # Volume

### seulement les prix clotures
# Définir les symboles
symbols <- c("AAPL", "MSFT", "GOOGL", "AMZN")

# Télécharger tous les tickers
getSymbols(symbols, src = "yahoo", from = "2023-01-01", to = "2024-01-01")

# Accéder à chaque dataframe
head(AAPL)
head(MSFT)

# Extraire uniquement les prix de clôture
closing_prices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))
colnames(closing_prices) <- symbols


###  Script complet d'analyse
library(quantmod)
library(PerformanceAnalytics)
library(xts)

# 1. Télécharger les données
symbols <- c("AAPL", "MSFT", "GOOGL")
getSymbols(symbols, src = "yahoo", from = "2020-01-01")

# 2. Calculer les rendements
returns <- do.call(merge, lapply(symbols, function(x) {
  dailyReturn(Cl(get(x)))
}))
colnames(returns) <- symbols

# 3. Statistiques de base
table.Stats(returns)

# 4. Corrélations
cor_matrix <- cor(returns, use = "complete.obs")
print(cor_matrix)

# 5. Visualisation
chartSeries(AAPL, 
            theme = chartTheme("white"),
            TA = "addBBands(); addRSI()")
