install.packages("quantmod")
library(quantmod)

# Définition des ETF MSCI (proxys géographiques)
# VGK  : MSCI Europe
# IVV  : MSCI USA
# EEM  : MSCI Emerging Markets
# EWJ  : MSCI Japan
# AAXJ : MSCI Asia ex Japan
tickers <- c("VGK", "IVV", "EEM", "EWJ", "AAXJ")


# Téléchargement des données financières depuis Yahoo Finance
# Période longue afin de capturer plusieurs cycles économiques
getSymbols(tickers, src = "yahoo", from = "2005-01-01", to = "2024-01-01")

# Extraction des prix de clôture ajustés (Adjusted Close)
# Les prix ajustés tiennent compte des dividendes et splits
prices <- na.omit(merge(
  Ad(VGK),
  Ad(IVV),
  Ad(EEM),
  Ad(EWJ),
  Ad(AAXJ)
))

# 5. Renommage des colonnes pour une meilleure lisibilité
colnames(prices) <- c(
  "MSCI_Europe",
  "MSCI_USA",
  "MSCI_Emerging",
  "MSCI_Japan",
  "MSCI_Asia_ex_Japan"
)

# Transformation des prix en logarithmes
# Cette transformation permet de linéariser les séries temporelles
# et facilite les analyses statistiques ultérieures
log_prices <- log(prices)

# Aperçu des premières observations
head(log_prices)


# Passage optionnel en fréquence mensuelle
# Recommandé pour les études académiques (réduction du bruit journalier)
prices_monthly <- to.monthly(prices,
                             indexAt  = "lastof",
                             drop.time = TRUE)[, 4]

# Calcul des log-prix mensuels
log_prices_monthly <- log(prices_monthly)

# Vérification descriptive des séries
summary(log_prices_monthly)
