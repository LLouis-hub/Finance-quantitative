###gographique (OK)

geo_TR <- c(
  # France (indice TR)
  "FR_CAC40_TR = ^CACGR",
  
  # Europe (indices TR)
  "EU_EUROSTOXX50_TR = ^SX5GT",
  "EU_STOXX600_TR   = ^SXXGR",
  
  # USA (indice TR)
  "US_SP500_TR = ^SP500TR",
  
  # Monde (ETF accumulant)
  "WORLD_MSCI_WORLD_ACC = IWDA.L"
)


###sectoriel (à revoir car énormement de données)

sectors_TR <- list(
  sectors_FR = c(
    # Agro / Consommation de base
    "FR_AGRO_ACC      = EXV4.PA",
    # Technologie
    "FR_TECH_ACC      = EXXT.PA",
    # Finance
    "FR_FINANCE_ACC   = EXV1.PA",
    # Santé
    "FR_HEALTH_ACC    = EXV3.PA",
    # Énergie
    "FR_ENERGY_ACC    = EXV6.PA",
    # Industrie
    "FR_INDUSTRY_ACC  = EXV5.PA"
  ),
  
  sectors_EU = c(
    # Agro / Consommation de base
    "EU_AGRO_ACC      = EXV4.PA",
    # Technologie
    "EU_TECH_ACC      = EXXT.PA",
    # Finance
    "EU_FINANCE_ACC   = EXV1.PA",
    # Santé
    "EU_HEALTH_ACC    = EXV3.PA",
    # Énergie
    "EU_ENERGY_ACC    = EXV6.PA",
    # Industrie
    "EU_INDUSTRY_ACC  = EXV5.PA"
  ),
  
  sectors_US = c(
    # Agro / Consommation de base
    "US_AGRO_TR      = XLP",
    # Technologie
    "US_TECH_TR      = XLK",
    # Finance
    "US_FINANCE_TR   = XLF",
    # Santé
    "US_HEALTH_TR    = XLV",
    # Énergie
    "US_ENERGY_TR    = XLE",
    # Industrie
    "US_INDUSTRY_TR  = XLI"
  )
)

# Accéder aux données :
sectors_TR$sectors_FR
sectors_TR$sectors_EU
sectors_TR$sectors_US


###capitalisation (modifié avec les ETFs donc a justifier)

size_TR <- c(
  # France (ETF accumulants)
  "FR_LARGE_ACC = CAC.PA",
  "FR_MID_ACC   = MCAP.PA",
  "FR_SMALL_ACC = SMCP.PA",
  
  # Europe (ETF accumulants)
  "EU_LARGE_ACC = MEUD.PA",
  "EU_SMALL_ACC = ESMC.PA",
  
  # USA (indices TR)
  "US_LARGE_TR = ^SP500TR",
  "US_MID_TR   = ^SP400TR",
  "US_SMALL_TR = ^RU2000TR",
  
  # Monde (ETF accumulants)
  "WORLD_LARGE_ACC = IWDA.L",
  "WORLD_SMALL_ACC = WSML.L"
)


###style (pareil ici)

style_TR <- c(
  # France (ETF accumulants)
  "FR_VALUE_ACC  = CVEG.PA",
  "FR_GROWTH_ACC = CREG.PA",
  
  # Europe (ETF accumulants)
  "EU_VALUE_ACC  = EVAL.PA",
  "EU_GROWTH_ACC = EGRO.PA",
  
  # USA (indices TR)
  "US_VALUE_TR  = ^RU1000VTR",
  "US_GROWTH_TR = ^RU1000GTR",
  
  # Monde (ETF accumulants)
  "WORLD_VALUE_ACC  = IWVL.L",
  "WORLD_GROWTH_ACC = IWFG.L"
)


###type (là aussi - courage)

asset_TR <- c(
  # Actions
  "FR_EQUITY_TR    = ^CACGR",
  "EU_EQUITY_TR    = ^SX5GT",
  "US_EQUITY_TR    = ^SP500TR",
  "WORLD_EQUITY_ACC = IWDA.L",
  
  # Obligations
  "EU_BONDS_ACC = IEGA.L",
  "WORLD_BONDS_ACC = BNDX",
  
  # Actifs réels
  "WORLD_REIT_ACC = VNQ",
  "WORLD_GOLD_ACC = GLD"
)


all_tickers <- c(asset_TR, style_TR, size_TR, geo_TR, sectors_TR)
length(all_tickers)
tickers_yahoo <- unname(all_tickers)

### utiliser quantmod si exctraction de cette liste (avec un bachage)
getSymbols(tickers_yahoo, src = "yahoo")
