###gographique (OK)

geo_TR <- c(
  # France
  FR_CAC40_ACC = "CAC.PA",        # Amundi CAC 40 Acc
  
  # Europe
  EU_EUROPE_ACC = "600X.AS",      # SPDR STOXX Europe 600 Acc
  
  # USA
  US_SP500_TR = "^SP500TR",
  
  # Monde
  WORLD_MSCI_WORLD_ACC = "IWDA.L"
)



###sectoriel (à revoir car énormement de données)

sectors_TR <- c(
  # Europe – STOXX Europe 600 sectoriels (Acc)
  EU_TECH_ACC = "EXXT.PA",
  EU_FINANCE_ACC = "EXV1.PA",
  EU_HEALTH_ACC = "EXV3.PA",
  EU_ENERGY_ACC = "EXV6.PA",
  EU_INDUSTRY_ACC = "EXV5.PA",
  EU_CONSUMER_ACC = "EXV4.PA",

  # USA – ETF sectoriels (proxy TR)
  US_TECH_TR = "XLK",
  US_FINANCE_TR = "XLF",
  US_HEALTH_TR = "XLV",
  US_ENERGY_TR = "XLE",
  US_INDUSTRY_TR = "XLI",
  US_CONSUMER_TR = "XLP"
)


###capitalisation (modifié avec les ETFs donc a justifier)

size_TR <- c(
  # France
  FR_LARGE_ACC = "CAC.PA",
  FR_MID_ACC   = "MCAP.PA",     # Amundi CAC Mid 60 Acc
  FR_SMALL_ACC = "SMCP.PA",     # Amundi CAC Small Acc

  # Europe
  EU_LARGE_ACC = "EUNL.DE",     # iShares Core MSCI Europe Acc
  EU_SMALL_ACC = "SMCE.AS",     # iShares MSCI Europe Small Cap Acc

  # USA (indices TR)
  US_LARGE_TR = "^SP500TR",
  US_MID_TR   = "^SP400TR",
  US_SMALL_TR = "^RUTTR",

  # Monde
  WORLD_LARGE_ACC = "IWDA.L",
  WORLD_SMALL_ACC = "WSML.L"
)


###style (pareil ici)

style_TR <- c(
  # France
  FR_VALUE_ACC  = "CVEG.PA",
  FR_GROWTH_ACC = "CREG.PA",

  # Europe
  EU_VALUE_ACC  = "EVAL.PA",
  EU_GROWTH_ACC = "EGRO.PA",

  # USA (indices TR Russell)
  US_VALUE_TR  = "^RU1000VTR",
  US_GROWTH_TR = "^RU1000GTR",

  # Monde
  WORLD_VALUE_ACC  = "IWVL.L",
  WORLD_GROWTH_ACC = "IWFG.L"
)



###type (là aussi - courage)

asset_TR <- c(
  # Actions
  FR_EQUITY_ACC    = "CAC.PA",
  EU_EQUITY_ACC    = "600X.AS",
  US_EQUITY_TR     = "^SP500TR",
  WORLD_EQUITY_ACC = "IWDA.L",

  # Obligations (Aggregate cohérent)
  EU_BONDS_ACC     = "IEAG.L",   # iShares Euro Aggregate Bond Acc
  WORLD_BONDS_ACC  = "AGGH.L",   # iShares Global Aggregate Bond Acc

  # Actifs réels
  WORLD_REIT_ACC = "IWDP.L",     # iShares Developed Markets Property Acc
  WORLD_GOLD_ACC = "SGLN.L"      # iShares Physical Gold
)


all_tickers <- c(asset_TR, style_TR, size_TR, geo_TR, sectors_TR)
length(all_tickers)
tickers_yahoo <- unname(all_tickers)

### utiliser quantmod si exctraction de cette liste (avec un bachage)
getSymbols(tickers_yahoo, src = "yahoo")
