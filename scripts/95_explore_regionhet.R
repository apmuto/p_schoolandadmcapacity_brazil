# ==================================================
# 95_explore_regionhet.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Explore regional heterogeneity in the
#       relationship between fiscal capacity and
#       school accessibility. Split models by:
#       - Norte + Nordeste vs Centro-Oeste + 
#         Sudeste + Sul
#       - By individual region
#       - Urban vs rural municipalities
# Input:  data/processed/master_mun_main.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading master_mun_main...\n")
main <- load_br_csv("data/processed/master_mun_main.csv")
report_dims(main, "master_mun_main")

# ============================================================
# ADD REGION VARIABLE
# ============================================================

cat("\nAdding region variable...\n")

main <- main %>%
  mutate(
    CO_UF = as.integer(substr(CO_MUNICIPIO, 1, 2)),
    regiao = case_when(
      CO_UF %in% c(11,12,13,14,15,16,17) ~ "Norte",
      CO_UF %in% c(21,22,23,24,25,26,27,28,29) ~ "Nordeste",
      CO_UF %in% c(31,32,33,35) ~ "Sudeste",
      CO_UF %in% c(41,42,43) ~ "Sul",
      CO_UF %in% c(50,51,52,53) ~ "Centro-Oeste",
      TRUE ~ NA_character_
    ),
    # Binary: peripheral vs core
    periphery = regiao %in% c("Norte", "Nordeste")
  )

cat("Region distribution:\n")
print(table(main$regiao, useNA = "always"))

cat("\nNo-school by region:\n")
main %>%
  group_by(regiao) %>%
  summarise(
    n_mun       = n(),
    no_school   = sum(!has_school),
    pct_no_school = round(100 * no_school / n_mun, 1),
    mean_dist   = round(mean(dist_nearest_school_km, na.rm=TRUE), 1),
    mean_coverage = round(mean(schools_per_10k, na.rm=TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_no_school)) %>%
  print()

# ============================================================
# MODEL 1: COVERAGE BY REGION GROUP
# Norte + Nordeste (periphery) vs rest (core)
# ============================================================

cat("\n============================================================\n")
cat("COVERAGE MODELS: PERIPHERY vs CORE\n")
cat("============================================================\n")

# Periphery: Norte + Nordeste
m_periph <- lm(schools_per_10k ~
                 fpm_dependence_mun +
                 fiscal_autonomy_mun +
                 dist_capital_km +
                 log_pop_mun +
                 log_gdp_pc_mun +
                 factor(bioma),
               data = main %>% filter(periphery))

cat("\nPeriphery (Norte + Nordeste):\n")
print(summary(m_periph))

# Core: Sul + Sudeste + Centro-Oeste
m_core <- lm(schools_per_10k ~
               fpm_dependence_mun +
               fiscal_autonomy_mun +
               dist_capital_km +
               log_pop_mun +
               log_gdp_pc_mun +
               factor(bioma),
             data = main %>% filter(!periphery))

cat("\nCore (Sul + Sudeste + Centro-Oeste):\n")
print(summary(m_core))

# ============================================================
# MODEL 2: COVERAGE BY INDIVIDUAL REGION
# ============================================================

cat("\n============================================================\n")
cat("COVERAGE BY INDIVIDUAL REGION\n")
cat("============================================================\n")

regions <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")

for (reg in regions) {
  df_reg <- main %>% filter(regiao == reg)
  cat(sprintf("\n--- %s (n=%d, no_school=%d) ---\n",
              reg, nrow(df_reg),
              sum(!df_reg$has_school)))

  m_reg <- lm(schools_per_10k ~
                fpm_dependence_mun +
                fiscal_autonomy_mun +
                log_pop_mun +
                log_gdp_pc_mun,
              data = df_reg)

  # Print only key coefficients
  coefs <- summary(m_reg)$coefficients
  cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
              "fpm_dependence_mun",
              coefs["fpm_dependence_mun", "Estimate"],
              coefs["fpm_dependence_mun", "Pr(>|t|)"]))
  cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
              "fiscal_autonomy_mun",
              coefs["fiscal_autonomy_mun", "Estimate"],
              coefs["fiscal_autonomy_mun", "Pr(>|t|)"]))
  cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
              "log_pop_mun",
              coefs["log_pop_mun", "Estimate"],
              coefs["log_pop_mun", "Pr(>|t|)"]))
  cat(sprintf("  R2=%.3f  n=%d\n",
              summary(m_reg)$r.squared,
              nobs(m_reg)))
}

# ============================================================
# MODEL 3: DISTANCE MODEL BY REGION
# No-school municipalities only
# ============================================================

cat("\n============================================================\n")
cat("DISTANCE TO NEAREST SCHOOL BY REGION\n")
cat("(No-school municipalities only)\n")
cat("============================================================\n")

no_school <- main %>% filter(!has_school)

cat("No-school municipalities by region:\n")
print(table(no_school$regiao, useNA = "always"))

for (reg in regions) {
  df_reg <- no_school %>% filter(regiao == reg)
  if (nrow(df_reg) < 10) {
    cat(sprintf("\n--- %s: only %d obs, skipping ---\n",
                reg, nrow(df_reg)))
    next
  }
  cat(sprintf("\n--- %s (n=%d) ---\n", reg, nrow(df_reg)))
  cat(sprintf("  Mean dist: %.1f km | Max dist: %.1f km\n",
              mean(df_reg$dist_nearest_school_km, na.rm=TRUE),
              max(df_reg$dist_nearest_school_km, na.rm=TRUE)))

  m_dist <- lm(dist_nearest_school_km ~
                 fpm_dependence_mun +
                 fiscal_autonomy_mun +
                 log_pop_mun +
                 log_gdp_pc_mun,
               data = df_reg)

  coefs <- summary(m_dist)$coefficients
  for (v in c("fpm_dependence_mun", "fiscal_autonomy_mun",
              "log_pop_mun")) {
    if (v %in% rownames(coefs)) {
      cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
                  v,
                  coefs[v, "Estimate"],
                  coefs[v, "Pr(>|t|)"]))
    }
  }
  cat(sprintf("  R2=%.3f\n", summary(m_dist)$r.squared))
}

# ============================================================
# MODEL 4: EXCLUDING RS/SC
# Test if RS/SC commuter towns drive the results
# ============================================================

cat("\n============================================================\n")
cat("SENSITIVITY: EXCLUDING RS AND SC\n")
cat("============================================================\n")

main_no_rssc <- main %>% filter(!SG_UF %in% c("RS", "SC"))

m_no_rssc <- lm(schools_per_10k ~
                  fpm_dependence_mun +
                  fiscal_autonomy_mun +
                  dist_capital_km +
                  log_pop_mun +
                  log_gdp_pc_mun +
                  factor(bioma),
                data = main_no_rssc)

cat("\nCoverage model excluding RS and SC:\n")
print(summary(m_no_rssc))

# Compare with RS/SC only
main_rssc <- main %>% filter(SG_UF %in% c("RS", "SC"))

m_rssc <- lm(schools_per_10k ~
               fpm_dependence_mun +
               fiscal_autonomy_mun +
               log_pop_mun +
               log_gdp_pc_mun,
             data = main_rssc)

cat("\nRS and SC only:\n")
print(summary(m_rssc))

cat("\nScript 95 complete.\n")