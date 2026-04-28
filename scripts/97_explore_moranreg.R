# ==================================================
# 97_explore_moranreg.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Test spatial autocorrelation by region
#       and urban type. Tests whether Moran's I
#       is stronger in periphery (Norte/Nordeste)
#       consistent with O'Donnell brown areas.
#       Also tests residual autocorrelation after
#       controlling for fiscal capacity.
# Input:  data/processed/master_mun_main.csv
#         data/processed/brazil_municipalities.gpkg
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(sf)
library(spdep)

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading data...\n")
main <- load_br_csv("data/processed/master_mun_main.csv") %>%
  mutate(
    CO_MUNICIPIO = as.character(CO_MUNICIPIO),
    CO_UF = as.integer(substr(CO_MUNICIPIO, 1, 2)),
    regiao = case_when(
      CO_UF %in% c(11,12,13,14,15,16,17) ~ "Norte",
      CO_UF %in% c(21,22,23,24,25,26,27,28,29) ~ "Nordeste",
      CO_UF %in% c(31,32,33,35) ~ "Sudeste",
      CO_UF %in% c(41,42,43) ~ "Sul",
      CO_UF %in% c(50,51,52,53) ~ "Centro-Oeste",
      TRUE ~ NA_character_
    ),
    periphery = regiao %in% c("Norte", "Nordeste")
  )

munic <- st_read("data/processed/brazil_municipalities.gpkg",
                 quiet = TRUE)

report_dims(main, "master_mun_main")

# ============================================================
# HELPER FUNCTION
# Build spatial weights and run Moran test for a subset
# ============================================================

run_moran <- function(df, outcome_var, label = "") {
  # Need at least 30 observations
  if (nrow(df) < 30) {
    cat(sprintf("  %s: too few obs (%d), skipping\n",
                label, nrow(df)))
    return(NULL)
  }

  # Join to municipality geometries
 df_sf <- df %>%
  filter(!is.na(.data[[outcome_var]])) %>%
  left_join(munic, by = "CO_MUNICIPIO") %>%
  st_as_sf() %>%
  filter(!is.na(.data[[outcome_var]]),
         !st_is_empty(geom),
         !is.na(st_dimension(geom)))
  
  if (nrow(df_sf) < 30) {
    cat(sprintf("  %s: too few obs after join, skipping\n",
                label))
    return(NULL)
  }

  # Build spatial weights
  sf::sf_use_s2(FALSE)
  coords <- tryCatch(
    st_coordinates(st_centroid(df_sf)),
    warning = function(w) st_coordinates(st_centroid(df_sf))
  )
  sf::sf_use_s2(TRUE)

knn <- knn2nb(knearneigh(coords, k = min(5, nrow(df_sf)-1)))
lw  <- nb2listw(knn, style = "W",
                  zero.policy = TRUE)

  # Moran test
  mt <- moran.test(df_sf[[outcome_var]], lw,
                   zero.policy = TRUE)

  cat(sprintf("  %-20s I=%.3f  p=%.4f  n=%d\n",
              label,
              mt$estimate["Moran I statistic"],
              mt$p.value,
              nrow(df_sf)))

  return(list(
    label   = label,
    I       = mt$estimate["Moran I statistic"],
    p_value = mt$p.value,
    n       = nrow(df_sf)
  ))
}

# ============================================================
# PART 1: MORAN'S I BY REGION
# Is spatial autocorrelation stronger in periphery?
# ============================================================

cat("\n============================================================\n")
cat("PART 1: MORAN'S I BY REGION\n")
cat("Outcome: log_mean_dist_school\n")
cat("============================================================\n")

regions <- c("Norte", "Nordeste", "Sudeste",
             "Sul", "Centro-Oeste")

results_region <- list()
for (reg in regions) {
  df_reg <- main %>% filter(regiao == reg)
  results_region[[reg]] <- run_moran(
    df_reg, "log_mean_dist_school", reg
  )
}

cat("\nOutcome: log_schools_per_10k\n")
for (reg in regions) {
  df_reg <- main %>% filter(regiao == reg)
  run_moran(df_reg, "log_schools_per_10k", reg)
}

# ============================================================
# PART 2: MORAN'S I BY URBAN TYPE
# ============================================================

cat("\n============================================================\n")
cat("PART 2: MORAN'S I BY URBAN TYPE\n")
cat("Outcome: log_mean_dist_school\n")
cat("============================================================\n")

for (utype in c("Urban", "Mixed", "Rural")) {
  df_u <- main %>% filter(urban_type == utype)
  run_moran(df_u, "log_mean_dist_school", utype)
}

# ============================================================
# PART 3: MORAN'S I ON MODEL RESIDUALS BY REGION
# Does fiscal capacity explain spatial clustering?
# If Moran's I drops substantially after controlling
# for fiscal capacity, capacity explains the clustering
# ============================================================

cat("\n============================================================\n")
cat("PART 3: MORAN'S I ON RESIDUALS BY REGION\n")
cat("Does fiscal capacity explain spatial clustering?\n")
cat("============================================================\n")

for (reg in regions) {
  df_reg <- main %>%
    filter(regiao == reg,
           !is.na(log_mean_dist_school),
           !is.na(adm_capacity_score_mun),
           !is.na(fiscal_autonomy_mun),
           !is.na(fpm_dependence_mun),
           !is.na(log_pop_mun),
           !is.na(log_gdp_pc_mun))

  if (nrow(df_reg) < 30) next

  # Fit model
  m <- lm(log_mean_dist_school ~
            adm_capacity_score_mun +
            fiscal_autonomy_mun +
            fpm_dependence_mun +
            log_pop_mun + log_gdp_pc_mun,
          data = df_reg)

  # Get residuals and join to geometry
  df_reg$resid <- residuals(m)

  df_sf <- df_reg %>%
    left_join(munic, by = "CO_MUNICIPIO") %>%
    st_as_sf()

  sf::sf_use_s2(FALSE)
  coords <- tryCatch(
    st_coordinates(st_centroid(df_sf)),
    warning = function(w) st_coordinates(st_centroid(df_sf))
  )
  sf::sf_use_s2(TRUE)

  knn <- knn2nb(knearneigh(coords, k = 5))
  lw  <- nb2listw(knn, style = "W",
                  zero.policy = TRUE)

  # Raw outcome Moran
  mt_raw <- moran.test(df_sf$log_mean_dist_school,
                       lw, zero.policy = TRUE)

  # Residual Moran
  mt_res <- moran.test(df_sf$resid,
                       lw, zero.policy = TRUE)

  cat(sprintf("\n%s (n=%d, R²=%.3f):\n",
              reg, nrow(df_sf),
              summary(m)$r.squared))
  cat(sprintf("  Raw outcome  Moran I=%.3f  p=%.4f\n",
              mt_raw$estimate["Moran I statistic"],
              mt_raw$p.value))
  cat(sprintf("  Residuals    Moran I=%.3f  p=%.4f\n",
              mt_res$estimate["Moran I statistic"],
              mt_res$p.value))
  cat(sprintf("  Reduction:   %.1f%%\n",
              100 * (1 - mt_res$estimate["Moran I statistic"] /
                         mt_raw$estimate["Moran I statistic"])))
}

# ============================================================
# PART 4: PERIPHERY vs CORE COMPARISON
# ============================================================

cat("\n============================================================\n")
cat("PART 4: PERIPHERY vs CORE\n")
cat("============================================================\n")

for (label in c("Periphery (Norte+Nordeste)",
                "Core (Sul+Sudeste+Centro-Oeste)")) {
  is_periph <- label == "Periphery (Norte+Nordeste)"
  df <- main %>% filter(periphery == is_periph)
  run_moran(df, "log_mean_dist_school", label)
}

cat("\nScript 97 complete.\n")