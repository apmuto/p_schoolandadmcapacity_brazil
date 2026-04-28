# ==================================================
# 94_explore_models.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Preliminary model exploration at
#       municipality level with updated outcomes:
#       - log_mean_dist_school (distance)
#       - log_schools_per_10k (coverage)
#       Also: Moran's I spatial autocorrelation test
#       Models by urban type and region
# Input:  data/processed/master_mun_main.csv
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

cat("Loading master_mun_main...\n")
main <- load_br_csv("data/processed/master_mun_main.csv") %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
report_dims(main, "master_mun_main")

# ============================================================
# QUICK SUMMARY
# ============================================================

cat("\n--- Key variable summary ---\n")
report_indicators(main, c(
  "log_mean_dist_school",
  "log_schools_per_10k",
  "mean_dist_school_km",
  "schools_per_10k",
  "adm_capacity_score_mun",
  "fiscal_autonomy_mun",
  "fpm_dependence_mun",
  "dist_capital_km",
  "log_pop_mun",
  "log_gdp_pc_mun"
))

# ============================================================
# OUTCOME 1: LOG MEAN DISTANCE TO SCHOOL
# All municipalities with settlement data (n~5,398)
# ============================================================

cat("\n============================================================\n")
cat("OUTCOME 1: LOG MEAN DISTANCE TO SCHOOL\n")
cat("============================================================\n")

# M1a: Behavioral capacity only
m1a <- lm(log_mean_dist_school ~
            adm_capacity_score_mun +
            log_pop_mun + log_gdp_pc_mun,
          data = main)
cat("\nM1a: log_dist ~ adm_capacity\n")
print(summary(m1a))

# M1b: Add geographic controls
m1b <- lm(log_mean_dist_school ~
            adm_capacity_score_mun +
            dist_capital_km +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = main)
cat("\nM1b: log_dist ~ adm_capacity + geography\n")
print(summary(m1b))

# M1c: Tilly indicator
m1c <- lm(log_mean_dist_school ~
            fiscal_autonomy_mun +
            dist_capital_km +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = main)
cat("\nM1c: log_dist ~ fiscal_autonomy (Tilly)\n")
print(summary(m1c))

# M1d: FPM dependence
m1d <- lm(log_mean_dist_school ~
            fpm_dependence_mun +
            dist_capital_km +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = main)
cat("\nM1d: log_dist ~ fpm_dependence\n")
print(summary(m1d))

# M1e: Full model — all capacity dimensions
m1e <- lm(log_mean_dist_school ~
            adm_capacity_score_mun +
            fiscal_autonomy_mun +
            fpm_dependence_mun +
            dist_capital_km +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = main)
cat("\nM1e: log_dist ~ full model\n")
print(summary(m1e))

# AIC comparison
cat("\n--- AIC comparison (distance models) ---\n")
cat("M1a:", AIC(m1a), "\n")
cat("M1b:", AIC(m1b), "\n")
cat("M1c:", AIC(m1c), "\n")
cat("M1d:", AIC(m1d), "\n")
cat("M1e:", AIC(m1e), "\n")

# ============================================================
# OUTCOME 2: LOG SCHOOLS PER 10K
# ============================================================

cat("\n============================================================\n")
cat("OUTCOME 2: LOG SCHOOLS PER 10K\n")
cat("============================================================\n")

# M2a: Behavioral capacity
m2a <- lm(log_schools_per_10k ~
            adm_capacity_score_mun +
            log_pop_mun + log_gdp_pc_mun,
          data = main)
cat("\nM2a: log_coverage ~ adm_capacity\n")
print(summary(m2a))

# M2b: Add geography
m2b <- lm(log_schools_per_10k ~
            adm_capacity_score_mun +
            dist_capital_km +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = main)
cat("\nM2b: log_coverage ~ adm_capacity + geography\n")
print(summary(m2b))

# M2c: Tilly
m2c <- lm(log_schools_per_10k ~
            fiscal_autonomy_mun +
            dist_capital_km +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = main)
cat("\nM2c: log_coverage ~ fiscal_autonomy (Tilly)\n")
print(summary(m2c))

# M2d: FPM
m2d <- lm(log_schools_per_10k ~
            fpm_dependence_mun +
            dist_capital_km +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = main)
cat("\nM2d: log_coverage ~ fpm_dependence\n")
print(summary(m2d))

# M2e: Full model
m2e <- lm(log_schools_per_10k ~
            adm_capacity_score_mun +
            fiscal_autonomy_mun +
            fpm_dependence_mun +
            dist_capital_km +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = main)
cat("\nM2e: log_coverage ~ full model\n")
print(summary(m2e))

# AIC comparison
cat("\n--- AIC comparison (coverage models) ---\n")
cat("M2a:", AIC(m2a), "\n")
cat("M2b:", AIC(m2b), "\n")
cat("M2c:", AIC(m2c), "\n")
cat("M2d:", AIC(m2d), "\n")
cat("M2e:", AIC(m2e), "\n")

# ============================================================
# MODELS BY URBAN TYPE
# ============================================================

cat("\n============================================================\n")
cat("MODELS BY URBAN TYPE\n")
cat("============================================================\n")

for (utype in c("Urban", "Mixed", "Rural")) {
  df <- main %>% filter(urban_type == utype,
                        !is.na(log_mean_dist_school))
  cat(sprintf("\n--- %s (n=%d) ---\n", utype, nrow(df)))

  m <- lm(log_mean_dist_school ~
            fpm_dependence_mun +
            fiscal_autonomy_mun +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = df)

  coefs <- summary(m)$coefficients
  for (v in c("fpm_dependence_mun", "fiscal_autonomy_mun",
              "log_pop_mun", "log_gdp_pc_mun")) {
    if (v %in% rownames(coefs)) {
      cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
                  v, coefs[v,"Estimate"],
                  coefs[v,"Pr(>|t|)"]))
    }
  }
  cat(sprintf("  R2=%.3f  n=%d\n",
              summary(m)$r.squared, nobs(m)))
}

# ============================================================
# MODELS BY REGION
# ============================================================

cat("\n============================================================\n")
cat("MODELS BY REGION\n")
cat("============================================================\n")

for (reg in c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")) {
  df <- main %>% filter(regiao == reg,
                        !is.na(log_mean_dist_school))
  cat(sprintf("\n--- %s (n=%d) ---\n", reg, nrow(df)))

  if (nrow(df) < 30) {
    cat("  Too few observations, skipping\n")
    next
  }

  m <- lm(log_mean_dist_school ~
            fpm_dependence_mun +
            fiscal_autonomy_mun +
            log_pop_mun + log_gdp_pc_mun,
          data = df)

  coefs <- summary(m)$coefficients
  for (v in c("fpm_dependence_mun", "fiscal_autonomy_mun",
              "log_pop_mun", "log_gdp_pc_mun")) {
    if (v %in% rownames(coefs)) {
      cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
                  v, coefs[v,"Estimate"],
                  coefs[v,"Pr(>|t|)"]))
    }
  }
  cat(sprintf("  R2=%.3f  n=%d\n",
              summary(m)$r.squared, nobs(m)))
}

# ============================================================
# MORAN'S I TEST
# Spatial autocorrelation in residuals
# ============================================================

cat("\n============================================================\n")
cat("MORAN'S I SPATIAL AUTOCORRELATION TEST\n")
cat("============================================================\n")

# Load municipality centroids for spatial weights
cat("Loading municipality boundaries...\n")
munic <- st_read("data/processed/brazil_municipalities.gpkg",
                 quiet = TRUE)

# Join with main data
main_sf <- main %>%
  filter(!is.na(log_mean_dist_school),
         !is.na(adm_capacity_score_mun)) %>%
  left_join(munic %>%
              st_drop_geometry() %>%
              select(CO_MUNICIPIO),
            by = "CO_MUNICIPIO") %>%
  left_join(munic, by = "CO_MUNICIPIO") %>%
  st_as_sf()

cat("Municipalities for Moran test:", nrow(main_sf), "\n")

# Get centroids
sf::sf_use_s2(FALSE)
centroids <- st_centroid(main_sf)
coords    <- st_coordinates(centroids)
sf::sf_use_s2(TRUE)

# Build spatial weights — k nearest neighbors (k=5)
cat("Building spatial weights (k=5 nearest neighbors)...\n")
knn5   <- knearneigh(coords, k = 5)
nb5    <- knn2nb(knn5)
lw5    <- nb2listw(nb5, style = "W")

# Moran's I on main outcome
cat("\nMoran's I test — log_mean_dist_school:\n")
moran_dist <- moran.test(main_sf$log_mean_dist_school, lw5)
print(moran_dist)

cat("\nMoran's I test — log_schools_per_10k:\n")
main_sf2 <- main %>%
  filter(!is.na(log_schools_per_10k),
         !is.na(adm_capacity_score_mun)) %>%
  left_join(munic, by = "CO_MUNICIPIO") %>%
  st_as_sf()
sf::sf_use_s2(FALSE)
centroids2 <- st_centroid(main_sf2)
coords2    <- st_coordinates(centroids2)
sf::sf_use_s2(TRUE)
knn5b  <- knearneigh(coords2, k = 5)
nb5b   <- knn2nb(knn5b)
lw5b   <- nb2listw(nb5b, style = "W")
moran_cov <- moran.test(main_sf2$log_schools_per_10k, lw5b)
print(moran_cov)

# Moran's I on model residuals
cat("\nMoran's I on M1b residuals:\n")
main_m1b <- main %>%
  filter(!is.na(log_mean_dist_school),
         !is.na(adm_capacity_score_mun),
         !is.na(dist_capital_km),
         !is.na(bioma))
m1b_clean <- lm(log_mean_dist_school ~
                  adm_capacity_score_mun +
                  dist_capital_km +
                  log_pop_mun + log_gdp_pc_mun +
                  factor(bioma),
                data = main_m1b)

main_m1b_sf <- main_m1b %>%
  left_join(munic, by = "CO_MUNICIPIO") %>%
  st_as_sf()
sf::sf_use_s2(FALSE)
cents_m1b <- st_centroid(main_m1b_sf)
coords_m1b <- st_coordinates(cents_m1b)
sf::sf_use_s2(TRUE)
knn_m1b <- knn2nb(knearneigh(coords_m1b, k = 5))
lw_m1b  <- nb2listw(knn_m1b, style = "W")
moran_resid <- moran.test(residuals(m1b_clean), lw_m1b)
print(moran_resid)

# ============================================================
# CORRELATION TABLE
# ============================================================

cat("\n============================================================\n")
cat("CORRELATIONS WITH OUTCOMES\n")
cat("============================================================\n")

cor_vars <- main %>%
  select(
    log_mean_dist_school,
    log_schools_per_10k,
    adm_capacity_score_mun,
    fiscal_autonomy_mun,
    fpm_dependence_mun,
    dist_capital_km,
    log_pop_mun,
    log_gdp_pc_mun
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

print(cor_vars)

cat("\nScript 94 complete.\n")