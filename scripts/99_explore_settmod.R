# ==================================================
# 99_explore_settmod.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Settlement-level models — instead of
#       municipality aggregates, run models at
#       settlement level (76k observations).
#       Each GHSL settlement point is the unit.
#       Outcome: distance from settlement to
#       nearest secondary school.
#       Much more power, directly measures child
#       travel burden at the place where people live.
# Input:  data/processed/settlement_centroids_13.gpkg
#         data/processed/accessibility.csv
#         data/processed/master_mun_main.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(sf)
library(lmtest)
library(sandwich)

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading settlement distances...\n")
# We need settlement-level distances — these were computed
# in script 11 but only saved as municipality aggregates
# We need to rerun the distance computation at settlement level

# Load settlements
settlements <- st_read(
  "data/processed/settlement_centroids_13.gpkg",
  quiet = TRUE)
cat("Settlements:", nrow(settlements), "\n")

# Load municipality boundaries for spatial join
munic <- st_read(
  "data/processed/brazil_municipalities.gpkg",
  quiet = TRUE)

# Load escola
escola <- load_br_csv(
  "data/processed/cescola_media_clean.csv") %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE))

# Load municipality-level controls
main_mun <- load_br_csv(
  "data/processed/master_mun_main.csv") %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

cat("Municipality controls:", nrow(main_mun), "\n")

# ============================================================
# REBUILD SETTLEMENT-LEVEL DISTANCES
# (fast vectorized version from script 11)
# ============================================================

cat("\nRebuilding settlement-level distances...\n")

sf::sf_use_s2(FALSE)

# Project
settle_proj <- st_transform(settlements, crs = 5880)
munic_proj  <- st_transform(munic, crs = 5880)

# Assign municipality to each settlement
cat("Joining settlements to municipalities...\n")
settle_mun <- st_join(
  settle_proj,
  munic_proj %>% select(CO_MUNICIPIO),
  join = st_within,
  left = TRUE
)

# Fix settlements outside polygons
missing <- is.na(settle_mun$CO_MUNICIPIO)
if (sum(missing) > 0) {
  nearest <- st_join(
    settle_proj[missing, ],
    munic_proj %>% select(CO_MUNICIPIO),
    join = st_nearest_feature
  )
  settle_mun$CO_MUNICIPIO[missing] <-
    nearest$CO_MUNICIPIO
}

cat("All settlements assigned to municipality\n")

# Build school points
school_proj <- escola %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
           crs = 4326) %>%
  st_transform(crs = 5880)

# Compute distances (vectorized)
cat("Computing distances...\n")
nearest_idx <- st_nearest_feature(settle_mun, school_proj)
dist_km <- as.numeric(
  st_distance(settle_mun,
              school_proj[nearest_idx, ],
              by_element = TRUE)
) / 1000

sf::sf_use_s2(TRUE)

cat("Distance computation complete\n")
cat("Mean:", round(mean(dist_km), 2), "km\n")
cat("Max:", round(max(dist_km), 1), "km\n")

# ============================================================
# BUILD SETTLEMENT-LEVEL DATASET
# ============================================================

cat("\nBuilding settlement-level dataset...\n")

settle_df <- settle_mun %>%
  st_drop_geometry() %>%
  mutate(
    CO_MUNICIPIO   = as.character(CO_MUNICIPIO),
    dist_km        = dist_km,
    log_dist_km    = log(dist_km + 0.01),
    smod_label     = smod_label
  ) %>%
  # Join municipality-level controls
  left_join(
    main_mun %>%
      select(CO_MUNICIPIO, SG_UF, regiao,
             fpm_dependence_mun,
             fiscal_autonomy_mun,
             adm_capacity_score_mun,
             log_pop_mun, log_gdp_pc_mun,
             bioma, dist_capital_km,
             log_dist_capital, urban_type,
             periphery),
    by = "CO_MUNICIPIO"
  ) %>%
  mutate(
    CO_UF = as.integer(substr(CO_MUNICIPIO, 1, 2)),
    periphery = as.integer(
      regiao %in% c("Norte", "Nordeste")),
    rural_settlement = as.integer(
      smod_label == "rural_cluster")
  )

cat("Settlement dataset:", nrow(settle_df), "rows\n")
cat("Missing municipality controls:",
    sum(is.na(settle_df$fpm_dependence_mun)), "\n")

# ============================================================
# DESCRIPTIVES AT SETTLEMENT LEVEL
# ============================================================

cat("\n--- Settlement-level descriptives ---\n")

cat("\nDistance distribution:\n")
cat("Mean:", round(mean(settle_df$dist_km), 2), "km\n")
cat("Median:", round(median(settle_df$dist_km), 2), "km\n")
cat("% >5km:", round(100*mean(settle_df$dist_km > 5), 1), "\n")
cat("% >10km:", round(100*mean(settle_df$dist_km > 10), 1), "\n")
cat("% >30km:", round(100*mean(settle_df$dist_km > 30), 1), "\n")

cat("\nBy settlement type:\n")
settle_df %>%
  group_by(smod_label) %>%
  summarise(
    n        = n(),
    mean_dist = round(mean(dist_km), 2),
    pct_over_10km = round(100*mean(dist_km > 10), 1),
    .groups  = "drop"
  ) %>%
  arrange(desc(mean_dist)) %>%
  print()

cat("\nBy region:\n")
settle_df %>%
  filter(!is.na(regiao)) %>%
  group_by(regiao) %>%
  summarise(
    n        = n(),
    mean_dist = round(mean(dist_km), 2),
    median_dist = round(median(dist_km), 2),
    pct_over_10km = round(100*mean(dist_km > 10), 1),
    .groups  = "drop"
  ) %>%
  arrange(desc(mean_dist)) %>%
  print()

# ============================================================
# SETTLEMENT-LEVEL MODELS
# Outcome: log_dist_km
# Controls at municipality level (clustered by municipality)
# ============================================================

cat("\n============================================================\n")
cat("SETTLEMENT-LEVEL MODELS\n")
cat("Outcome: log distance from settlement to nearest school\n")
cat("SE clustered by municipality\n")
cat("============================================================\n")

# Clean sample
settle_clean <- settle_df %>%
  filter(!is.na(log_dist_km),
         !is.na(fpm_dependence_mun),
         !is.na(fiscal_autonomy_mun),
         !is.na(adm_capacity_score_mun),
         !is.na(log_pop_mun),
         !is.na(log_gdp_pc_mun),
         !is.na(bioma))

cat("Clean settlement sample:", nrow(settle_clean), "\n")

# S1: Base model
s1 <- lm(log_dist_km ~
           fpm_dependence_mun +
           fiscal_autonomy_mun +
           log_pop_mun + log_gdp_pc_mun +
           factor(bioma),
         data = settle_clean)

cat("\nS1: Base model\n")
print(coeftest(s1,
               vcov = vcovCL(s1,
                             cluster = ~CO_MUNICIPIO,
                             data = settle_clean)
               )[1:6, ])
cat(sprintf("R²=%.3f  n=%d\n",
            summary(s1)$r.squared,
            nobs(s1)))

# S2: Add adm_capacity
s2 <- lm(log_dist_km ~
           fpm_dependence_mun +
           fiscal_autonomy_mun +
           adm_capacity_score_mun +
           log_pop_mun + log_gdp_pc_mun +
           factor(bioma),
         data = settle_clean)

cat("\nS2: Add adm capacity\n")
print(coeftest(s2,
               vcov = vcovCL(s2,
                             cluster = ~CO_MUNICIPIO,
                             data = settle_clean)
               )[1:7, ])
cat(sprintf("R²=%.3f  n=%d\n",
            summary(s2)$r.squared,
            nobs(s2)))

# S3: Add rural settlement type
s3 <- lm(log_dist_km ~
           fpm_dependence_mun +
           fiscal_autonomy_mun +
           adm_capacity_score_mun +
           rural_settlement +
           log_pop_mun + log_gdp_pc_mun +
           factor(bioma),
         data = settle_clean)

cat("\nS3: Add rural settlement type\n")
print(coeftest(s3,
               vcov = vcovCL(s3,
                             cluster = ~CO_MUNICIPIO,
                             data = settle_clean)
               )[1:8, ])
cat(sprintf("R²=%.3f  n=%d\n",
            summary(s3)$r.squared,
            nobs(s3)))

# S4: Add state fixed effects
s4 <- lm(log_dist_km ~
           fpm_dependence_mun +
           fiscal_autonomy_mun +
           adm_capacity_score_mun +
           rural_settlement +
           log_pop_mun + log_gdp_pc_mun +
           factor(SG_UF),
         data = settle_clean)

key_vars <- c("fpm_dependence_mun",
              "fiscal_autonomy_mun",
              "adm_capacity_score_mun",
              "rural_settlement",
              "log_pop_mun",
              "log_gdp_pc_mun")

cat("\nS4: State fixed effects\n")
s4_robust <- coeftest(
  s4,
  vcov = vcovCL(s4,
                cluster = ~CO_MUNICIPIO,
                data = settle_clean))
for (v in key_vars) {
  cat(sprintf("  %-30s coef=%7.3f p=%.4f\n",
              v,
              s4_robust[v, "Estimate"],
              s4_robust[v, "Pr(>|t|)"]))
}
cat(sprintf("R²=%.3f  n=%d\n",
            summary(s4)$r.squared,
            nobs(s4)))

# ============================================================
# SETTLEMENT MODELS BY REGION
# ============================================================

cat("\n============================================================\n")
cat("SETTLEMENT MODELS BY REGION\n")
cat("============================================================\n")

regions <- c("Norte","Nordeste","Sudeste",
             "Sul","Centro-Oeste")

for (reg in regions) {
  df_reg <- settle_clean %>%
    filter(regiao == reg)

  if (nrow(df_reg) < 100) next

  m <- lm(log_dist_km ~
            fpm_dependence_mun +
            fiscal_autonomy_mun +
            log_pop_mun + log_gdp_pc_mun,
          data = df_reg)

  robust <- coeftest(
    m,
    vcov = vcovCL(m,
                  cluster = ~CO_MUNICIPIO,
                  data = df_reg))

  cat(sprintf("\n--- %s (n=%d settlements) ---\n",
              reg, nrow(df_reg)))
  for (v in c("fpm_dependence_mun",
              "fiscal_autonomy_mun")) {
    cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
                v,
                robust[v, "Estimate"],
                robust[v, "Pr(>|t|)"]))
  }
  cat(sprintf("  R²=%.3f\n", summary(m)$r.squared))
}

# ============================================================
# RURAL vs URBAN SETTLEMENTS
# ============================================================

cat("\n============================================================\n")
cat("RURAL vs URBAN SETTLEMENTS\n")
cat("============================================================\n")

for (stype in c("rural_cluster", "suburban",
                "semi_dense_urban", "urban_centre")) {
  df_s <- settle_clean %>%
    filter(smod_label == stype)

  if (nrow(df_s) < 50) next

  m <- lm(log_dist_km ~
            fpm_dependence_mun +
            fiscal_autonomy_mun +
            log_pop_mun + log_gdp_pc_mun +
            factor(bioma),
          data = df_s)

  robust <- coeftest(
    m,
    vcov = vcovCL(m,
                  cluster = ~CO_MUNICIPIO,
                  data = df_s))

  cat(sprintf("\n--- %s (n=%d) ---\n",
              stype, nrow(df_s)))
  for (v in c("fpm_dependence_mun",
              "fiscal_autonomy_mun")) {
    cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
                v,
                robust[v, "Estimate"],
                robust[v, "Pr(>|t|)"]))
  }
  cat(sprintf("  R²=%.3f\n", summary(m)$r.squared))
}

# ============================================================
# COMPARE MUNICIPALITY vs SETTLEMENT LEVEL
# ============================================================

cat("\n============================================================\n")
cat("MUNICIPALITY vs SETTLEMENT LEVEL COMPARISON\n")
cat("============================================================\n")

# Municipality level equivalent
m_mun <- lm(log_mean_dist_school ~
              fpm_dependence_mun +
              fiscal_autonomy_mun +
              adm_capacity_score_mun +
              log_pop_mun + log_gdp_pc_mun +
              factor(bioma),
            data = main_mun %>%
              filter(!is.na(log_mean_dist_school),
                     !is.na(fpm_dependence_mun),
                     !is.na(fiscal_autonomy_mun),
                     !is.na(adm_capacity_score_mun),
                     !is.na(bioma)))

cat("\nMunicipality level (n=",
    nobs(m_mun), "):\n", sep="")
for (v in key_vars[1:3]) {
  cat(sprintf("  %-30s coef=%7.3f\n",
              v, coef(m_mun)[v]))
}
cat(sprintf("  R²=%.3f\n",
            summary(m_mun)$r.squared))

cat("\nSettlement level (n=",
    nobs(s2), "):\n", sep="")
for (v in key_vars[1:3]) {
  cat(sprintf("  %-30s coef=%7.3f\n",
              v, coef(s2)[v]))
}
cat(sprintf("  R²=%.3f\n",
            summary(s2)$r.squared))

cat("\nScript 99 complete.\n")