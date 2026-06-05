# ==============================================================================
# 11_calculationdistances.R
# Project: Municipal administrative capacity and secondary school
#          accessibility in Brazil
#
# Goal: Compute school accessibility indicators at two levels:
#   - Municipality level  → main models
#   - Settlement level    → robustness checks (granular, pre-aggregation)
#
#   Origins:      GHSL settlement points (unit of lived experience)
#   Destinations: secondary schools — two layers:
#                   (a) any school type  → main DV
#                   (b) municipal schools only → robustness DV
#   Aggregation:  settlement distances → municipality-level summaries
#
# Outputs:
#   data/processed/accessibility_mun.csv     — municipality level
#   data/processed/accessibility_settle.csv  — settlement level
#
# Municipality-level variables:
#   DISTANCE (any school):
#     mean_dist_km        — mean settlement-to-school distance
#     median_dist_km      — median settlement-to-school distance
#     max_dist_km         — worst-case distance in municipality
#     pct_over_10km       — share of settlements >10km from any school
#     pct_over_30km       — share of settlements >30km (primary DV candidate)
#     pct_over_50km       — share of settlements >50km
#     log_mean_dist       — log(mean_dist_km + 1)
#
#   DISTANCE (municipal schools only — robustness DV):
#     mean_dist_mun_km
#     pct_over_30km_mun
#     log_mean_dist_mun
#
#   COVERAGE:
#     n_schools           — total secondary schools in municipality
#     n_schools_mun       — municipal schools only
#     n_schools_state     — state schools
#     schools_per_10k     — schools per 10,000 inhabitants
#     enrollment_per_10k  — secondary enrollment per 10,000 inhabitants
#     has_school          — binary: any secondary school present
#
# Settlement-level variables:
#     CO_MUNICIPIO        — municipality identifier (for merging capacity vars)
#     dist_any_km         — distance to nearest school of any type
#     dist_mun_km         — distance to nearest municipal school
#     log_dist_any        — log(dist_any_km + 1)
#     log_dist_mun        — log(dist_mun_km + 1)
#     + all municipality-level controls merged in
#
# Inputs:
#   data/processed/settlement_centroids_13.gpkg
#   data/processed/brazil_municipalities.gpkg
#   data/processed/cescola_media_clean.csv
#   data/processed/controls_municipios.csv
#
# Note: Distances computed in SIRGAS 2000 (EPSG:5880), projected CRS for Brazil
#
# Author: Ana Paula Muto
# ==============================================================================
 
source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(sf)
 
# ==============================================================================
# CONSTANTS
# ==============================================================================
 
PATH_SETTLE    <- "data/processed/settlement_centroids_12.gpkg"
PATH_MUNIC     <- "data/processed/brazil_municipalities.gpkg"
PATH_ESCOLA    <- "data/processed/cescola_media_clean.csv"
PATH_CTRL      <- "data/processed/controls_municipios.csv"
PATH_OUT_MUN   <- "data/processed/accessibility_mun.csv"
PATH_OUT_SETTLE <- "data/processed/accessibility_settle.csv"
 
CRS_PROJ    <- 5880   # SIRGAS 2000 / Brazil Polyconic — accurate distances
CRS_GEO     <- 4326   # WGS84 — for raw coordinates
 
# ==============================================================================
# LOAD DATA
# ==============================================================================
 
cat("Loading settlements...\n")
settlements <- st_read(PATH_SETTLE, quiet = TRUE)
report_dims(settlements %>% st_drop_geometry(), "settlements")
 
cat("Loading municipality boundaries...\n")
munic <- st_read(PATH_MUNIC, quiet = TRUE)
report_dims(munic %>% st_drop_geometry(), "municipalities")
 
cat("Loading schools...\n")
escola <- load_br_csv(PATH_ESCOLA) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
report_dims(escola, "escola (raw)")
 
cat("Loading controls...\n")
ctrl <- load_br_csv(PATH_CTRL) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
report_dims(ctrl, "controls")
 
# ==============================================================================
# PROJECT TO SIRGAS 2000
# ==============================================================================
 
cat("\nProjecting to SIRGAS 2000 (EPSG:", CRS_PROJ, ")...\n")
sf::sf_use_s2(FALSE)
 
settle_proj <- st_transform(settlements, crs = CRS_PROJ)
munic_proj  <- st_transform(munic,       crs = CRS_PROJ)
 
# ==============================================================================
# BUILD SCHOOL POINT LAYERS
# Two layers: all schools, municipal schools only
# Filter to schools with valid coordinates before projecting
# ==============================================================================
 
cat("\nBuilding school point layers...\n")
 
escola_geo <- escola %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE))
 
cat("Schools with valid coordinates:", nrow(escola_geo),
    "of", nrow(escola), "total\n")
 
# All secondary schools
schools_all <- escola_geo %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = CRS_GEO) %>%
  st_transform(crs = CRS_PROJ)
 
# Municipal schools only (TP_DEPENDENCIA == 3)
schools_mun <- escola_geo %>%
  filter(TP_DEPENDENCIA == 3) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = CRS_GEO) %>%
  st_transform(crs = CRS_PROJ)
 
cat("All schools:", nrow(schools_all), "\n")
cat("Municipal schools:", nrow(schools_mun), "\n")
 
# ==============================================================================
# ASSIGN MUNICIPALITY TO EACH SETTLEMENT
# Primary: spatial join (within polygon)
# Fallback: nearest polygon for settlements on boundaries
# ==============================================================================
 
cat("\nAssigning settlements to municipalities...\n")
t0 <- proc.time()
 
settle_mun <- st_join(
  settle_proj,
  munic_proj %>% select(CO_MUNICIPIO),
  join = st_within,
  left = TRUE
)
 
# Fallback for settlements outside any polygon (boundary artifacts)
n_missing <- sum(is.na(settle_mun$CO_MUNICIPIO))
if (n_missing > 0) {
  cat("Fallback (nearest) for", n_missing, "settlements outside polygons...\n")
  fallback <- st_join(
    settle_proj[is.na(settle_mun$CO_MUNICIPIO), ],
    munic_proj %>% select(CO_MUNICIPIO),
    join = st_nearest_feature
  )
  settle_mun$CO_MUNICIPIO[is.na(settle_mun$CO_MUNICIPIO)] <-
    fallback$CO_MUNICIPIO
}
 
cat("Municipality assignment complete in",
    round((proc.time() - t0)["elapsed"], 1), "seconds\n")
cat("Unassigned settlements remaining:",
    sum(is.na(settle_mun$CO_MUNICIPIO)), "\n")
 
# ==============================================================================
# COMPUTE DISTANCES: SETTLEMENT → NEAREST SCHOOL
# Helper function to avoid repeating the sf distance logic
# ==============================================================================
 
compute_distances <- function(origins, destinations, label) {
  cat("\nComputing distances to", label, "...\n")
  t0 <- proc.time()
  idx    <- st_nearest_feature(origins, destinations)
  dist_m <- st_distance(origins, destinations[idx, ], by_element = TRUE)
  dist_km <- as.numeric(dist_m) / 1000
  cat("Done in", round((proc.time() - t0)["elapsed"], 1), "seconds\n")
  cat("  Mean:", round(mean(dist_km, na.rm = TRUE), 2), "km\n")
  cat("  Max: ", round(max(dist_km,  na.rm = TRUE), 1), "km\n")
  cat("  >10km:", sum(dist_km > 10, na.rm = TRUE), "settlements\n")
  cat("  >30km:", sum(dist_km > 30, na.rm = TRUE), "settlements\n")
  dist_km
}
 
dist_any <- compute_distances(settle_mun, schools_all, "any secondary school")
dist_mun <- compute_distances(settle_mun, schools_mun, "municipal schools only")
 
# ==============================================================================
# ATTACH DISTANCES TO SETTLEMENT DATA
# ==============================================================================
 
settle_dist <- settle_mun %>%
  st_drop_geometry() %>%
  mutate(
    CO_MUNICIPIO  = as.character(CO_MUNICIPIO),
    dist_any_km   = dist_any,
    dist_mun_km   = dist_mun
  )
 
# ==============================================================================
# AGGREGATE TO MUNICIPALITY LEVEL
# ==============================================================================
 
cat("\nAggregating to municipality level...\n")
 
dist_mun_level <- settle_dist %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(
    n_settlements     = n(),
 
    # Any school
    mean_dist_km      = mean(dist_any_km,   na.rm = TRUE),
    median_dist_km    = median(dist_any_km, na.rm = TRUE),
    max_dist_km       = max(dist_any_km,    na.rm = TRUE),
    pct_over_10km     = mean(dist_any_km > 10, na.rm = TRUE),
    pct_over_30km     = mean(dist_any_km > 30, na.rm = TRUE),
    pct_over_50km     = mean(dist_any_km > 50, na.rm = TRUE),
 
    # Municipal schools only
    mean_dist_mun_km  = mean(dist_mun_km,   na.rm = TRUE),
    pct_over_30km_mun = mean(dist_mun_km > 30, na.rm = TRUE),
 
    .groups = "drop"
  ) %>%
  mutate(
    log_mean_dist     = log(mean_dist_km     + 1),
    log_mean_dist_mun = log(mean_dist_mun_km + 1)
  )
 
cat("Municipalities with settlement data:", nrow(dist_mun_level), "\n")
 
# ==============================================================================
# COVERAGE INDICATORS (from escola, aggregated by municipality)
# ==============================================================================
 
cat("\nComputing coverage indicators...\n")
 
coverage <- escola %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(
    n_schools       = n(),
    n_schools_mun   = sum(TP_DEPENDENCIA == 3, na.rm = TRUE),
    n_schools_state = sum(TP_DEPENDENCIA == 2, na.rm = TRUE),
    n_schools_fed   = sum(TP_DEPENDENCIA == 1, na.rm = TRUE),
    n_schools_priv  = sum(TP_DEPENDENCIA == 4, na.rm = TRUE),
    total_enroll    = sum(QT_MAT_MED, na.rm = TRUE),
    .groups = "drop"
  )
 
# ==============================================================================
# MERGE EVERYTHING
# Base: full municipality list from controls
# Left join distances and coverage so municipalities without schools appear
# ==============================================================================
 
cat("\nMerging all components...\n")
 
accessibility <- ctrl %>%
  select(CO_MUNICIPIO, SG_UF, pop_mun, log_pop_mun,
         log_gdp_pc_mun, bioma, dist_capital_km,
         log_dist_capital, urban_share_mun) %>%
  left_join(dist_mun_level, by = "CO_MUNICIPIO") %>%
  left_join(coverage,       by = "CO_MUNICIPIO") %>%
  mutate(
    # Zero-fill coverage for municipalities without any school
    across(c(n_schools, n_schools_mun, n_schools_state,
             n_schools_fed, n_schools_priv, total_enroll),
           ~ replace_na(., 0)),
    has_school         = n_schools > 0,
    schools_per_10k    = n_schools    / (pop_mun / 10000),
    enrollment_per_10k = total_enroll / (pop_mun / 10000)
  )
 
report_dims(accessibility, "accessibility (final)")
 
# ==============================================================================
# DISTRIBUTION DIAGNOSTICS
# Print these to decide on functional form for models
# ==============================================================================
 
cat("\n", strrep("=", 60), "\n")
cat("DISTRIBUTION DIAGNOSTICS — read before choosing model DV\n")
cat(strrep("=", 60), "\n")
 
cat("\n--- Mean distance to any school (km) ---\n")
report_indicators(accessibility, "mean_dist_km")
 
cat("\n--- Log mean distance (log + 1) ---\n")
report_indicators(accessibility, "log_mean_dist")
 
cat("\n--- pct_over_30km (share settlements >30km) ---\n")
report_indicators(accessibility, "pct_over_30km")
 
cat("\n--- Mean distance to municipal school (km) ---\n")
report_indicators(accessibility, "mean_dist_mun_km")
 
cat("\nDistance category distribution (mean_dist_km):\n")
accessibility %>%
  mutate(dist_cat = case_when(
    is.na(mean_dist_km)      ~ "No data",
    mean_dist_km <= 5        ~ "0-5km",
    mean_dist_km <= 10       ~ "5-10km",
    mean_dist_km <= 20       ~ "10-20km",
    mean_dist_km <= 30       ~ "20-30km",
    mean_dist_km <= 50       ~ "30-50km",
    TRUE                     ~ "50km+"
  )) %>%
  count(dist_cat) %>%
  arrange(dist_cat) %>%
  print()
 
cat("\nCoverage distribution:\n")
accessibility %>%
  mutate(cov_cat = case_when(
    n_schools == 0  ~ "No school",
    n_schools == 1  ~ "1 school",
    n_schools <= 3  ~ "2-3 schools",
    n_schools <= 10 ~ "4-10 schools",
    TRUE            ~ "11+ schools"
  )) %>%
  count(cov_cat) %>%
  print()
 
cat("\nTop 10 most inaccessible municipalities:\n")
accessibility %>%
  arrange(desc(mean_dist_km)) %>%
  select(CO_MUNICIPIO, SG_UF, bioma,
         mean_dist_km, pct_over_30km,
         n_settlements, pop_mun) %>%
  head(10) %>%
  print()
 
cat("\nMissing values summary:\n")
# Either specify vars:
report_missing(accessibility, c("mean_dist_km", "log_mean_dist",
                                 "mean_dist_mun_km", "pct_over_30km"))


 
# ==============================================================================
# BUILD SETTLEMENT-LEVEL DATASET
# Merge municipality-level controls down to settlement level
# This is the robustness dataset — unit of observation is a settlement point
# ==============================================================================
 
cat("\nBuilding settlement-level dataset...\n")
 
# Controls to merge down (capacity vars will be added in model scripts)
ctrl_for_merge <- ctrl %>%
  select(CO_MUNICIPIO, SG_UF, pop_mun, log_pop_mun,
         log_gdp_pc_mun, bioma, dist_capital_km,
         log_dist_capital, urban_share_mun)
 
accessibility_settle <- settle_dist %>%
  mutate(
    log_dist_any = log(dist_any_km + 1),
    log_dist_mun = log(dist_mun_km + 1)
  ) %>%
  left_join(ctrl_for_merge, by = "CO_MUNICIPIO")
 
report_dims(accessibility_settle, "accessibility_settle")
 
cat("\nSettlement-level distance summary (any school):\n")
report_indicators(accessibility_settle, "dist_any_km")
 
# ==============================================================================
# SAVE
# ==============================================================================
 
sf::sf_use_s2(TRUE)
 
save_processed(accessibility,        PATH_OUT_MUN)
save_processed(accessibility_settle, PATH_OUT_SETTLE)
 
cat("\nScript 11 complete.\n")
cat("  Municipality level:", PATH_OUT_MUN,
    "—", nrow(accessibility), "rows,",
    ncol(accessibility), "cols\n")
cat("  Settlement level:  ", PATH_OUT_SETTLE,
    "—", nrow(accessibility_settle), "rows,",
    ncol(accessibility_settle), "cols\n")
cat("\nNext step: inspect distribution diagnostics above,\n")
cat("then choose between mean_dist_km (logged) or pct_over_30km\n")
cat("as primary DV. Settlement-level file ready for robustness models.\n")