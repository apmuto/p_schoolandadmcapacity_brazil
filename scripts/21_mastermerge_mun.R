# ==================================================
# 21_mastermerge_mun.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Build municipality-level master dataset
#       combining all cleaned data sources.
#       Unit of analysis: municipality (5,570 rows)
# Main outcomes:
#   mean_dist_school_km  — avg settlement-to-school
#   pct_over_10km        — % settlements >10km
#   pct_over_30km        — % settlements >30km
#   schools_per_10k      — coverage density
# Input:  data/processed/accessibility.csv
#         data/processed/coverage.csv
#         data/processed/finbra_municipios.csv
#         data/processed/finbra_estados.csv
#         data/processed/capag_municipios.csv
#         data/processed/capag_estados.csv
#         data/processed/controls_municipios.csv
#         data/processed/controls_estados.csv
# Output: data/processed/master_mun_full.csv
#         data/processed/master_mun_main.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)

# ============================================================
# CONSTANTS
# ============================================================

PATH_ACCESS     <- "data/processed/accessibility.csv"
PATH_COVERAGE   <- "data/processed/coverage.csv"
PATH_FINBRA_MUN <- "data/processed/finbra_municipios.csv"
PATH_FINBRA_EST <- "data/processed/finbra_estados.csv"
PATH_CAPAG_MUN  <- "data/processed/capag_municipios.csv"
PATH_CAPAG_EST  <- "data/processed/capag_estados.csv"
PATH_CTRL_MUN   <- "data/processed/controls_municipios.csv"
PATH_CTRL_EST   <- "data/processed/controls_estados.csv"
PATH_OUT_FULL   <- "data/processed/master_mun_full.csv"
PATH_OUT_MAIN   <- "data/processed/master_mun_main.csv"

# ============================================================
# LOAD ALL FILES
# ============================================================

cat("Loading processed files...\n")

acc        <- load_br_csv(PATH_ACCESS) %>%
              mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
cov        <- load_br_csv(PATH_COVERAGE) %>%
              mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
finbra_mun <- load_br_csv(PATH_FINBRA_MUN) %>%
              mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
finbra_est <- load_br_csv(PATH_FINBRA_EST)
capag_mun  <- load_br_csv(PATH_CAPAG_MUN) %>%
              mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
capag_est  <- load_br_csv(PATH_CAPAG_EST)
ctrl_mun   <- load_br_csv(PATH_CTRL_MUN) %>%
              mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
ctrl_est   <- load_br_csv(PATH_CTRL_EST)

report_dims(acc,        "accessibility")
report_dims(cov,        "coverage")
report_dims(finbra_mun, "finbra_municipios")
report_dims(finbra_est, "finbra_estados")
report_dims(capag_mun,  "capag_municipios")
report_dims(capag_est,  "capag_estados")
report_dims(ctrl_mun,   "controls_municipios")
report_dims(ctrl_est,   "controls_estados")

# ============================================================
# STANDARDIZE MERGE KEYS
# ============================================================

# Use coverage as base — has all 5,571 municipalities
# accessibility only has 5,399 (municipalities with settlements)
# Missing municipalities get NA for distance variables

# ============================================================
# CHECK MERGE KEY OVERLAPS
# ============================================================

cat("\n--- Merge key overlap checks ---\n")
n_base <- n_distinct(cov$CO_MUNICIPIO)
cat("Base municipalities (coverage):", n_base, "\n")

cat("Overlap cov <-> accessibility:",
    sum(unique(cov$CO_MUNICIPIO) %in%
        acc$CO_MUNICIPIO), "/", n_base, "\n")
cat("Overlap cov <-> finbra_mun:",
    sum(unique(cov$CO_MUNICIPIO) %in%
        finbra_mun$CO_MUNICIPIO), "/", n_base, "\n")
cat("Overlap cov <-> capag_mun:",
    sum(unique(cov$CO_MUNICIPIO) %in%
        capag_mun$CO_MUNICIPIO), "/", n_base, "\n")
cat("Overlap cov <-> ctrl_mun:",
    sum(unique(cov$CO_MUNICIPIO) %in%
        ctrl_mun$CO_MUNICIPIO), "/", n_base, "\n")
cat("Overlap cov <-> finbra_est:",
    sum(unique(cov$SG_UF) %in%
        finbra_est$SG_UF), "/ 27\n")
cat("Overlap cov <-> ctrl_est:",
    sum(unique(cov$SG_UF) %in%
        ctrl_est$SG_UF), "/ 27\n")

# ============================================================
# BUILD MASTER_MUN_FULL
# Base: coverage (5,571 municipalities — most complete)
# ============================================================

cat("\n--- Building master_mun_full ---\n")

master_mun_full <- cov %>%

  # Accessibility — settlement-level distance measures
  left_join(
    acc %>%
      select(CO_MUNICIPIO,
             n_settlements,
             mean_dist_school_km,
             median_dist_school_km,
             max_dist_school_km,
             log_mean_dist_school,
             log_median_dist_school,
             log_max_dist_school,
             pct_over_10km,
             pct_over_30km,
             pct_over_50km),
    by = "CO_MUNICIPIO"
  ) %>%

  # Municipal fiscal capacity
  left_join(
    finbra_mun %>%
      select(CO_MUNICIPIO,
             receita_total, receita_propria,
             transf_total, transf_uniao, fpm,
             desp_liquidadas, desp_educacao,
             fiscal_autonomy_mun, transfer_dependence_mun,
             fpm_dependence_mun, edu_share_mun,
             budget_execution_mun, debt_ratio_mun,
             adm_capacity_score_mun),
    by = "CO_MUNICIPIO"
  ) %>%

  # Municipal CAPAG
  left_join(
    capag_mun %>%
      select(CO_MUNICIPIO, capag, capag_numeric,
             dca_2024, capag_rebaixada),
    by = "CO_MUNICIPIO"
  ) %>%

  # Additional municipal controls
left_join(
    ctrl_mun %>%
      select(CO_MUNICIPIO,
             area_km2_mun, pop_density_mun,
             log_area_mun, log_pop_density_mun,
             log_dist_capital),
    by = "CO_MUNICIPIO"
  ) %>%

  # State fiscal capacity
  left_join(
    finbra_est %>%
      select(SG_UF,
             fiscal_autonomy_est, transfer_dependence_est,
             fpm_dependence_est, edu_share_est,
             budget_execution_est, debt_ratio_est,
             adm_capacity_score_est),
    by = "SG_UF"
  ) %>%

  # State CAPAG
  left_join(
    capag_est %>%
      select(SG_UF,
             capag_estado, capag_estado_numeric),
    by = "SG_UF"
  ) %>%

  # State controls
  left_join(
    ctrl_est %>%
      select(SG_UF,
             pop_est, gdp_pc_est,
             log_pop_est, log_gdp_pc_est,
             log_area_est, log_pop_density_est),
    by = "SG_UF"
  ) %>%

  # Recreate has_school from n_schools
  mutate(has_school = n_schools > 0)

check_merge(master_mun_full, nrow(cov), "master_mun_full")
report_dims(master_mun_full, "master_mun_full")

# ============================================================
# ADD DERIVED VARIABLES
# ============================================================

master_mun_full <- master_mun_full %>%
  mutate(
    # Urban/rural/mixed classification
    urban_type = case_when(
      urban_share_mun >= 0.8 ~ "Urban",
      urban_share_mun >= 0.5 ~ "Mixed",
      urban_share_mun <  0.5 ~ "Rural",
      TRUE ~ NA_character_
    ),

    # Population size category
    pop_type = case_when(
      pop_mun >= 20000 ~ "Large",
      pop_mun >= 5000  ~ "Medium",
      pop_mun <  5000  ~ "Small",
      TRUE ~ NA_character_
    ),

    # Region
    CO_UF = as.integer(substr(CO_MUNICIPIO, 1, 2)),
    regiao = case_when(
      CO_UF %in% c(11,12,13,14,15,16,17) ~ "Norte",
      CO_UF %in% c(21,22,23,24,25,26,27,28,29) ~ "Nordeste",
      CO_UF %in% c(31,32,33,35) ~ "Sudeste",
      CO_UF %in% c(41,42,43) ~ "Sul",
      CO_UF %in% c(50,51,52,53) ~ "Centro-Oeste",
      TRUE ~ NA_character_
    ),
    periphery = regiao %in% c("Norte", "Nordeste"),

    # Log schools per 10k (for models)
    log_schools_per_10k = log(schools_per_10k + 0.01)
  )

# ============================================================
# MISSINGNESS CHECK
# ============================================================

cat("\n--- Key variable missingness ---\n")
report_missing(master_mun_full, c(
  "CO_MUNICIPIO", "SG_UF",
  "mean_dist_school_km", "log_mean_dist_school",
  "pct_over_10km", "pct_over_30km",
  "schools_per_10k", "log_schools_per_10k",
  "adm_capacity_score_mun", "adm_capacity_score_est",
  "fiscal_autonomy_mun",
  "log_pop_mun", "log_gdp_pc_mun",
  "bioma", "dist_capital_km",
  "capag_numeric"
))

# ============================================================
# BUILD MASTER_MUN_MAIN
# ============================================================

cat("\n--- Building master_mun_main ---\n")

master_mun_main <- master_mun_full %>%
  select(
    # --- IDENTIFIERS ---
    CO_MUNICIPIO, SG_UF, regiao, periphery,

    # --- MUNICIPALITY CHARACTERISTICS ---
    urban_type, pop_type,
    urban_share_mun,

    # --- MAIN OUTCOME VARIABLES ---
    # Distance measures (from settlement-level analysis)
    mean_dist_school_km,
    log_mean_dist_school,
    median_dist_school_km,
    max_dist_school_km,
    pct_over_10km,
    pct_over_30km,
    pct_over_50km,

    # Coverage measures
    has_school,
    n_schools,
    schools_per_10k,
    log_schools_per_10k,
    enrollment_per_10k,

    # --- BEHAVIORAL CAPACITY (main IV) ---
    adm_capacity_score_mun,
    adm_capacity_score_est,

    # --- COMPOSITE COMPONENTS ---
    edu_share_mun,
    budget_execution_mun,
    debt_ratio_mun,
    fpm_dependence_mun,

    # --- STRUCTURAL FISCAL (Tilly) ---
    fiscal_autonomy_mun,
    transfer_dependence_mun,
    fiscal_autonomy_est,
    transfer_dependence_est,
    fpm_dependence_est,

    # --- MUNICIPAL CONTROLS ---
    pop_mun,
    log_pop_mun,
    log_gdp_pc_mun,
    log_area_mun,
    log_pop_density_mun,
    bioma,
    dist_capital_km,
    log_dist_capital,

    # --- STATE CONTROLS ---
    log_pop_est,
    log_gdp_pc_est,

    # --- ROBUSTNESS ---
    capag_numeric,
    capag_estado_numeric
  )

report_dims(master_mun_main, "master_mun_main")

# ============================================================
# FINAL DIAGNOSTICS
# ============================================================

report_table(master_mun_main, "has_school",  "Has school")
report_table(master_mun_main, "bioma",       "Biome")
report_table(master_mun_main, "urban_type",  "Urban type")
report_table(master_mun_main, "pop_type",    "Pop type")
report_table(master_mun_main, "regiao",      "Region")

report_indicators(master_mun_main, c(
  "mean_dist_school_km",
  "log_mean_dist_school",
  "pct_over_10km",
  "schools_per_10k",
  "log_schools_per_10k",
  "adm_capacity_score_mun",
  "fiscal_autonomy_mun",
  "log_pop_mun",
  "log_gdp_pc_mun"
))

# ============================================================
# SAVE
# ============================================================

save_processed(master_mun_full, PATH_OUT_FULL)
save_processed(master_mun_main, PATH_OUT_MAIN)

cat("\nScript 21 complete.\n")