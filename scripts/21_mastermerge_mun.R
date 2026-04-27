# ==================================================
# 21_mastermerge_mun.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Build municipality-level master dataset
#       combining all cleaned data sources.
#       Unit of analysis: municipality (5,571 rows)
#       Main outcome: dist_nearest_school_km,
#                     schools_per_10k
#       Main IV: fiscal capacity indicators
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

PATH_ACCESS    <- "data/processed/accessibility.csv"
PATH_COVERAGE  <- "data/processed/coverage.csv"
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

acc <- load_br_csv(PATH_ACCESS) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO)) %>%
  select(-n_schools, -has_school)  # these come from coverage
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
# CHECK MERGE KEY OVERLAPS
# ============================================================

cat("\n--- Merge key overlap checks ---\n")
n_mun <- n_distinct(acc$CO_MUNICIPIO)
cat("Base municipalities (accessibility):", n_mun, "\n")

cat("Overlap acc <-> finbra_mun:",
    sum(unique(acc$CO_MUNICIPIO) %in%
        finbra_mun$CO_MUNICIPIO), "/", n_mun, "\n")
cat("Overlap acc <-> capag_mun:",
    sum(unique(acc$CO_MUNICIPIO) %in%
        capag_mun$CO_MUNICIPIO), "/", n_mun, "\n")
cat("Overlap acc <-> ctrl_mun:",
    sum(unique(acc$CO_MUNICIPIO) %in%
        ctrl_mun$CO_MUNICIPIO), "/", n_mun, "\n")
cat("Overlap acc <-> finbra_est:",
    sum(unique(acc$SG_UF) %in%
        finbra_est$SG_UF), "/ 27\n")
cat("Overlap acc <-> ctrl_est:",
    sum(unique(acc$SG_UF) %in%
        ctrl_est$SG_UF), "/ 27\n")

# ============================================================
# BUILD MASTER_MUN_FULL
# Base: accessibility (5,570 municipalities)
# ============================================================

cat("\n--- Building master_mun_full ---\n")

master_mun_full <- acc %>%

  # Coverage indicators
  left_join(
    cov %>%
      select(CO_MUNICIPIO,
             n_schools, n_schools_urban, n_schools_rural,
             n_schools_mun, n_schools_state,
             n_schools_fed, n_schools_priv,
             total_enrollment,
             schools_per_10k, enrollment_per_10k,
             log_schools_per_10k, log_enrollment_per_10k,
             coverage_cat),
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

  # Additional municipal controls not in accessibility
  left_join(
    ctrl_mun %>%
      select(CO_MUNICIPIO,
             area_km2_mun, pop_density_mun,
             log_area_mun, log_pop_density_mun),
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
  )

master_mun_full <- master_mun_full %>%
  mutate(has_school = n_schools > 0)

check_merge(master_mun_full, nrow(acc), "master_mun_full")
report_dims(master_mun_full, "master_mun_full")

# ============================================================
# MISSINGNESS CHECK
# ============================================================

cat("\n--- Key variable missingness ---\n")
report_missing(master_mun_full, c(
  "CO_MUNICIPIO", "SG_UF",
  "dist_nearest_school_km", "schools_per_10k",
  "adm_capacity_score_mun", "adm_capacity_score_est",
  "fiscal_autonomy_mun",
  "log_pop_mun", "log_gdp_pc_mun",
  "bioma", "dist_capital_km",
  "capag_numeric", "capag_estado_numeric"
))

# ============================================================
# BUILD MASTER_MUN_MAIN
# Trimmed for main model — accessibility outcomes + IVs
# ============================================================

cat("\n--- Building master_mun_main ---\n")

master_mun_main <- master_mun_full %>%
  select(
    # --- IDENTIFIERS ---
    CO_MUNICIPIO, SG_UF,

    # --- MAIN OUTCOME VARIABLES ---
    has_school,
    dist_nearest_school_km,  # distance to nearest school
    log_dist_school,         # log version
    schools_per_10k,         # coverage density
    log_schools_per_10k,
    enrollment_per_10k,      # enrollment coverage
    n_schools,               # raw count

    # --- BEHAVIORAL CAPACITY (main IV) ---
    adm_capacity_score_mun,
    adm_capacity_score_est,

    # --- COMPOSITE COMPONENTS ---
    edu_share_mun,
    budget_execution_mun,
    debt_ratio_mun,
    fpm_dependence_mun,

    # --- STRUCTURAL FISCAL (Tilly indicator) ---
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
    urban_share_mun,
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
# FINAL DIAGNOSTICS ON MAIN DATASET
# ============================================================

report_table(master_mun_main, "has_school", "Has school")
report_table(master_mun_main, "bioma",      "Biome")

report_indicators(master_mun_main, c(
  "dist_nearest_school_km",
  "schools_per_10k",
  "adm_capacity_score_mun",
  "adm_capacity_score_est",
  "fiscal_autonomy_mun",
  "log_pop_mun",
  "log_gdp_pc_mun",
  "dist_capital_km"
))

# ============================================================
# SAVE
# ============================================================

save_processed(master_mun_full, PATH_OUT_FULL)
save_processed(master_mun_main, PATH_OUT_MAIN)

cat("\nScript 21 complete.\n")