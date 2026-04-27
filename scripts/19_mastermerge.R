# ==================================================
# 11_mastermerge.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Merge all cleaned datasets into two outputs:
#       - master_full: all variables for robustness
#       - master_main: trimmed for main model
# Input:  data/processed/cescola_media_clean.csv
#         data/processed/finbra_municipios.csv
#         data/processed/finbra_estados.csv
#         data/processed/capag_municipios.csv
#         data/processed/capag_estados.csv
#         data/processed/controls_municipios.csv
#         data/processed/controls_estados.csv
# Output: data/processed/master_full.csv
#         data/processed/master_main.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)

# ============================================================
# CONSTANTS
# ============================================================

PATH_ESCOLA    <- "data/processed/cescola_media_clean.csv"
PATH_FINBRA_MUN <- "data/processed/finbra_municipios.csv"
PATH_FINBRA_EST <- "data/processed/finbra_estados.csv"
PATH_CAPAG_MUN  <- "data/processed/capag_municipios.csv"
PATH_CAPAG_EST  <- "data/processed/capag_estados.csv"
PATH_CTRL_MUN   <- "data/processed/controls_municipios.csv"
PATH_CTRL_EST   <- "data/processed/controls_estados.csv"
PATH_OUT_FULL   <- "data/processed/master_full.csv"
PATH_OUT_MAIN   <- "data/processed/master_main.csv"

# ============================================================
# LOAD ALL PROCESSED FILES
# ============================================================

cat("Loading processed files...\n")

escola      <- load_br_csv(PATH_ESCOLA)
finbra_mun  <- load_br_csv(PATH_FINBRA_MUN)
finbra_est  <- load_br_csv(PATH_FINBRA_EST)
capag_mun   <- load_br_csv(PATH_CAPAG_MUN)
capag_est   <- load_br_csv(PATH_CAPAG_EST)
ctrl_mun    <- load_br_csv(PATH_CTRL_MUN)
ctrl_est    <- load_br_csv(PATH_CTRL_EST)

report_dims(escola,     "cescola_media_clean")
report_dims(finbra_mun, "finbra_municipios")
report_dims(finbra_est, "finbra_estados")
report_dims(capag_mun,  "capag_municipios")
report_dims(capag_est,  "capag_estados")
report_dims(ctrl_mun,   "controls_municipios")
report_dims(ctrl_est,   "controls_estados")

# ============================================================
# STANDARDIZE MERGE KEYS
# All CO_MUNICIPIO and SG_UF as character
# ============================================================

escola$CO_MUNICIPIO      <- as.character(escola$CO_MUNICIPIO)
finbra_mun$CO_MUNICIPIO  <- as.character(finbra_mun$CO_MUNICIPIO)
capag_mun$CO_MUNICIPIO   <- as.character(capag_mun$CO_MUNICIPIO)
ctrl_mun$CO_MUNICIPIO    <- as.character(ctrl_mun$CO_MUNICIPIO)

# ============================================================
# CHECK MERGE KEY OVERLAPS BEFORE JOINING
# ============================================================

cat("\n--- Merge key overlap checks ---\n")

n_escola_mun <- n_distinct(escola$CO_MUNICIPIO)
cat("Unique municipalities in escola:", n_escola_mun, "\n")

cat("Overlap escola <-> finbra_mun:",
    sum(unique(escola$CO_MUNICIPIO) %in%
        finbra_mun$CO_MUNICIPIO), "/", n_escola_mun, "\n")

cat("Overlap escola <-> capag_mun:",
    sum(unique(escola$CO_MUNICIPIO) %in%
        capag_mun$CO_MUNICIPIO), "/", n_escola_mun, "\n")

cat("Overlap escola <-> ctrl_mun:",
    sum(unique(escola$CO_MUNICIPIO) %in%
        ctrl_mun$CO_MUNICIPIO), "/", n_escola_mun, "\n")

cat("Overlap escola <-> finbra_est:",
    sum(unique(escola$SG_UF) %in%
        finbra_est$SG_UF), "/ 27\n")

cat("Overlap escola <-> ctrl_est:",
    sum(unique(escola$SG_UF) %in%
        ctrl_est$SG_UF), "/ 27\n")

# ============================================================
# BUILD MASTER_FULL
# School level base, joined with all municipal and state data
# One row per school — 30,175 rows
# ============================================================

cat("\n--- Building master_full ---\n")

master_full <- escola %>%
  mutate(log_mat_med = log(QT_MAT_MED + 1)) %>%

  # Municipal fiscal capacity
  left_join(
    finbra_mun %>%
      select(CO_MUNICIPIO,
             receita_total, receita_propria,
             transf_total, transf_uniao, fpm, transf_estados,
             desp_empenhadas, desp_liquidadas, desp_educacao,
             passivo_circulante, passivo_nao_circ, divida_lp,
             fiscal_autonomy_mun, transfer_dependence_mun,
             fpm_dependence_mun, edu_share_mun,
             budget_execution_mun, debt_ratio_mun,
             adm_capacity_score_mun),
    by = "CO_MUNICIPIO"
  ) %>%

  # Municipal CAPAG (robustness)
  left_join(
    capag_mun %>%
      select(CO_MUNICIPIO, capag, capag_numeric,
             dca_2024, capag_rebaixada),
    by = "CO_MUNICIPIO"
  ) %>%

  # Municipal controls
  left_join(
    ctrl_mun %>%
      select(CO_MUNICIPIO,
             pop_mun, gdp_pc_mun,
             log_pop_mun, log_gdp_pc_mun,
             log_area_mun, pop_density_mun,
             log_pop_density_mun, urban_share_mun,
             bioma, dist_capital_km, log_dist_capital),
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

  # State CAPAG (robustness)
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
             log_area_est, pop_density_est,
             log_pop_density_est),
    by = "SG_UF"
  )

check_merge(master_full, nrow(escola), "master_full")
report_dims(master_full, "master_full")

# ============================================================
# INSPECT MISSINGNESS IN MASTER_FULL
# ============================================================

cat("\n--- Key variable missingness in master_full ---\n")
report_missing(master_full, c(
  "CO_MUNICIPIO", "SG_UF",
  "infra_ok", "edu_ok", "edu_score",
  "adm_capacity_score_mun", "adm_capacity_score_est",
  "log_pop_mun", "log_gdp_pc_mun",
  "bioma", "dist_capital_km",
  "capag_numeric", "capag_estado_numeric"
))

# ============================================================
# BUILD MASTER_MAIN
# Trimmed to main model variables only
# Lighter file for day-to-day analysis
# ============================================================

cat("\n--- Building master_main ---\n")

master_main <- master_full %>%
  select(
    # --- SCHOOL IDENTIFIERS ---
    CO_ENTIDADE, NO_ENTIDADE,
    CO_MUNICIPIO, NO_MUNICIPIO,
    CO_UF, SG_UF,

    # --- SCHOOL CHARACTERISTICS ---
    TP_LOCALIZACAO,              # 1=urban, 2=rural
    TP_LOCALIZACAO_DIFERENCIADA, # indigenous/quilombola
    TP_DEPENDENCIA,              # federal/state/municipal/private
    QT_MAT_MED,                  # secondary enrollment
    log_mat_med,                 # log enrollment — right skewed

    # --- OUTCOME VARIABLES ---
    infra_ok,
    edu_ok,
    edu_score,

    # --- BEHAVIORAL CAPACITY COMPOSITE (main IV) ---
    # Built from: edu_share, budget_execution,
    #             debt_ratio, fpm_dependence
    adm_capacity_score_mun,
    adm_capacity_score_est,

    # --- COMPOSITE COMPONENTS (for disaggregated models) ---
    edu_share_mun,
    budget_execution_mun,
    debt_ratio_mun,
    fpm_dependence_mun,

    # --- STRUCTURAL FISCAL INDICATORS (separate controls) ---
    # These measure fiscal position, not behavior
    # Correlated with municipality size — use carefully
    fiscal_autonomy_mun,
    transfer_dependence_mun,
    fiscal_autonomy_est,
    transfer_dependence_est,
    fpm_dependence_est,

    # --- MUNICIPAL CONTROLS ---
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

report_dims(master_main, "master_main")

# ============================================================
# FINAL CHECKS ON MASTER_MAIN
# ============================================================

report_table(master_main, "TP_LOCALIZACAO",  "Localization")
report_table(master_main, "TP_DEPENDENCIA",  "Dependency type")
report_table(master_main, "bioma",           "Biome")
report_table(master_main, "infra_ok",        "infra_ok")
report_table(master_main, "edu_ok",          "edu_ok")

report_indicators(master_main, c(
  "adm_capacity_score_mun",
  "adm_capacity_score_est",
  "log_pop_mun",
  "log_gdp_pc_mun",
  "dist_capital_km"
))

# ============================================================
# SAVE
# ============================================================

save_processed(master_full, PATH_OUT_FULL)
save_processed(master_main, PATH_OUT_MAIN)

cat("\nScript 11 complete.\n")