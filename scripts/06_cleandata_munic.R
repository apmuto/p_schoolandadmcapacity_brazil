# =============================================================================
# Script 06 – Clean & select MUNIC 2023 data
# Project: Municipal administrative capacity and secondary school accessibility
# =============================================================================
# MUNIC 2023 covers 5,570 municipalities across 9 thematic sheets.
# Only two sheets are theoretically relevant here:
#
#   • Recursos humanos  – staffing by employment type (permanent vs. precarious)
#   • Primeira Infância – institutional/planning capacity indicators
#
# No education-specific sheet exists in MUNIC 2023.
#
# Key variables selected
# ──────────────────────
# From Recursos Humanos:
#   mreh_estatutario_mun   : count of estatutários (permanent civil servants)
#   mreh_clt_mun           : count of CLT workers
#   mreh_comissionado_mun  : count of commissioned staff
#   mreh_temporario_mun    : count of temporary workers
#   mreh_terceirizado_mun  : count of outsourced workers
#   mreh_total_mun         : total municipal staff
#   mreh_pct_estavel_mun   : share of "stable" workers (estatutário + CLT)
#   mreh_secretaria_mun    : has a dedicated municipal secretariat (0/1)
#
# From Primeira Infância (institutional planning):
#   mpri_plano_mun         : has a municipal early-childhood plan (0/1)
#   mpri_conselho_mun      : has a first-childhood council (0/1)
#   mpri_comite_mun        : has an inter-sector committee (0/1)
#
# Output: data/processed/munic_clean.csv  (5,570 rows × ~15 cols)
# =============================================================================

# ── 0. Setup ──────────────────────────────────────────────────────────────────
library(tidyverse)
library(readxl)

source("scripts/00_functions_misc.R")   # load_br_csv, save_processed, report_dims, etc.

# Path to raw MUNIC 2023 file (adjust if stored elsewhere)
MUNIC_PATH <- "data/raw/Base_MUNIC_2023.xlsx"

# ── 1. Load Recursos Humanos ─────────────────────────────────────────────────
# Variables:
#   MREH0111 estatutário | MREH0112 CLT | MREH0113 comissionado
#   MREH0114 temporário  | MREH0115 terceirizado | MREH0116 total
#   MREH02   dedicated secretariat (Sim/Não/Não informou)

message("Loading MUNIC 2023 – Recursos humanos...")

rh_raw <- read_excel(MUNIC_PATH, sheet = "Recursos humanos")

rh <- rh_raw |>
  rename(
    cod_mun              = CodMun,
    uf                   = `Sigla UF`,
    mreh_estatutario_mun = MREH0111,
    mreh_clt_mun         = MREH0112,
    mreh_comissionado_mun= MREH0113,
    mreh_temporario_mun  = MREH0114,
    mreh_terceirizado_mun= MREH0115,
    mreh_total_mun       = MREH0116,
    mreh_secretaria_raw  = MREH02
  ) |>
  # Keep only the columns we need
  select(cod_mun, uf, starts_with("mreh_")) |>
  mutate(
    # Coerce numeric; "-" strings (used for N/A in MUNIC) become NA_real_
    across(mreh_estatutario_mun:mreh_total_mun,
           ~ suppressWarnings(as.numeric(if_else(grepl("^[0-9]+$", trimws(as.character(.x))), trimws(as.character(.x)), NA_character_)))),

    # Binary secretariat indicator: 1 = Sim, 0 = Não / Não informou
    mreh_secretaria_mun = if_else(mreh_secretaria_raw == "Sim", 1L, 0L),

    # Share of "stable" workers (estatutário + CLT) out of total
    # Reflects behavioural capacity: reliance on permanent vs. precarious staff
    mreh_pct_estavel_mun = if_else(
      mreh_total_mun > 0,
      (mreh_estatutario_mun + mreh_clt_mun) / mreh_total_mun,
      NA_real_
    )
  ) |>
  select(-mreh_secretaria_raw)

report_dims(rh, "rh")
report_missing(rh, vars = names(rh))

# ── 2. Load Primeira Infância ─────────────────────────────────────────────────
# MPRI01  : municipal early-childhood plan (Sim/Em elaboração/Não/etc.)
# MPRI02  : inter-sector first-childhood committee (Sim/Não)
# MPRI05  : first-childhood municipal council (Sim/Não)
#
# These tap the *planning and coordination* dimension of capacity, distinct from
# the fiscal/behavioural indicators used in adm_capacity_score_mun.

message("Loading MUNIC 2023 – Primeira Infância...")

pi_raw <- read_excel(MUNIC_PATH, sheet = "Primeira Infância")

pi <- pi_raw |>
  rename(
    cod_mun         = CodMun,
    mpri_plano_raw  = MPRI01,
    mpri_comite_raw = MPRI02,
    mpri_conselho_raw = MPRI05
  ) |>
  select(cod_mun, mpri_plano_raw, mpri_comite_raw, mpri_conselho_raw) |>
  mutate(
    # Binary: has a formalised plan (Sim) vs not/in progress
    mpri_plano_mun    = if_else(
      mpri_plano_raw == "Sim, e é regulamentado por instrumento legal" |
      mpri_plano_raw == "Sim, mas não é regulamentado por instrumento legal",
      1L, 0L
    ),
    # Binary: inter-sector committee exists
    mpri_comite_mun   = if_else(mpri_comite_raw == "Sim", 1L, 0L),
    # Binary: dedicated council exists
    mpri_conselho_mun = if_else(mpri_conselho_raw == "Sim", 1L, 0L)
  ) |>
  select(-ends_with("_raw"))

report_dims(pi, "pi")
report_missing(pi, vars = names(pi))

# ── 3. Merge RH + PI ─────────────────────────────────────────────────────────
message("Merging sheets...")

munic_clean <- left_join(rh, pi, by = "cod_mun")

# Sanity check: all 5,570 municipalities present, no row inflation
stopifnot("merge inflated rows" = nrow(munic_clean) == nrow(rh))
message("Merge OK – ", nrow(munic_clean), " rows | ", ncol(munic_clean), " cols")

report_dims(munic_clean, "munic_clean")

# ── 4. Optional: composite planning index ────────────────────────────────────
# Simple additive index of institutional planning effort (0–3 range).
# Not used in the main capacity composite (adm_capacity_score_mun), but useful
# as a robustness check or additional covariate.

munic_clean <- munic_clean |>
  mutate(
    munic_planning_index = mpri_plano_mun + mpri_comite_mun + mpri_conselho_mun
  )

# ── 5. Quick descriptive check ───────────────────────────────────────────────
message("\n── Summary of selected variables ──")

report_indicators(
  munic_clean,
  vars = c("mreh_total_mun", "mreh_pct_estavel_mun",
           "mreh_secretaria_mun", "mpri_plano_mun",
           "mpri_conselho_mun", "mpri_comite_mun",
           "munic_planning_index")
)

# ── 6. Save ───────────────────────────────────────────────────────────────────
save_processed(munic_clean, "data/processed/munic_clean.csv")
message("\nScript 06 complete. Output: data/processed/munic_clean.csv")
message("Rows: ", nrow(munic_clean), " | Cols: ", ncol(munic_clean))