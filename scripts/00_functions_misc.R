# ==================================================
# 00_functions_misc.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Create models that can be reused across 
#       multiple project scripts
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

library(tidyverse)
library(data.table)

# ============================================================
# STRING CLEANING
# ============================================================

# Remove accents. Example: "São Paulo" -> "Sao Paulo"
remove_accents <- function(x) {
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
}

# Lowercase + no accents + trimmed whitespace
clean_string <- function(x) {
  tolower(remove_accents(trimws(x)))
}

# Standardize column names to snake_case lowercase.
clean_colnames <- function(df) {
  names(df) <- names(df) %>%
    tolower() %>%
    gsub("\\.", "_", .) %>%
    gsub(" ",  "_", .) %>%
    gsub("[^a-z0-9_]", "", .)
  df
}

# Remove surrounding quotes from FINBRA exported strings
clean_conta <- function(x) {
  gsub('^"|"$', "", x)
}

# Fix Brazilian decimal separator (comma -> period)
fix_decimal <- function(x) {
  as.numeric(gsub(",", ".", x))
}

# ============================================================
# DATA LOADING
# ============================================================

# Load standard Brazilian government CSV
# Semicolon separated, Latin-1 encoding
load_br_csv <- function(path, ...) {
  fread(path, encoding = "Latin-1", sep = ";", ...)
}

# Load FINBRA files — all have 3 metadata header rows
load_finbra <- function(path) {
  df <- fread(path,
              encoding = "Latin-1",
              quote    = "",
              fill     = TRUE,
              sep      = ";",
              skip     = 3)
  df$Conta  <- clean_conta(df$Conta)
  df$Coluna <- clean_conta(df$Coluna)
  df$Valor  <- fix_decimal(df$Valor)
  df
}

# Save processed file standardized for all outputs)
save_processed <- function(df, path) {
  fwrite(df, path, sep = ";", bom = TRUE)
  cat("Saved", nrow(df), "rows |", ncol(df),
      "cols to", path, "\n")
}

# ============================================================
# DIAGNOSTICS
# ============================================================

# Print formatted row/col count
report_dims <- function(df, label = "") {
  if (nchar(label) > 0) cat(label, "— ")
  cat("Rows:", nrow(df), "| Cols:", ncol(df), "\n")
}

# Print formatted missing value report
report_missing <- function(df, vars) {
  cat("\nMissing values:\n")
  for (v in vars) {
    if (!v %in% names(df)) {
      cat(sprintf("  %-35s NOT FOUND\n", v))
    } else {
      n_miss <- sum(is.na(df[[v]]))
      pct    <- round(100 * n_miss / nrow(df), 1)
      cat(sprintf("  %-35s %d missing (%.1f%%)\n",
                  v, n_miss, pct))
    }
  }
}

# Print mean/sd/NA diagnostics for numeric variables
report_indicators <- function(df, vars) {
  cat("\nIndicator diagnostics:\n")
  for (v in vars) {
    if (!v %in% names(df)) {
      cat(sprintf("  %-30s NOT FOUND\n", v))
    } else {
      vals <- df[[v]]
      cat(sprintf("  %-30s mean=%8.3f | sd=%7.3f | NA=%d\n",
                  v,
                  mean(vals, na.rm = TRUE),
                  sd(vals,   na.rm = TRUE),
                  sum(is.na(vals))))
    }
  }
}

# Print frequency table with NA always shown
report_table <- function(df, var, label = NULL) {
  if (is.null(label)) label <- var
  cat("\n", label, " distribution:\n", sep = "")
  print(table(df[[var]], useNA = "always"))
}

# Check merge didn't change row count
check_merge <- function(df_result, n_expected, label = "Merge") {
  cat(sprintf("\n%s: %d rows (expected %d) — ",
              label, nrow(df_result), n_expected))
  if (nrow(df_result) != n_expected) {
    cat("WARNING: row count changed!\n")
    warning(sprintf("%s changed row count: %d -> %d",
                    label, n_expected, nrow(df_result)))
  } else {
    cat("OK\n")
  }
}

# ============================================================
# CAPAG HELPERS
# ============================================================

# Convert CAPAG letter grade to numeric 1-4
# A/A+ = 4, B/B+ = 3, C = 2, D = 1, else NA
capag_to_numeric <- function(x) {
  base <- stringr::str_extract(x, "^[A-D]")
  dplyr::case_when(
    base == "A" ~ 4,
    base == "B" ~ 3,
    base == "C" ~ 2,
    base == "D" ~ 1,
    TRUE        ~ NA_real_
  )
}

# ============================================================
# STATE CODE LOOKUP
# ============================================================

# IBGE numeric state code -> UF abbreviation
# Used to fix SIDRA state data which returns numeric codes
# Example: "35" -> "SP"
state_code_to_uf <- function(code_vec) {
  lookup <- c(
    "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR",
    "15" = "PA", "16" = "AP", "17" = "TO", "21" = "MA",
    "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
    "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA",
    "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
    "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
    "51" = "MT", "52" = "GO", "53" = "DF"
  )
  unname(lookup[as.character(code_vec)])
}

# ============================================================
# SPATIAL HELPERS
# ============================================================

# Compute distance in km between two projected sf objects
dist_km_sf <- function(from_sf, to_sf) {
  as.numeric(sf::st_distance(from_sf, to_sf)) / 1000
}

robust_se <- function(model, cluster_var) {
  coeftest(model, vcov = vcovCL(model, cluster = cluster_var))
}
# ============================================================
# Winsorize a numeric vector at given percentile thresholds
# Replaces extreme values with percentile bounds
# Example: winsorize(x, 0.01) caps at p1 and p99
# ============================================================

winsorize <- function(x, p = 0.01) {
  lo <- quantile(x, p,     na.rm = TRUE)
  hi <- quantile(x, 1 - p, na.rm = TRUE)
  pmax(pmin(x, hi), lo)
}

cat("00-functions.R loaded successfully\n")

# ============================================================
# MODEL CONSTANTS
# (shared across 31_models_main.R, 81_robustcapag.R, 83_robustness_spatial.R)
# ============================================================

# Dependent variable
DV_LOG_DIST     <- "log_mean_dist"       # log mean settlement-to-school distance
DV_LOG_DIST_MUN <- "log_mean_dist_mun"   # log mean distance to municipal schools only

# Core geographic and socioeconomic controls
CONTROLS_GEO <- c(
  "log_pop_mun",      # log municipal population
  "log_gdp_pc_mun",   # log GDP per capita
  "log_area_mun",     # log municipal area km²
  "urban_share_mun",  # share of urban population
  "log_dist_capital", # log distance to state capital
  "bioma"             # biome fixed effects
)

# State-level capacity controls: used when no state FE
# dropped automatically when state FE is included (collinear)
CONTROLS_STATE_CAP <- c(
  "adm_capacity_score_est", # state behavioral capacity composite
  "fiscal_autonomy_est"     # state fiscal autonomy ratio
)

# Disaggregated behavioral capacity indicators
VARS_COMPOSITE_COMPONENTS <- c(
  "edu_share_mun",          # education spending / total liquidated
  "budget_execution_mun",   # liquidated / committed spending
  "debt_ratio_mun",         # total liabilities / total revenue
  "fpm_dependence_mun"      # FPM transfers / total revenue
)

# ============================================================
# MODEL HELPERS (fixest-based)
# ============================================================

# Build feols formula with optional state FE
# Usage: make_model_formula("log_mean_dist", "adm_capacity_score_mun", state_fe = TRUE)
make_model_formula <- function(dv, ivs, state_fe = FALSE) {
  state_controls <- if (state_fe) NULL else CONTROLS_STATE_CAP
  rhs_vars <- c(ivs, CONTROLS_GEO, state_controls)
  rhs      <- paste(rhs_vars, collapse = " + ")
  fe_part  <- if (state_fe) " | SG_UF" else ""
  as.formula(paste(dv, "~", rhs, fe_part))
}

# Extract key coefficients from a feols model into a tidy tibble
# Usage: extract_model_coef(m3, "adm_capacity_score_mun", "M3 Composite (no FE)")
extract_model_coef <- function(model, vars, model_name) {
  ct         <- fixest::coeftable(model)
  vars_found <- intersect(vars, rownames(ct))
  if (length(vars_found) == 0) return(NULL)
  tibble::as_tibble(ct[vars_found, , drop = FALSE],
                    rownames = "Variable") %>%
    dplyr::rename(SE = `Std. Error`,
                  t  = `t value`,
                  p  = `Pr(>|t|)`) %>%
    dplyr::mutate(
      Model = model_name,
      N     = as.integer(sum(fixest::obs(model))),
      R2    = round(fixest::r2(model)["r2"], 3),
      dplyr::across(c(Estimate, SE, t, p), ~ round(., 4))
    )
}

#=================================================
cat("Script 00_functions_misc.R complete.\n")
