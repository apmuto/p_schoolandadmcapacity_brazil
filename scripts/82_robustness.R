# ==================================================
# 82_robustness.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Robustness checks for main models.
#       Block 1: Alternative IV (CAPAG validation)
#       Block 2: Alternative explanations
#                (population density, own resources)
#       Block 3: Geographic FE robustness
#                (biome FE, region FE)
# Input:  data/processed/master_mun_main.csv
# Output: results/tab_robustness_capag.tex
#         results/tab_robustness_altexp.tex
#         results/tab_robustness_geoge.tex
#         results/coef_robustness_summary.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(fixest)
library(data.table)
PATH_TABLES <- "outputs/tables/"
# ============================================================
# LOAD DATA
# ============================================================

cat("Loading master dataset...\n")
d <- load_br_csv("data/processed/master_mun_main.csv") %>%
  mutate(
    CO_MUNICIPIO = as.character(CO_MUNICIPIO)
  ) %>%
  filter(has_school == TRUE | has_school == 1)

cat("Analytic sample:", nrow(d), "municipalities\n")
cat("CAPAG subsample (non-missing):",
    sum(!is.na(d$capag_numeric)), "\n")

# ============================================================
# SHARED FORMULA COMPONENTS
# ============================================================

# Base controls used throughout (matching main models)
CONTROLS <- "log_pop_mun + log_gdp_pc_mun + log_area_mun +
             urban_share_mun + log_dist_capital + bioma"

# Controls without bioma — used in Block 3 geographic FE models
# to avoid collinearity between bioma dummies and biome/region FE
CONTROLS_NOGEO <- "log_pop_mun + log_gdp_pc_mun + log_area_mun +
                   urban_share_mun + log_dist_capital"

# ============================================================
# HELPER: extract coefficient row for summary CSV
# ============================================================

extract_coef <- function(model, var, model_label) {
  ct <- coeftable(model)
  if (!var %in% rownames(ct)) {
    return(data.frame(Model = model_label, Variable = var,
                      Estimate = NA, SE = NA, t = NA, p = NA,
                      N = nobs(model), R2 = NA))
  }
  data.frame(
    Model    = model_label,
    Variable = var,
    Estimate = ct[var, "Estimate"],
    SE       = ct[var, "Std. Error"],
    t        = ct[var, "t value"],
    p        = ct[var, "Pr(>|t|)"],
    N        = nobs(model),
    R2       = r2(model, "r2")
  )
}

coef_rows <- list()

# ============================================================
# BLOCK 1 — ALTERNATIVE IV: CAPAG VALIDATION
# Replace composite with official CAPAG fiscal health rating
# capag_numeric: A=4, B=3, C=2, D=1
# Caveat: 33.9% missing — sample selectivity should be noted
# ============================================================

cat("\n--- Block 1: CAPAG validation ---\n")

d_capag <- d %>% filter(!is.na(capag_numeric))
cat("CAPAG sample:", nrow(d_capag), "municipalities\n")

# M_capag1: CAPAG no FE
m_capag1 <- feols(
  as.formula(paste("log_mean_dist ~ capag_numeric +", CONTROLS,
                   "+ adm_capacity_score_est + fiscal_autonomy_est")),
  data    = d_capag,
  cluster = ~SG_UF
)

# M_capag2: CAPAG with state FE
m_capag2 <- feols(
  as.formula(paste("log_mean_dist ~ capag_numeric +", CONTROLS,
                   "| SG_UF")),
  data    = d_capag,
  cluster = ~SG_UF
)

# M_capag3: CAPAG + composite together (convergent validity)
m_capag3 <- feols(
  as.formula(paste("log_mean_dist ~ capag_numeric +
                    adm_capacity_score_mun +", CONTROLS,
                   "+ adm_capacity_score_est + fiscal_autonomy_est")),
  data    = d_capag,
  cluster = ~SG_UF
)

# M_capag4: CAPAG + composite + state FE
m_capag4 <- feols(
  as.formula(paste("log_mean_dist ~ capag_numeric +
                    adm_capacity_score_mun +", CONTROLS,
                   "| SG_UF")),
  data    = d_capag,
  cluster = ~SG_UF
)

coef_rows <- c(coef_rows, list(
  extract_coef(m_capag1, "capag_numeric",          "M_capag1 CAPAG (no FE)"),
  extract_coef(m_capag2, "capag_numeric",          "M_capag2 CAPAG (state FE)"),
  extract_coef(m_capag3, "capag_numeric",          "M_capag3 CAPAG+Composite (no FE)"),
  extract_coef(m_capag3, "adm_capacity_score_mun", "M_capag3 CAPAG+Composite (no FE)"),
  extract_coef(m_capag4, "capag_numeric",          "M_capag4 CAPAG+Composite (state FE)"),
  extract_coef(m_capag4, "adm_capacity_score_mun", "M_capag4 CAPAG+Composite (state FE)")
))

cat("CAPAG models estimated\n")
cat("  M_capag1 capag coef:",
    round(coeftable(m_capag1)["capag_numeric", "Estimate"], 4),
    "p =", round(coeftable(m_capag1)["capag_numeric", "Pr(>|t|)"], 4), "\n")
cat("  M_capag2 capag coef:",
    round(coeftable(m_capag2)["capag_numeric", "Estimate"], 4),
    "p =", round(coeftable(m_capag2)["capag_numeric", "Pr(>|t|)"], 4), "\n")

# LaTeX table — Block 1
tab_capag <- etable(
  m_capag1, m_capag2, m_capag3, m_capag4,
  keep       = c("capag_numeric", "adm_capacity_score_mun"),
  tex        = TRUE,
  title      = "Robustness: CAPAG as alternative IV",
  notes      = paste(
    "CAPAG subsample only (n =", nrow(d_capag), ").",
    "CAPAG numeric: A=4, B=3, C=2, D=1.",
    "Cols 3-4 include composite score alongside CAPAG for convergent validity.",
    "SE clustered by state."
  ),
  style.tex  = style.tex("base")
)
writeLines(tab_capag, "results/tab_robustness_capag.tex")
cat("Saved results/tab_robustness_capag.tex\n")

# ============================================================
# BLOCK 2 — ALTERNATIVE EXPLANATIONS
# 2a: Population density (more schools because more people)
# 2b: Own resources per capita (more schools because richer)
# ============================================================

cat("\n--- Block 2: Alternative explanations ---\n")

# --- 2a: Population density ---
# Main composite spec + log_pop_density_mun replacing/alongside log_pop

# M_dens1: density replaces log_pop (no FE)
m_dens1 <- feols(
  as.formula(paste(
    "log_mean_dist ~ adm_capacity_score_mun +
     log_pop_density_mun + log_gdp_pc_mun + log_area_mun +
     urban_share_mun + log_dist_capital + bioma +
     adm_capacity_score_est + fiscal_autonomy_est"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_dens2: density replaces log_pop (state FE)
m_dens2 <- feols(
  as.formula(paste(
    "log_mean_dist ~ adm_capacity_score_mun +
     log_pop_density_mun + log_gdp_pc_mun + log_area_mun +
     urban_share_mun + log_dist_capital | SG_UF"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_dens3: both pop and density (no FE) — kitchen sink
m_dens3 <- feols(
  as.formula(paste(
    "log_mean_dist ~ adm_capacity_score_mun +
     log_pop_mun + log_pop_density_mun + log_gdp_pc_mun +
     log_area_mun + urban_share_mun + log_dist_capital + bioma +
     adm_capacity_score_est + fiscal_autonomy_est"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_dens4: both pop and density (state FE)
m_dens4 <- feols(
  as.formula(paste(
    "log_mean_dist ~ adm_capacity_score_mun +
     log_pop_mun + log_pop_density_mun + log_gdp_pc_mun +
     log_area_mun + urban_share_mun + log_dist_capital | SG_UF"
  )),
  data    = d,
  cluster = ~SG_UF
)

coef_rows <- c(coef_rows, list(
  extract_coef(m_dens1, "adm_capacity_score_mun", "M_dens1 Density repl. pop (no FE)"),
  extract_coef(m_dens2, "adm_capacity_score_mun", "M_dens2 Density repl. pop (state FE)"),
  extract_coef(m_dens3, "adm_capacity_score_mun", "M_dens3 Pop + density (no FE)"),
  extract_coef(m_dens4, "adm_capacity_score_mun", "M_dens4 Pop + density (state FE)")
))

cat("Density models estimated\n")

# --- 2b: Own resources per capita vs fiscal autonomy ---
# Tests whether it's absolute wealth that drives school provision,
# not administrative capacity per se

# M_own1: own resources per capita (no FE)
m_own1 <- feols(
  as.formula(paste(
    "log_mean_dist ~ fiscal_autonomy_mun +", CONTROLS,
    "+ adm_capacity_score_est + fiscal_autonomy_est"
  )),
  data    = d %>% filter(!is.na(fiscal_autonomy_mun)),
  cluster = ~SG_UF
)

# M_own2: own resources per capita (state FE)
m_own2 <- feols(
  as.formula(paste(
    "log_mean_dist ~ fiscal_autonomy_mun +", CONTROLS, "| SG_UF"
  )),
  data    = d %>% filter(!is.na(fiscal_autonomy_mun)),
  cluster = ~SG_UF
)

# M_own3: fiscal autonomy (ratio) — for direct comparison
m_own3 <- feols(
  as.formula(paste(
    "log_mean_dist ~ fiscal_autonomy_mun +", CONTROLS,
    "+ adm_capacity_score_est + fiscal_autonomy_est"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_own4: fiscal autonomy (state FE)
m_own4 <- feols(
  as.formula(paste(
    "log_mean_dist ~ fiscal_autonomy_mun +", CONTROLS, "| SG_UF"
  )),
  data    = d,
  cluster = ~SG_UF
)

coef_rows <- c(coef_rows, list(
  extract_coef(m_own1, "fiscal_autonomy_mun",   "M_own1 Own resources pc (no FE)"),
  extract_coef(m_own2, "fiscal_autonomy_mun",   "M_own2 Own resources pc (state FE)"),
  extract_coef(m_own3, "fiscal_autonomy_mun", "M_own3 Fiscal autonomy (no FE)"),
  extract_coef(m_own4, "fiscal_autonomy_mun", "M_own4 Fiscal autonomy (state FE)")
))

cat("Own resources models estimated\n")

# LaTeX table — Block 2
tab_altexp <- etable(
  m_dens1, m_dens2, m_own1, m_own2, m_own3, m_own4,
  keep      = c("adm_capacity_score_mun", "log_pop_density_mun",
                "fiscal_autonomy_mun", "fiscal_autonomy_mun"),
  tex       = TRUE,
  title     = "Robustness: Alternative explanations",
  notes     = paste(
    "Cols 1-2: population density replaces log population.",
    "Cols 3-4: log own-source revenue per capita as IV.",
    "Cols 5-6: fiscal autonomy (own/total revenue) as IV.",
    "SE clustered by state."
  ),
  style.tex = style.tex("base")
)
writeLines(tab_altexp, "results/tab_robustness_altexp.tex")
cat("Saved results/tab_robustness_altexp.tex\n")

# ============================================================
# BLOCK 3 — GEOGRAPHIC FE ROBUSTNESS
# Tests whether biome or region FE absorbs the capacity effect
# Main predictors: edu_share_mun + fpm_dependence_mun
# (strongest disaggregated predictors from main models)
# Also run composite for comparability
# ============================================================

cat("\n--- Block 3: Geographic FE robustness ---\n")

# --- 3a: Biome FE ---

# M_biome1: disaggregated + biome FE
m_biome1 <- feols(
  as.formula(paste(
    "log_mean_dist ~ edu_share_mun + fpm_dependence_mun +",
    CONTROLS_NOGEO, "| bioma"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_biome2: disaggregated + biome FE + state FE
m_biome2 <- feols(
  as.formula(paste(
    "log_mean_dist ~ edu_share_mun + fpm_dependence_mun +",
    CONTROLS_NOGEO, "| bioma + SG_UF"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_biome3: composite + biome FE
m_biome3 <- feols(
  as.formula(paste(
    "log_mean_dist ~ adm_capacity_score_mun +",
    CONTROLS_NOGEO, "| bioma"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_biome4: composite + biome FE + state FE
m_biome4 <- feols(
  as.formula(paste(
    "log_mean_dist ~ adm_capacity_score_mun +",
    CONTROLS_NOGEO, "| bioma + SG_UF"
  )),
  data    = d,
  cluster = ~SG_UF
)

coef_rows <- c(coef_rows, list(
  extract_coef(m_biome1, "edu_share_mun",          "M_biome1 Disaggr. + biome FE"),
  extract_coef(m_biome1, "fpm_dependence_mun",     "M_biome1 Disaggr. + biome FE"),
  extract_coef(m_biome2, "edu_share_mun",          "M_biome2 Disaggr. + biome+state FE"),
  extract_coef(m_biome2, "fpm_dependence_mun",     "M_biome2 Disaggr. + biome+state FE"),
  extract_coef(m_biome3, "adm_capacity_score_mun", "M_biome3 Composite + biome FE"),
  extract_coef(m_biome4, "adm_capacity_score_mun", "M_biome4 Composite + biome+state FE")
))

cat("Biome FE models estimated\n")

# --- 3b: Region FE ---

# M_region1: disaggregated + region FE
m_region1 <- feols(
  as.formula(paste(
    "log_mean_dist ~ edu_share_mun + fpm_dependence_mun +",
    CONTROLS_NOGEO, "| regiao"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_region2: disaggregated + region FE + state FE
m_region2 <- feols(
  as.formula(paste(
    "log_mean_dist ~ edu_share_mun + fpm_dependence_mun +",
    CONTROLS_NOGEO, "| regiao + SG_UF"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_region3: composite + region FE
m_region3 <- feols(
  as.formula(paste(
    "log_mean_dist ~ adm_capacity_score_mun +",
    CONTROLS_NOGEO, "| regiao"
  )),
  data    = d,
  cluster = ~SG_UF
)

# M_region4: composite + region FE + state FE
m_region4 <- feols(
  as.formula(paste(
    "log_mean_dist ~ adm_capacity_score_mun +",
    CONTROLS_NOGEO, "| regiao + SG_UF"
  )),
  data    = d,
  cluster = ~SG_UF
)

coef_rows <- c(coef_rows, list(
  extract_coef(m_region1, "edu_share_mun",          "M_region1 Disaggr. + region FE"),
  extract_coef(m_region1, "fpm_dependence_mun",     "M_region1 Disaggr. + region FE"),
  extract_coef(m_region2, "edu_share_mun",          "M_region2 Disaggr. + region+state FE"),
  extract_coef(m_region2, "fpm_dependence_mun",     "M_region2 Disaggr. + region+state FE"),
  extract_coef(m_region3, "adm_capacity_score_mun", "M_region3 Composite + region FE"),
  extract_coef(m_region4, "adm_capacity_score_mun", "M_region4 Composite + region+state FE")
))

cat("Region FE models estimated\n")

# LaTeX table — Block 3
tab_geo <- etable(
  m_biome1, m_biome2, m_biome3, m_biome4,
  m_region1, m_region2, m_region3, m_region4,
  keep      = c("edu_share_mun", "fpm_dependence_mun",
                "adm_capacity_score_mun"),
  tex       = TRUE,
  title     = "Robustness: Geographic fixed effects",
  notes     = paste(
    "Cols 1-4: biome fixed effects.",
    "Cols 5-8: region fixed effects.",
    "Odd columns: disaggregated IV (edu share + FPM dependence).",
    "Even columns add state FE on top of geographic FE.",
    "SE clustered by state."
  ),
  style.tex = style.tex("base")
)
writeLines(tab_geo, "results/tab_robustness_geo.tex")
cat("Saved results/tab_robustness_geo.tex\n")
# ============================================================
# BLOCK 4 — FPM × DISTANCE INTERACTION
# H: Federal transfer dependence matters more far from capital
# (O'Donnell: state reach weakens with distance)
# ============================================================

cat("\n--- Block 4: FPM x distance interaction ---\n")

m_interact <- feols(
  log_mean_dist ~
    fpm_dependence_mun * log_dist_capital +
    adm_capacity_score_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + bioma +
    fiscal_autonomy_est,
  data    = d,
  cluster = ~SG_UF
)

cat("Interaction model:\n")
print(coeftable(m_interact)[
  c("fpm_dependence_mun",
    "log_dist_capital",
    "fpm_dependence_mun:log_dist_capital"), ])
cat("N:", nobs(m_interact), 
    "| R2:", round(r2(m_interact)["r2"], 3), "\n")

# Marginal effects at key distances
cat("\nMarginal effect of FPM at different distances:\n")
b_fpm  <- coeftable(m_interact)["fpm_dependence_mun", "Estimate"]
b_int  <- coeftable(m_interact)["fpm_dependence_mun:log_dist_capital", "Estimate"]
for (km in c(50, 100, 200, 400, 800)) {
  me <- b_fpm + b_int * log(km)
  cat(sprintf("  At %4dkm: %.3f\n", km, me))
}
coef_rows <- c(coef_rows, list(
  extract_coef(m_interact, "fpm_dependence_mun",
               "M_interact FPM x distance"),
  extract_coef(m_interact, "fpm_dependence_mun:log_dist_capital",
               "M_interact FPM x distance")
))

cat("Saved Block 4 coefficients\n")


# ============================================================
# SAVE COEFFICIENT SUMMARY
# ============================================================

cat("\n--- Saving coefficient summary ---\n")

coef_summary <- bind_rows(coef_rows) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

fwrite(coef_summary, "results/coef_robustness_summary.csv", sep = ";")
cat("Saved results/coef_robustness_summary.csv\n")

# ============================================================
# PRINT DIGEST
# ============================================================

cat("\n============================================================\n")
cat("ROBUSTNESS SUMMARY\n")
cat("============================================================\n")

cat("\n--- Block 1: CAPAG validation ---\n")
print(coef_summary %>%
        filter(grepl("capag", Model, ignore.case = TRUE)) %>%
        select(Model, Variable, Estimate, SE, p, N))

cat("\n--- Block 2: Alternative explanations ---\n")
print(coef_summary %>%
        filter(grepl("dens|own", Model, ignore.case = TRUE)) %>%
        select(Model, Variable, Estimate, SE, p, N))

cat("\n--- Block 3: Geographic FE ---\n")
print(coef_summary %>%
        filter(grepl("biome|region", Model, ignore.case = TRUE)) %>%
        select(Model, Variable, Estimate, SE, p, N))

cat("\nScript 82 complete.\n")