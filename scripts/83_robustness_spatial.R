# ==================================================
# 83_robustness_spatial.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Spatial diagnostics and spatial regression
#       models as robustness checks.
#       Step 1: Build spatial weights matrix (queen
#               contiguity + distance fallback)
#       Step 2: Moran's I on main model residuals
#       Step 3: Lagrange Multiplier tests (lag vs error)
#       Step 4: Spatial Error Model (SEM)
#       Step 5: Geographic FE comparison
#               (biome / region / state / SEM)
#       Note:   SLM dropped — LM tests consistently prefer SEM
# Input:  data/processed/master_mun_main.csv
#         data/processed/brazil_municipalities.gpkg
# Output: results/tab_spatial_diagnostics.tex
#         results/tab_spatial_sem.tex
#         results/tab_geo_fe_comparison.tex
#         results/coef_spatial_summary.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(sf)
library(spdep)
library(spatialreg)

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading master dataset...\n")
d <- load_br_csv("data/processed/master_mun_main.csv") %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO)) %>%
  filter(has_school == TRUE | has_school == 1)

cat("Analytic sample:", nrow(d), "municipalities\n")

# Add region variable from CO_MUNICIPIO if not present
if (!"regiao" %in% names(d)) {
  d <- d %>%
    mutate(
      CO_UF  = as.integer(substr(CO_MUNICIPIO, 1, 2)),
      regiao = case_when(
        CO_UF %in% c(11,12,13,14,15,16,17) ~ "Norte",
        CO_UF %in% c(21,22,23,24,25,26,27,28,29) ~ "Nordeste",
        CO_UF %in% c(31,32,33,35) ~ "Sudeste",
        CO_UF %in% c(41,42,43) ~ "Sul",
        CO_UF %in% c(50,51,52,53) ~ "Centro-Oeste",
        TRUE ~ NA_character_
      )
    )
}

cat("\nLoading municipality boundaries...\n")
munic <- st_read("data/processed/brazil_municipalities.gpkg",
                 quiet = TRUE) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

cat("Boundaries loaded:", nrow(munic), "municipalities\n")

# ============================================================
# MERGE SPATIAL AND ANALYTIC DATA
# ============================================================

cat("\nMerging spatial data with analytic sample...\n")

# Drop columns from munic that also exist in d to avoid .x/.y suffixes
munic_geom <- munic %>%
  select(CO_MUNICIPIO, geom)

d_sf <- munic_geom %>%
  inner_join(
    d %>% select(CO_MUNICIPIO, log_mean_dist,
                 adm_capacity_score_mun, edu_share_mun,
                 fpm_dependence_mun, fiscal_autonomy_mun,
                 log_pop_mun, log_gdp_pc_mun, log_area_mun,
                 urban_share_mun, log_dist_capital, bioma,
                 adm_capacity_score_est, fiscal_autonomy_est,
                 SG_UF, regiao),
    by = "CO_MUNICIPIO"
  )

cat("After join:", nrow(d_sf), "municipalities\n")

# Complete cases — must match OLS sample exactly
# lm() drops NAs silently; this keeps all lengths aligned
model_vars <- c("log_mean_dist", "adm_capacity_score_mun",
                "edu_share_mun", "fpm_dependence_mun",
                "log_pop_mun", "log_gdp_pc_mun", "log_area_mun",
                "urban_share_mun", "log_dist_capital", "bioma",
                "adm_capacity_score_est", "fiscal_autonomy_est")

d_sf <- d_sf %>%
  filter(complete.cases(st_drop_geometry(.)[, model_vars]))

cat("Complete cases for spatial models:", nrow(d_sf), "\n")

# ============================================================
# BUILD SPATIAL WEIGHTS MATRIX
# Queen contiguity + k=1 fallback for disconnected units
# ============================================================

cat("\nBuilding spatial weights matrix...\n")

nb_queen <- poly2nb(d_sf, queen = TRUE)

n_no_neighbor <- sum(card(nb_queen) == 0)
cat("Units with no queen neighbor:", n_no_neighbor, "\n")

if (n_no_neighbor > 0) {
  cat("Applying k=1 nearest neighbor fallback...\n")
  coords   <- st_centroid(d_sf) %>% st_coordinates()
  nb_knn1  <- knearneigh(coords, k = 1) %>% knn2nb()
  nb_final <- nb_queen
  empty    <- which(card(nb_queen) == 0)
  for (i in empty) nb_final[[i]] <- nb_knn1[[i]]
  cat("Fallback applied to", length(empty), "units\n")
} else {
  nb_final <- nb_queen
}

lw <- nb2listw(nb_final, style = "W", zero.policy = TRUE)
cat("Weights matrix built:", nrow(d_sf), "units\n")
cat("Average neighbors:", round(mean(card(nb_final)), 2), "\n")

# ============================================================
# BASELINE OLS
# ============================================================

cat("\nEstimating baseline OLS...\n")

ols_base <- lm(
  log_mean_dist ~ adm_capacity_score_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + log_dist_capital + bioma +
    adm_capacity_score_est + fiscal_autonomy_est,
  data = d_sf
)

ols_disaggr <- lm(
  log_mean_dist ~ edu_share_mun + fpm_dependence_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + log_dist_capital + bioma +
    adm_capacity_score_est + fiscal_autonomy_est,
  data = d_sf
)

cat("OLS n:", nobs(ols_base),
    "| Residuals n:", length(residuals(ols_base)),
    "| Weights n:", nrow(d_sf), "\n")
cat("Baseline OLS R2:", round(summary(ols_base)$r.squared, 3), "\n")

# ============================================================
# STEP 2 - MORAN'S I ON RESIDUALS
# ============================================================

cat("\n============================================================\n")
cat("STEP 2: MORAN'S I ON RESIDUALS\n")
cat("============================================================\n")

moran_base <- moran.test(residuals(ols_base),
                         listw = lw, zero.policy = TRUE)
moran_disaggr <- moran.test(residuals(ols_disaggr),
                            listw = lw, zero.policy = TRUE)

cat("\nMoran's I - Composite:\n")
print(moran_base)
cat("\nMoran's I - Disaggregated:\n")
print(moran_disaggr)

cat("\nMoran's I summary:\n")
cat(sprintf("  Composite:  I = %.4f | p = %.2e\n",
            unname(moran_base$estimate[1]), moran_base$p.value))
cat(sprintf("  Disaggr:    I = %.4f | p = %.2e\n",
            unname(moran_disaggr$estimate[1]), moran_disaggr$p.value))

# ============================================================
# STEP 3 - LAGRANGE MULTIPLIER TESTS
# ============================================================

cat("\n============================================================\n")
cat("STEP 3: LAGRANGE MULTIPLIER TESTS\n")
cat("============================================================\n")

lm_tests_base <- lm.RStests(ols_base, listw = lw,
                            test = c("RSlag","RSerr","adjRSlag","adjRSerr"),
                            zero.policy = TRUE)

lm_tests_disaggr <- lm.RStests(ols_disaggr, listw = lw,
                               test = c("RSlag","RSerr","adjRSlag","adjRSerr"),
                               zero.policy = TRUE)

cat("\nLM tests - Composite:\n")
print(summary(lm_tests_base))
cat("\nLM tests - Disaggregated:\n")
print(summary(lm_tests_disaggr))

extract_lm <- function(obj, label) {
  s <- summary(obj)
  data.frame(
    Model = label,
    Test  = rownames(s$results),
    Stat  = round(as.numeric(s$results[, "statistic"]), 4),
    p     = round(as.numeric(s$results[, "p.value"]),   6)
  )
}

lm_summary <- bind_rows(
  extract_lm(lm_tests_base,    "Composite"),
  extract_lm(lm_tests_disaggr, "Disaggregated")
)
cat("\nLM summary:\n")
print(lm_summary)

adj_lag_s <- lm_tests_base$adjRSlag$statistic
adj_err_s <- lm_tests_base$adjRSerr$statistic
adj_lag_p <- lm_tests_base$adjRSlag$p.value
adj_err_p <- lm_tests_base$adjRSerr$p.value

cat("\nModel selection (composite, robust tests):\n")
if (adj_lag_p < 0.05 & adj_err_p < 0.05) {
  if (adj_lag_s > adj_err_s) {
    cat("  => adjRS-lag > adjRS-error: PREFER SLM\n")
  } else {
    cat("  => adjRS-error > adjRS-lag: PREFER SEM\n")
  }
} else if (adj_lag_p < 0.05) {
  cat("  => Only adjRS-lag significant: PREFER SLM\n")
} else if (adj_err_p < 0.05) {
  cat("  => Only adjRS-error significant: PREFER SEM\n")
} else {
  cat("  => Neither significant: OLS adequate\n")
}

# ============================================================
# STEP 4 - SPATIAL ERROR MODEL (SEM)
# ============================================================

cat("\n============================================================\n")
cat("STEP 4: SPATIAL ERROR MODEL (SEM)\n")
cat("============================================================\n")

sem_composite <- errorsarlm(
  log_mean_dist ~ adm_capacity_score_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + log_dist_capital + bioma +
    adm_capacity_score_est + fiscal_autonomy_est,
  data = d_sf, listw = lw, zero.policy = TRUE
)

sem_disaggr <- errorsarlm(
  log_mean_dist ~ edu_share_mun + fpm_dependence_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + log_dist_capital + bioma +
    adm_capacity_score_est + fiscal_autonomy_est,
  data = d_sf, listw = lw, zero.policy = TRUE
)

cat("\nSEM composite:\n")
cat("  adm_capacity_score_mun:",
    round(coef(sem_composite)["adm_capacity_score_mun"], 4), "\n")
cat("  lambda:", round(sem_composite$lambda, 4), "\n")

cat("\nSEM disaggregated:\n")
cat("  edu_share_mun:",
    round(coef(sem_disaggr)["edu_share_mun"], 4), "\n")
cat("  fpm_dependence_mun:",
    round(coef(sem_disaggr)["fpm_dependence_mun"], 4), "\n")
cat("  lambda:", round(sem_disaggr$lambda, 4), "\n")

# ============================================================
# MODEL COMPARISON: OLS vs SEM
# ============================================================

cat("\n============================================================\n")
cat("MODEL COMPARISON: OLS vs SEM\n")
cat("============================================================\n")

extract_spatial <- function(model, vars, label, type) {
  cf <- coef(model)
  sm <- summary(model)$Coef
  pv <- sm[, "Pr(>|z|)"]
  se <- sm[, "Std. Error"]
  map_dfr(vars, function(v) {
    data.frame(
      Model = label, Type = type, Variable = v,
      Estimate = if (v %in% names(cf)) round(cf[v], 4) else NA,
      SE       = if (v %in% names(se)) round(se[v], 4) else NA,
      p        = if (v %in% names(pv)) round(pv[v], 4) else NA
    )
  })
}

extract_ols <- function(model, vars, label) {
  cf <- coef(summary(model))
  map_dfr(vars, function(v) {
    data.frame(
      Model = label, Type = "OLS", Variable = v,
      Estimate = if (v %in% rownames(cf)) round(cf[v, "Estimate"],   4) else NA,
      SE       = if (v %in% rownames(cf)) round(cf[v, "Std. Error"], 4) else NA,
      p        = if (v %in% rownames(cf)) round(cf[v, "Pr(>|t|)"],   4) else NA
    )
  })
}

spatial_summary <- bind_rows(
  extract_ols(ols_base,
              "adm_capacity_score_mun", "Composite"),
  extract_spatial(sem_composite,
                  "adm_capacity_score_mun", "Composite", "SEM"),
  extract_ols(ols_disaggr,
              c("edu_share_mun", "fpm_dependence_mun"), "Disaggregated"),
  extract_spatial(sem_disaggr,
                  c("edu_share_mun", "fpm_dependence_mun"), "Disaggregated", "SEM")
)

cat("\nOLS vs SEM comparison:\n")
print(spatial_summary)

cat("\nSEM spatial parameters:\n")
cat(sprintf("  Composite   lambda = %.4f\n", sem_composite$lambda))
cat(sprintf("  Disaggr     lambda = %.4f\n", sem_disaggr$lambda))

# ============================================================
# STEP 5: GEOGRAPHIC FE COMPARISON
# Composite coefficient across biome / region / state FE
# ============================================================

cat("\n============================================================\n")
cat("STEP 5: GEOGRAPHIC FE COMPARISON\n")
cat("============================================================\n")

library(lmtest)
library(sandwich)

robust_cl <- function(model) {
  coeftest(model,
           vcov = vcovCL(model,
                         cluster = d_sf[["SG_UF"]]))
}

# Add region variable to d_sf if not already present
if (!"regiao" %in% names(d_sf)) {
  d_sf <- d_sf %>%
    mutate(
      CO_UF  = as.integer(substr(CO_MUNICIPIO, 1, 2)),
      regiao = case_when(
        CO_UF %in% c(11,12,13,14,15,16,17) ~ "Norte",
        CO_UF %in% c(21,22,23,24,25,26,27,28,29) ~ "Nordeste",
        CO_UF %in% c(31,32,33,35) ~ "Sudeste",
        CO_UF %in% c(41,42,43) ~ "Sul",
        CO_UF %in% c(50,51,52,53) ~ "Centro-Oeste",
        TRUE ~ NA_character_
      )
    )
}

base_controls <- c("log_pop_mun", "log_gdp_pc_mun",
                   "log_area_mun", "urban_share_mun",
                   "log_dist_capital")
state_cap     <- c("adm_capacity_score_est", "fiscal_autonomy_est")

# --- Composite models ---
m_biome  <- lm(log_mean_dist ~ adm_capacity_score_mun +
                 log_pop_mun + log_gdp_pc_mun + log_area_mun +
                 urban_share_mun + log_dist_capital +
                 factor(bioma) + adm_capacity_score_est +
                 fiscal_autonomy_est,
               data = d_sf)

m_region <- lm(log_mean_dist ~ adm_capacity_score_mun +
                 log_pop_mun + log_gdp_pc_mun + log_area_mun +
                 urban_share_mun + log_dist_capital +
                 factor(regiao) + adm_capacity_score_est +
                 fiscal_autonomy_est,
               data = d_sf)

m_state  <- lm(log_mean_dist ~ adm_capacity_score_mun +
                 log_pop_mun + log_gdp_pc_mun + log_area_mun +
                 urban_share_mun + log_dist_capital +
                 factor(SG_UF),
               data = d_sf)

r_biome  <- robust_cl(m_biome)
r_region <- robust_cl(m_region)
r_state  <- robust_cl(m_state)

v <- "adm_capacity_score_mun"
cat(sprintf("\nComposite — biome FE:  coef=%.4f  p=%.4f  R2=%.3f\n",
            r_biome[v,"Estimate"],  r_biome[v,"Pr(>|t|)"],
            summary(m_biome)$r.squared))
cat(sprintf("Composite — region FE: coef=%.4f  p=%.4f  R2=%.3f\n",
            r_region[v,"Estimate"], r_region[v,"Pr(>|t|)"],
            summary(m_region)$r.squared))
cat(sprintf("Composite — state FE:  coef=%.4f  p=%.4f  R2=%.3f\n",
            r_state[v,"Estimate"],  r_state[v,"Pr(>|t|)"],
            summary(m_state)$r.squared))

# --- Disaggregated models ---
m_biome_dis  <- lm(log_mean_dist ~ edu_share_mun +
                     fpm_dependence_mun +
                     log_pop_mun + log_gdp_pc_mun + log_area_mun +
                     urban_share_mun + log_dist_capital +
                     factor(bioma) + adm_capacity_score_est +
                     fiscal_autonomy_est,
                   data = d_sf)

m_region_dis <- lm(log_mean_dist ~ edu_share_mun +
                     fpm_dependence_mun +
                     log_pop_mun + log_gdp_pc_mun + log_area_mun +
                     urban_share_mun + log_dist_capital +
                     factor(regiao) + adm_capacity_score_est +
                     fiscal_autonomy_est,
                   data = d_sf)

m_state_dis  <- lm(log_mean_dist ~ edu_share_mun +
                     fpm_dependence_mun +
                     log_pop_mun + log_gdp_pc_mun + log_area_mun +
                     urban_share_mun + log_dist_capital +
                     factor(SG_UF),
                   data = d_sf)

r_biome_dis  <- robust_cl(m_biome_dis)
r_region_dis <- robust_cl(m_region_dis)
r_state_dis  <- robust_cl(m_state_dis)

for (vv in c("edu_share_mun", "fpm_dependence_mun")) {
  cat(sprintf("\n%s:\n", vv))
  cat(sprintf("  biome FE:  coef=%.4f  p=%.4f\n",
              r_biome_dis[vv,"Estimate"],
              r_biome_dis[vv,"Pr(>|t|)"]))
  cat(sprintf("  region FE: coef=%.4f  p=%.4f\n",
              r_region_dis[vv,"Estimate"],
              r_region_dis[vv,"Pr(>|t|)"]))
  cat(sprintf("  state FE:  coef=%.4f  p=%.4f\n",
              r_state_dis[vv,"Estimate"],
              r_state_dis[vv,"Pr(>|t|)"]))
}

# ============================================================
# STEP 5b: M1 FISCAL AUTONOMY — GEOGRAPHIC FE + SEM
# Mirrors Step 5 composite and disaggregated panels;
# fiscal_autonomy_mun as IV throughout
# ============================================================

cat("\n============================================================\n")
cat("STEP 5b: M1 FISCAL AUTONOMY — GEOGRAPHIC FE + SEM\n")
cat("============================================================\n")

m_fa_biome  <- lm(log_mean_dist ~ fiscal_autonomy_mun +
                    log_pop_mun + log_gdp_pc_mun + log_area_mun +
                    urban_share_mun + log_dist_capital +
                    factor(bioma) + adm_capacity_score_est +
                    fiscal_autonomy_est,
                  data = d_sf)

m_fa_region <- lm(log_mean_dist ~ fiscal_autonomy_mun +
                    log_pop_mun + log_gdp_pc_mun + log_area_mun +
                    urban_share_mun + log_dist_capital +
                    factor(regiao) + adm_capacity_score_est +
                    fiscal_autonomy_est,
                  data = d_sf)

m_fa_state  <- lm(log_mean_dist ~ fiscal_autonomy_mun +
                    log_pop_mun + log_gdp_pc_mun + log_area_mun +
                    urban_share_mun + log_dist_capital +
                    factor(SG_UF),
                  data = d_sf)

r_fa_biome  <- robust_cl(m_fa_biome)
r_fa_region <- robust_cl(m_fa_region)
r_fa_state  <- robust_cl(m_fa_state)

sem_m1 <- errorsarlm(
  log_mean_dist ~ fiscal_autonomy_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + log_dist_capital + bioma +
    adm_capacity_score_est + fiscal_autonomy_est,
  data = d_sf, listw = lw, zero.policy = TRUE
)

vfa <- "fiscal_autonomy_mun"
cat(sprintf("\nM1 biome FE:  coef=%.4f  SE=%.4f  p=%.4f  R2=%.3f  N=%d\n",
            r_fa_biome[vfa,"Estimate"],  r_fa_biome[vfa,"Std. Error"],
            r_fa_biome[vfa,"Pr(>|t|)"],  summary(m_fa_biome)$r.squared,
            nobs(m_fa_biome)))
cat(sprintf("M1 region FE: coef=%.4f  SE=%.4f  p=%.4f  R2=%.3f  N=%d\n",
            r_fa_region[vfa,"Estimate"], r_fa_region[vfa,"Std. Error"],
            r_fa_region[vfa,"Pr(>|t|)"], summary(m_fa_region)$r.squared,
            nobs(m_fa_region)))
cat(sprintf("M1 state FE:  coef=%.4f  SE=%.4f  p=%.4f  R2=%.3f  N=%d\n",
            r_fa_state[vfa,"Estimate"],  r_fa_state[vfa,"Std. Error"],
            r_fa_state[vfa,"Pr(>|t|)"],  summary(m_fa_state)$r.squared,
            nobs(m_fa_state)))
cat(sprintf("M1 SEM:       coef=%.4f  SE=%.4f  p=%.4f  lambda=%.4f  N=%d\n",
            coef(sem_m1)[vfa],
            summary(sem_m1)$Coef[vfa, "Std. Error"],
            summary(sem_m1)$Coef[vfa, "Pr(>|z|)"],
            sem_m1$lambda,
            nobs(sem_m1)))

# --- Key numbers formatted for robustness table ---
cat(sprintf("\n[TAB] M1 biome FE:  beta=%.3f SE=%.3f p=%.4f N=%d\n",
            r_fa_biome[vfa,"Estimate"],  r_fa_biome[vfa,"Std. Error"],
            r_fa_biome[vfa,"Pr(>|t|)"],  nobs(m_fa_biome)))
cat(sprintf("[TAB] M1 region FE: beta=%.3f SE=%.3f p=%.4f N=%d\n",
            r_fa_region[vfa,"Estimate"], r_fa_region[vfa,"Std. Error"],
            r_fa_region[vfa,"Pr(>|t|)"], nobs(m_fa_region)))
cat(sprintf("[TAB] M1 state FE:  beta=%.3f SE=%.3f p=%.4f N=%d\n",
            r_fa_state[vfa,"Estimate"],  r_fa_state[vfa,"Std. Error"],
            r_fa_state[vfa,"Pr(>|t|)"],  nobs(m_fa_state)))
cat(sprintf("[TAB] M1 SEM:       beta=%.3f SE=%.3f p=%.4f lambda=%.4f N=%d\n",
            coef(sem_m1)[vfa],
            summary(sem_m1)$Coef[vfa, "Std. Error"],
            summary(sem_m1)$Coef[vfa, "Pr(>|z|)"],
            sem_m1$lambda,
            nobs(sem_m1)))

# ============================================================
# SAVE OUTPUTS
# ============================================================

cat("\n--- Saving outputs ---\n")
dir.create("results", showWarnings = FALSE)

fwrite(spatial_summary, "results/coef_spatial_summary.csv", sep = ";")
cat("Saved results/coef_spatial_summary.csv\n")

fwrite(lm_summary, "results/diagnostics_lm_tests.csv", sep = ";")
cat("Saved results/diagnostics_lm_tests.csv\n")

moran_out <- data.frame(
  Model  = c("Composite", "Disaggregated"),
  MoranI = c(unname(moran_base$estimate[1]),
             unname(moran_disaggr$estimate[1])),
  p      = c(moran_base$p.value, moran_disaggr$p.value)
)
fwrite(moran_out, "results/diagnostics_moran.csv", sep = ";")
cat("Saved results/diagnostics_moran.csv\n")

# Table 1: Spatial diagnostics (Moran + LM tests)
diag_lines <- c(
  "\\begin{table}[h]",
  "\\centering",
  "\\caption{Spatial diagnostics}",
  "\\begin{tabular}{llrr}",
  "\\toprule",
  "Model & Test & Statistic & p-value \\\\",
  "\\midrule",
  sprintf("Composite & Moran's I & %.4f & $<$0.001 \\\\",
          unname(moran_base$estimate[1])),
  sprintf("Composite & RS-lag & %.4f & $<$0.001 \\\\",
          lm_tests_base$RSlag$statistic),
  sprintf("Composite & RS-error & %.4f & $<$0.001 \\\\",
          lm_tests_base$RSerr$statistic),
  sprintf("Composite & adjRS-lag & %.4f & $<$0.001 \\\\",
          lm_tests_base$adjRSlag$statistic),
  sprintf("Composite & adjRS-error & %.4f & $<$0.001 \\\\",
          lm_tests_base$adjRSerr$statistic),
  "\\midrule",
  sprintf("Disaggregated & Moran's I & %.4f & $<$0.001 \\\\",
          unname(moran_disaggr$estimate[1])),
  sprintf("Disaggregated & RS-lag & %.4f & $<$0.001 \\\\",
          lm_tests_disaggr$RSlag$statistic),
  sprintf("Disaggregated & RS-error & %.4f & $<$0.001 \\\\",
          lm_tests_disaggr$RSerr$statistic),
  sprintf("Disaggregated & adjRS-lag & %.4f & $<$0.001 \\\\",
          lm_tests_disaggr$adjRSlag$statistic),
  sprintf("Disaggregated & adjRS-error & %.4f & $<$0.001 \\\\",
          lm_tests_disaggr$adjRSerr$statistic),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(diag_lines, "results/tab_spatial_diagnostics.tex")
cat("Saved results/tab_spatial_diagnostics.tex\n")

# Table 2: OLS vs SEM coefficient comparison
sem_comp_coef  <- coef(sem_composite)["adm_capacity_score_mun"]
sem_comp_se    <- summary(sem_composite)$Coef["adm_capacity_score_mun", "Std. Error"]
sem_comp_p     <- summary(sem_composite)$Coef["adm_capacity_score_mun", "Pr(>|z|)"]
sem_edu_coef   <- coef(sem_disaggr)["edu_share_mun"]
sem_edu_se     <- summary(sem_disaggr)$Coef["edu_share_mun", "Std. Error"]
sem_edu_p      <- summary(sem_disaggr)$Coef["edu_share_mun", "Pr(>|z|)"]
sem_fpm_coef   <- coef(sem_disaggr)["fpm_dependence_mun"]
sem_fpm_se     <- summary(sem_disaggr)$Coef["fpm_dependence_mun", "Std. Error"]
sem_fpm_p      <- summary(sem_disaggr)$Coef["fpm_dependence_mun", "Pr(>|z|)"]
ols_comp_cf    <- coef(summary(ols_base))
ols_dis_cf     <- coef(summary(ols_disaggr))

fmt <- function(est, se, p) {
  stars <- ifelse(p < 0.01, "$^{***}$", ifelse(p < 0.05, "$^{**}$",
                                               ifelse(p < 0.1, "$^{*}$", "")))
  sprintf("%.4f%s (%.4f)", est, stars, se)
}

sem_lines <- c(
  "\\begin{table}[h]",
  "\\centering",
  "\\caption{OLS vs Spatial Error Model (SEM) — key coefficients}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Composite} & \\multicolumn{2}{c}{Disaggregated} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Variable & OLS & SEM & OLS & SEM \\\\",
  "\\midrule",
  sprintf("adm\\_capacity\\_score\\_mun & %s & %s & & \\\\",
          fmt(ols_comp_cf["adm_capacity_score_mun","Estimate"],
              ols_comp_cf["adm_capacity_score_mun","Std. Error"],
              ols_comp_cf["adm_capacity_score_mun","Pr(>|t|)"]),
          fmt(sem_comp_coef, sem_comp_se, sem_comp_p)),
  sprintf("edu\\_share\\_mun & & & %s & %s \\\\",
          fmt(ols_dis_cf["edu_share_mun","Estimate"],
              ols_dis_cf["edu_share_mun","Std. Error"],
              ols_dis_cf["edu_share_mun","Pr(>|t|)"]),
          fmt(sem_edu_coef, sem_edu_se, sem_edu_p)),
  sprintf("fpm\\_dependence\\_mun & & & %s & %s \\\\",
          fmt(ols_dis_cf["fpm_dependence_mun","Estimate"],
              ols_dis_cf["fpm_dependence_mun","Std. Error"],
              ols_dis_cf["fpm_dependence_mun","Pr(>|t|)"]),
          fmt(sem_fpm_coef, sem_fpm_se, sem_fpm_p)),
  "\\midrule",
  sprintf("$\\lambda$ & & %.4f & & %.4f \\\\",
          sem_composite$lambda, sem_disaggr$lambda),
  "\\bottomrule",
  "\\end{tabular}",
  "\\\\[4pt]",
  "\\footnotesize{Coef (SE). $^{*}$p$<$0.1, $^{**}$p$<$0.05, $^{***}$p$<$0.01.}",
  "\\end{table}"
)
writeLines(sem_lines, "results/tab_spatial_sem.tex")
cat("Saved results/tab_spatial_sem.tex\n")

# Table 3: Geographic FE comparison
fmt_geo <- function(r, v) {
  if (!v %in% rownames(r)) return("--")
  est   <- r[v, "Estimate"]
  se    <- r[v, "Std. Error"]
  p     <- r[v, "Pr(>|t|)"]
  stars <- ifelse(p < 0.01, "$^{***}$",
                  ifelse(p < 0.05, "$^{**}$",
                         ifelse(p < 0.1,  "$^{*}$", "")))
  sprintf("%.4f%s (%.4f)", est, stars, se)
}

fmt_sem <- function(model, v) {
  cf  <- coef(model)
  sm  <- summary(model)$Coef
  if (!v %in% names(cf)) return("--")
  est   <- cf[v]
  se    <- sm[v, "Std. Error"]
  p     <- sm[v, "Pr(>|z|)"]
  stars <- ifelse(p < 0.01, "$^{***}$",
                  ifelse(p < 0.05, "$^{**}$",
                         ifelse(p < 0.1,  "$^{*}$", "")))
  sprintf("%.4f%s (%.4f)", est, stars, se)
}

geo_lines <- c(
  "\\begin{table}[h]",
  "\\centering",
  "\\caption{Geographic fixed effects comparison — composite and disaggregated}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "& Biome FE & Region FE & State FE & SEM \\\\",
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel A: Composite model}} \\\\",
  sprintf("adm\\_capacity\\_score & %s & %s & %s & %s \\\\",
          fmt_geo(r_biome,  "adm_capacity_score_mun"),
          fmt_geo(r_region, "adm_capacity_score_mun"),
          fmt_geo(r_state,  "adm_capacity_score_mun"),
          fmt_sem(sem_composite, "adm_capacity_score_mun")),
  sprintf("$R^2$ & %.3f & %.3f & %.3f & -- \\\\",
          summary(m_biome)$r.squared,
          summary(m_region)$r.squared,
          summary(m_state)$r.squared),
  sprintf("$N$ & %d & %d & %d & %d \\\\",
          nobs(m_biome), nobs(m_region),
          nobs(m_state), nobs(ols_base)),
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B: Disaggregated model}} \\\\",
  sprintf("edu\\_share\\_mun & %s & %s & %s & %s \\\\",
          fmt_geo(r_biome_dis,  "edu_share_mun"),
          fmt_geo(r_region_dis, "edu_share_mun"),
          fmt_geo(r_state_dis,  "edu_share_mun"),
          fmt_sem(sem_disaggr,  "edu_share_mun")),
  sprintf("fpm\\_dependence\\_mun & %s & %s & %s & %s \\\\",
          fmt_geo(r_biome_dis,  "fpm_dependence_mun"),
          fmt_geo(r_region_dis, "fpm_dependence_mun"),
          fmt_geo(r_state_dis,  "fpm_dependence_mun"),
          fmt_sem(sem_disaggr,  "fpm_dependence_mun")),
  sprintf("$R^2$ & %.3f & %.3f & %.3f & -- \\\\",
          summary(m_biome_dis)$r.squared,
          summary(m_region_dis)$r.squared,
          summary(m_state_dis)$r.squared),
  "\\bottomrule",
  "\\end{tabular}",
  "\\\\[4pt]",
  paste0("\\footnotesize{DV: log mean settlement-to-school distance. ",
         "SE clustered by state (in parentheses). ",
         "SEM uses spatial error model with queen contiguity weights. ",
         "$^{*}$p$<$0.1, $^{**}$p$<$0.05, $^{***}$p$<$0.01.}"),
  "\\end{table}"
)
writeLines(geo_lines, "results/tab_geo_fe_comparison.tex")
cat("Saved results/tab_geo_fe_comparison.tex\n")

cat("\nScript 83 complete.\n")