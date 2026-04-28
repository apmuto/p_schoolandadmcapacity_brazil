# ==================================================
# 93_explore_int.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Explore interaction effects between fiscal
#       capacity and key moderators:
#       1. Fiscal capacity × Rural/Urban
#       2. Fiscal capacity × Periphery
#       3. Fiscal capacity × Population size
#       4. Fiscal capacity × Distance to capital
#       5. FPM dependence × Population threshold
# Input:  data/processed/master_mun_main.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(lmtest)
library(sandwich)

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading master_mun_main...\n")
main <- load_br_csv("data/processed/master_mun_main.csv") %>%
  mutate(
    CO_MUNICIPIO = as.character(CO_MUNICIPIO),
    CO_UF = as.integer(substr(CO_MUNICIPIO, 1, 2)),
    regiao = case_when(
      CO_UF %in% c(11,12,13,14,15,16,17) ~ "Norte",
      CO_UF %in% c(21,22,23,24,25,26,27,28,29) ~ "Nordeste",
      CO_UF %in% c(31,32,33,35) ~ "Sudeste",
      CO_UF %in% c(41,42,43) ~ "Sul",
      CO_UF %in% c(50,51,52,53) ~ "Centro-Oeste",
      TRUE ~ NA_character_
    ),
    periphery     = as.integer(regiao %in%
                                 c("Norte", "Nordeste")),
    rural         = as.integer(urban_type == "Rural"),
    small_mun     = as.integer(pop_mun < 5000),
    far_capital   = as.integer(dist_capital_km > 200)
  )

report_dims(main, "master_mun_main")

# ============================================================
# HELPER: print model with robust SE
# ============================================================

print_robust <- function(model, cluster_var, label = "") {
  cat(sprintf("\n%s\n", label))
  coefs <- coeftest(model,
                    vcov = vcovCL(model,
                                  cluster = cluster_var))
  # Print only non-biome rows
  keep <- !grepl("bioma|SG_UF|Intercept", rownames(coefs))
  print(coefs[keep, ])
  cat(sprintf("R²=%.3f  n=%d\n",
              summary(model)$r.squared,
              nobs(model)))
}

# ============================================================
# PART 1: FISCAL CAPACITY × RURAL/URBAN
# Does fiscal capacity matter more in rural areas?
# O'Donnell: state reach most limited in rural periphery
# ============================================================

cat("\n============================================================\n")
cat("PART 1: FISCAL CAPACITY × RURAL\n")
cat("H: Fiscal capacity reduces distance MORE in rural areas\n")
cat("============================================================\n")

# Interaction: fpm_dependence × rural
m1 <- lm(log_mean_dist_school ~
           fpm_dependence_mun * rural +
           fiscal_autonomy_mun * rural +
           log_pop_mun + log_gdp_pc_mun +
           factor(bioma),
         data = main)

print_robust(m1, ~SG_UF,
             "FPM × Rural + Fiscal autonomy × Rural")

# Marginal effects
cat("\nMarginal effect of fpm_dependence:\n")
b_fpm    <- coef(m1)["fpm_dependence_mun"]
b_int    <- coef(m1)["fpm_dependence_mun:rural"]
cat(sprintf("  Urban (rural=0): %.3f\n", b_fpm))
cat(sprintf("  Rural (rural=1): %.3f\n", b_fpm + b_int))

cat("\nMarginal effect of fiscal_autonomy:\n")
b_fa     <- coef(m1)["fiscal_autonomy_mun"]
b_int_fa <- coef(m1)["fiscal_autonomy_mun:rural"]
cat(sprintf("  Urban (rural=0): %.3f\n", b_fa))
cat(sprintf("  Rural (rural=1): %.3f\n", b_fa + b_int_fa))

# ============================================================
# PART 2: FISCAL CAPACITY × PERIPHERY
# Does fiscal capacity matter more in Norte/Nordeste?
# ============================================================

cat("\n============================================================\n")
cat("PART 2: FISCAL CAPACITY × PERIPHERY\n")
cat("H: Fiscal capacity reduces distance MORE in periphery\n")
cat("============================================================\n")

m2 <- lm(log_mean_dist_school ~
           fpm_dependence_mun * periphery +
           fiscal_autonomy_mun * periphery +
           log_pop_mun + log_gdp_pc_mun +
           factor(bioma),
         data = main)

print_robust(m2, ~SG_UF,
             "FPM × Periphery + Fiscal autonomy × Periphery")

cat("\nMarginal effect of fpm_dependence:\n")
b_fpm  <- coef(m2)["fpm_dependence_mun"]
b_int  <- coef(m2)["fpm_dependence_mun:periphery"]
cat(sprintf("  Core (periphery=0):    %.3f\n", b_fpm))
cat(sprintf("  Periphery (=1):        %.3f\n", b_fpm + b_int))

cat("\nMarginal effect of fiscal_autonomy:\n")
b_fa   <- coef(m2)["fiscal_autonomy_mun"]
b_int  <- coef(m2)["fiscal_autonomy_mun:periphery"]
cat(sprintf("  Core (periphery=0):    %.3f\n", b_fa))
cat(sprintf("  Periphery (=1):        %.3f\n", b_fa + b_int))

# ============================================================
# PART 3: FISCAL CAPACITY × POPULATION SIZE
# Does fiscal capacity matter more for small municipalities?
# ============================================================

cat("\n============================================================\n")
cat("PART 3: FISCAL CAPACITY × POPULATION SIZE\n")
cat("H: Fiscal capacity reduces distance MORE for small muns\n")
cat("============================================================\n")

m3 <- lm(log_mean_dist_school ~
           fpm_dependence_mun * small_mun +
           fiscal_autonomy_mun * small_mun +
           log_pop_mun + log_gdp_pc_mun +
           factor(bioma),
         data = main)

print_robust(m3, ~SG_UF,
             "FPM × Small + Fiscal autonomy × Small")

cat("\nMarginal effect of fpm_dependence:\n")
b_fpm <- coef(m3)["fpm_dependence_mun"]
b_int <- coef(m3)["fpm_dependence_mun:small_mun"]
cat(sprintf("  Large (small=0): %.3f\n", b_fpm))
cat(sprintf("  Small (small=1): %.3f\n", b_fpm + b_int))

# ============================================================
# PART 4: FISCAL CAPACITY × DISTANCE TO CAPITAL
# Direct O'Donnell test: does fiscal capacity matter
# more when municipalities are far from state capital?
# ============================================================

cat("\n============================================================\n")
cat("PART 4: FISCAL CAPACITY × DISTANCE TO CAPITAL\n")
cat("H: Fiscal capacity matters MORE far from capital\n")
cat("(O'Donnell: state reach weakens with distance)\n")
cat("============================================================\n")

m4 <- lm(log_mean_dist_school ~
           fpm_dependence_mun * far_capital +
           fiscal_autonomy_mun * far_capital +
           log_pop_mun + log_gdp_pc_mun +
           factor(bioma),
         data = main)

print_robust(m4, ~SG_UF,
             "FPM × Far capital + Fiscal autonomy × Far capital")

cat("\nMarginal effect of fpm_dependence:\n")
b_fpm <- coef(m4)["fpm_dependence_mun"]
b_int <- coef(m4)["fpm_dependence_mun:far_capital"]
cat(sprintf("  Near capital (<200km): %.3f\n", b_fpm))
cat(sprintf("  Far capital (>200km):  %.3f\n", b_fpm + b_int))

cat("\nMarginal effect of fiscal_autonomy:\n")
b_fa  <- coef(m4)["fiscal_autonomy_mun"]
b_int <- coef(m4)["fiscal_autonomy_mun:far_capital"]
cat(sprintf("  Near capital (<200km): %.3f\n", b_fa))
cat(sprintf("  Far capital (>200km):  %.3f\n", b_fa + b_int))

# ============================================================
# PART 5: THREE-WAY — FPM × PERIPHERY × RURAL
# Where does FPM matter most?
# ============================================================

cat("\n============================================================\n")
cat("PART 5: FPM × PERIPHERY × RURAL (three-way)\n")
cat("Where does federal dependency matter most?\n")
cat("============================================================\n")

m5 <- lm(log_mean_dist_school ~
           fpm_dependence_mun * periphery * rural +
           log_pop_mun + log_gdp_pc_mun +
           factor(bioma),
         data = main)

coefs5 <- coef(m5)
b_fpm   <- coefs5["fpm_dependence_mun"]
b_p     <- coefs5["fpm_dependence_mun:periphery"]
b_r     <- coefs5["fpm_dependence_mun:rural"]
b_pr    <- coefs5["fpm_dependence_mun:periphery:rural"]

cat("\nMarginal effect of fpm_dependence by subgroup:\n")
cat(sprintf("  Core + Urban:       %.3f\n", b_fpm))
cat(sprintf("  Core + Rural:       %.3f\n", b_fpm + b_r))
cat(sprintf("  Periphery + Urban:  %.3f\n", b_fpm + b_p))
cat(sprintf("  Periphery + Rural:  %.3f\n",
            b_fpm + b_p + b_r + b_pr))

print_robust(m5, ~SG_UF, "Three-way interaction")

# ============================================================
# PART 6: CONTINUOUS INTERACTION
# FPM × log_dist_capital (continuous)
# ============================================================

cat("\n============================================================\n")
cat("PART 6: FPM × log_dist_capital (continuous)\n")
cat("============================================================\n")

m6 <- lm(log_mean_dist_school ~
           fpm_dependence_mun * log_dist_capital +
           fiscal_autonomy_mun +
           log_pop_mun + log_gdp_pc_mun +
           factor(bioma),
         data = main)

print_robust(m6, ~SG_UF,
             "FPM × log_dist_capital (continuous)")

# Marginal effect at different distances
cat("\nMarginal effect of fpm_dependence at different distances:\n")
b_fpm  <- coef(m6)["fpm_dependence_mun"]
b_int  <- coef(m6)["fpm_dependence_mun:log_dist_capital"]
for (dist in c(50, 100, 200, 400, 800)) {
  me <- b_fpm + b_int * log(dist + 1)
  cat(sprintf("  At %4dkm from capital: %.3f\n", dist, me))
}

# ============================================================
# MODEL COMPARISON — AIC
# ============================================================

cat("\n============================================================\n")
cat("MODEL COMPARISON — AIC\n")
cat("============================================================\n")

models <- list(
  "Base (no interaction)"  = lm(log_mean_dist_school ~
                                   fpm_dependence_mun +
                                   fiscal_autonomy_mun +
                                   log_pop_mun + log_gdp_pc_mun +
                                   factor(bioma), data = main),
  "× Rural"       = m1,
  "× Periphery"   = m2,
  "× Small mun"   = m3,
  "× Far capital" = m4,
  "Three-way"     = m5,
  "× Cont. dist"  = m6
)

for (name in names(models)) {
  cat(sprintf("  %-25s AIC=%8.1f  R²=%.3f\n",
              name,
              AIC(models[[name]]),
              summary(models[[name]])$r.squared))
}

cat("\nScript 93 complete.\n")