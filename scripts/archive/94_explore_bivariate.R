# ==================================================
# 94_explore_models.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Preliminary model exploration at
#       municipality level. Tests relationships
#       between fiscal capacity, geographic reach,
#       and secondary school accessibility.
# Input:  data/processed/master_mun_main.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading master_mun_main...\n")
main <- load_br_csv("data/processed/master_mun_main.csv")
report_dims(main, "master_mun_main")

# Quick summary of key variables
cat("\n--- Key variable summary ---\n")
report_indicators(main, c(
  "dist_nearest_school_km", "log_dist_school",
  "schools_per_10k", "log_schools_per_10k",
  "adm_capacity_score_mun", "fiscal_autonomy_mun",
  "dist_capital_km", "log_pop_mun", "log_gdp_pc_mun"
))

# ============================================================
# OUTCOME 1: DISTANCE TO NEAREST SCHOOL
# OLS on log_dist_school (continuous, right skewed)
# Only meaningful for municipalities WITHOUT schools
# For municipalities with schools dist=0 → log=0
# ============================================================

cat("\n============================================================\n")
cat("OUTCOME 1: DISTANCE TO NEAREST SCHOOL\n")
cat("============================================================\n")

# Subset to municipalities WITHOUT schools only
# These are the true accessibility problem cases
no_school <- main %>% filter(!has_school)
cat("Municipalities without schools:", nrow(no_school), "\n")

# M1a: Simple — fiscal capacity only
m1a <- lm(dist_nearest_school_km ~ adm_capacity_score_mun +
             log_pop_mun + log_gdp_pc_mun,
           data = no_school)
cat("\nM1a: Distance ~ adm_capacity (no school municipalities)\n")
print(summary(m1a))

# M1b: Add geographic controls
m1b <- lm(dist_nearest_school_km ~ adm_capacity_score_mun +
             dist_capital_km +
             log_pop_mun + log_gdp_pc_mun +
             factor(bioma),
           data = no_school)
cat("\nM1b: Distance ~ adm_capacity + geography\n")
print(summary(m1b))

# M1c: Tilly indicator instead of composite
m1c <- lm(dist_nearest_school_km ~ fiscal_autonomy_mun +
             dist_capital_km +
             log_pop_mun + log_gdp_pc_mun +
             factor(bioma),
           data = no_school)
cat("\nM1c: Distance ~ fiscal_autonomy (Tilly) + geography\n")
print(summary(m1c))

# ============================================================
# OUTCOME 2: SCHOOL COVERAGE (schools per 10k)
# All municipalities including zeros
# ============================================================

cat("\n============================================================\n")
cat("OUTCOME 2: SCHOOL COVERAGE (schools per 10,000 pop)\n")
cat("============================================================\n")

# M2a: Simple — fiscal capacity only
m2a <- lm(schools_per_10k ~ adm_capacity_score_mun +
             log_pop_mun + log_gdp_pc_mun,
           data = main)
cat("\nM2a: Coverage ~ adm_capacity\n")
print(summary(m2a))

# M2b: Add geographic controls
m2b <- lm(schools_per_10k ~ adm_capacity_score_mun +
             dist_capital_km +
             log_pop_mun + log_gdp_pc_mun +
             factor(bioma),
           data = main)
cat("\nM2b: Coverage ~ adm_capacity + geography\n")
print(summary(m2b))

# M2c: Tilly indicator
m2c <- lm(schools_per_10k ~ fiscal_autonomy_mun +
             dist_capital_km +
             log_pop_mun + log_gdp_pc_mun +
             factor(bioma),
           data = main)
cat("\nM2c: Coverage ~ fiscal_autonomy (Tilly) + geography\n")
print(summary(m2c))

# ============================================================
# OUTCOME 3: HAS SCHOOL (binary)
# Logit model — does the municipality have any school?
# ============================================================

cat("\n============================================================\n")
cat("OUTCOME 3: HAS SCHOOL (binary logit)\n")
cat("============================================================\n")

# M3a: Fiscal capacity
m3a <- glm(has_school ~ adm_capacity_score_mun +
              log_pop_mun + log_gdp_pc_mun,
            data = main, family = binomial)
cat("\nM3a: Has school ~ adm_capacity (logit)\n")
print(summary(m3a))

# M3b: Add geography
m3b <- glm(has_school ~ adm_capacity_score_mun +
              dist_capital_km +
              log_pop_mun + log_gdp_pc_mun +
              factor(bioma),
            data = main, family = binomial)
cat("\nM3b: Has school ~ adm_capacity + geography (logit)\n")
print(summary(m3b))

# M3c: Tilly indicator
m3c <- glm(has_school ~ fiscal_autonomy_mun +
              dist_capital_km +
              log_pop_mun + log_gdp_pc_mun +
              factor(bioma),
            data = main, family = binomial)
cat("\nM3c: Has school ~ fiscal_autonomy (Tilly) + geography (logit)\n")
print(summary(m3c))

# ============================================================
# QUICK CORRELATION CHECK
# ============================================================

cat("\n============================================================\n")
cat("CORRELATIONS WITH OUTCOMES\n")
cat("============================================================\n")

cor_vars <- main %>%
  select(dist_nearest_school_km, schools_per_10k,
         adm_capacity_score_mun, fiscal_autonomy_mun,
         fpm_dependence_mun, dist_capital_km,
         log_pop_mun, log_gdp_pc_mun) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

print(cor_vars)

cat("\nScript 94 complete.\n")