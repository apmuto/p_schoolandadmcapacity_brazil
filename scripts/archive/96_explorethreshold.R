# ==============================================================================
# 96_explorethreshold.R
# Project: Municipal administrative capacity and secondary school
#          accessibility in Brazil
#
# Goal: Explore population threshold effects on school presence and
#       coverage. Test whether there is a minimum population size below
#       which municipalities systematically lack secondary schools.
#       Also run rural-only models where school provision theory
#       applies most directly.
#
# Input:  data/processed/master_mun_main.csv
# Author: Ana Paula Muto
# ==============================================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(lmtest)
library(sandwich)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading master_mun_main...\n")
main <- load_br_csv("data/processed/master_mun_main.csv") %>%
  mutate(
    CO_MUNICIPIO = as.character(CO_MUNICIPIO),
    bioma        = relevel(factor(bioma), ref = "Cerrado"),
    mostly_rural = urban_share_mun < 0.5,
    periphery    = regiao %in% c("Norte", "Nordeste")
  )
report_dims(main, "master_mun_main")

# ==============================================================================
# PART 1: POPULATION THRESHOLD ANALYSIS
# At what population size do municipalities get schools?
# ==============================================================================

cat("\n============================================================\n")
cat("PART 1: POPULATION THRESHOLD ANALYSIS\n")
cat("============================================================\n")

pop_bins <- main %>%
  mutate(
    pop_cat = cut(pop_mun,
                  breaks = c(0, 2000, 5000, 10000, 20000,
                             50000, 100000, Inf),
                  labels = c("<2k", "2-5k", "5-10k",
                             "10-20k", "20-50k",
                             "50-100k", "100k+"))
  ) %>%
  group_by(pop_cat) %>%
  summarise(
    n_mun           = n(),
    n_with_school   = sum(has_school, na.rm = TRUE),
    pct_with_school = round(100 * n_with_school / n_mun, 1),
    mean_dist_km    = round(mean(mean_dist_km, na.rm = TRUE), 1),
    mean_coverage   = round(mean(schools_per_10k, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\nSchool presence by population size:\n")
print(pop_bins)

cat("\nFine-grained threshold check:\n")
main %>%
  mutate(pop_bin = cut(pop_mun / 1000,
                       breaks = c(0,1,2,3,4,5,7,10,15,20,Inf),
                       labels = c("<1k","1-2k","2-3k","3-4k",
                                  "4-5k","5-7k","7-10k",
                                  "10-15k","15-20k","20k+"))) %>%
  group_by(pop_bin) %>%
  summarise(
    n          = n(),
    pct_school = round(100 * mean(has_school, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  print()

# ==============================================================================
# PART 2: THRESHOLD REGRESSION
# Does school presence jump at a specific population cutoff?
# ==============================================================================

cat("\n============================================================\n")
cat("PART 2: THRESHOLD REGRESSION\n")
cat("============================================================\n")

thresholds <- c(5000, 10000, 15000, 20000)

for (thresh in thresholds) {
  d_thresh <- main %>%
    filter(!is.na(fiscal_autonomy_mun),
           !is.na(log_gdp_pc_mun),
           !is.na(regiao)) %>%
    mutate(
      above_thresh = as.integer(pop_mun >= thresh),
      pop_centered = pop_mun - thresh
    )
  
  m_thresh <- lm(has_school ~
                   above_thresh +
                   pop_centered +
                   fiscal_autonomy_mun +
                   log_gdp_pc_mun +
                   factor(regiao),
                 data = d_thresh)
  
  coefs <- summary(m_thresh)$coefficients
  cat(sprintf("\nThreshold at %dk:\n", thresh / 1000))
  cat(sprintf("  above_thresh  coef = %.3f  p = %.4f\n",
              coefs["above_thresh", "Estimate"],
              coefs["above_thresh", "Pr(>|t|)"]))
  cat(sprintf("  R2 = %.3f  N = %d\n",
              summary(m_thresh)$r.squared, nobs(m_thresh)))
}

# ==============================================================================
# PART 3: RURAL MUNICIPALITIES ONLY
# ==============================================================================

cat("\n============================================================\n")
cat("PART 3: RURAL MUNICIPALITIES ONLY\n")
cat("============================================================\n")

rural <- main %>% filter(mostly_rural == TRUE)
cat("Rural municipalities (urban_share < 0.5):", nrow(rural), "\n")
cat("No school among rural:", sum(!rural$has_school, na.rm = TRUE), "\n")
cat("Pct with school:", round(100 * mean(rural$has_school, na.rm = TRUE), 1), "%\n")

cat("\nRural profile by region:\n")
rural %>%
  group_by(regiao) %>%
  summarise(
    n             = n(),
    no_school     = sum(!has_school, na.rm = TRUE),
    pct_school    = round(100 * mean(has_school, na.rm = TRUE), 1),
    mean_dist_km  = round(mean(mean_dist_km, na.rm = TRUE), 1),
    mean_coverage = round(mean(schools_per_10k, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()

# R1: Coverage model — rural only
cat("\n--- R1: Coverage ~ fiscal capacity (rural only) ---\n")
r1 <- lm(schools_per_10k ~
           fpm_dependence_mun +
           fiscal_autonomy_mun +
           log_dist_capital +
           log_pop_mun +
           log_gdp_pc_mun +
           bioma,
         data = rural %>%
           filter(!is.na(fpm_dependence_mun),
                  !is.na(fiscal_autonomy_mun)))

r1_clustered <- coeftest(r1, vcov = vcovCL(r1, cluster = ~SG_UF))
cat("N:", nobs(r1), "| R2:", round(summary(r1)$r.squared, 3), "\n")
print(r1_clustered[c("fpm_dependence_mun", "fiscal_autonomy_mun",
                     "log_dist_capital", "log_pop_mun",
                     "log_gdp_pc_mun"), ])

# R2: Coverage ~ fiscal capacity, rural periphery only
cat("\n--- R2: Coverage ~ fiscal capacity (rural Norte+Nordeste) ---\n")
r2 <- lm(schools_per_10k ~
           fpm_dependence_mun +
           fiscal_autonomy_mun +
           log_dist_capital +
           log_pop_mun +
           log_gdp_pc_mun +
           bioma,
         data = rural %>%
           filter(periphery == TRUE,
                  !is.na(fpm_dependence_mun),
                  !is.na(fiscal_autonomy_mun)))

r2_clustered <- coeftest(r2, vcov = vcovCL(r2, cluster = ~SG_UF))
cat("N:", nobs(r2), "| R2:", round(summary(r2)$r.squared, 3), "\n")
print(r2_clustered[c("fpm_dependence_mun", "fiscal_autonomy_mun",
                     "log_dist_capital"), ])

# R3: Distance model — has_school == FALSE rural municipalities
no_school_rural <- rural %>%
  filter(has_school == FALSE,
         !is.na(fpm_dependence_mun),
         !is.na(fiscal_autonomy_mun),
         !is.na(mean_dist_km))

cat(sprintf("\n--- R3: Distance model (rural no-school, n=%d) ---\n",
            nrow(no_school_rural)))

if (nrow(no_school_rural) > 20) {
  r3 <- lm(mean_dist_km ~
             fpm_dependence_mun +
             fiscal_autonomy_mun +
             log_pop_mun +
             log_gdp_pc_mun +
             factor(regiao),
           data = no_school_rural)
  cat("N:", nobs(r3), "| R2:", round(summary(r3)$r.squared, 3), "\n")
  print(summary(r3)$coefficients[
    c("fpm_dependence_mun", "fiscal_autonomy_mun"), ])
}

# ==============================================================================
# PART 4: URBAN vs RURAL COMPARISON
# ==============================================================================

cat("\n============================================================\n")
cat("PART 4: URBAN vs RURAL COMPARISON\n")
cat("============================================================\n")

urban <- main %>% filter(mostly_rural == FALSE)
cat("Urban/mixed municipalities:", nrow(urban), "\n")
cat("Rural municipalities:", nrow(rural), "\n")

for (label in c("Urban", "Rural")) {
  df <- if (label == "Urban") urban else rural
  df_clean <- df %>%
    filter(!is.na(fpm_dependence_mun),
           !is.na(fiscal_autonomy_mun),
           !is.na(log_pop_mun),
           !is.na(log_gdp_pc_mun))
  
  m <- lm(schools_per_10k ~
            fpm_dependence_mun +
            fiscal_autonomy_mun +
            log_pop_mun +
            log_gdp_pc_mun +
            bioma,
          data = df_clean)
  
  m_cl <- coeftest(m, vcov = vcovCL(m, cluster = ~SG_UF))
  cat(sprintf("\n%s (n=%d, no_school=%d):\n",
              label, nrow(df_clean),
              sum(!df_clean$has_school, na.rm = TRUE)))
  
  for (v in c("fpm_dependence_mun", "fiscal_autonomy_mun",
              "log_pop_mun", "log_gdp_pc_mun")) {
    if (v %in% rownames(m_cl)) {
      cat(sprintf("  %-25s coef=%7.3f  p=%.3f\n",
                  v, m_cl[v, "Estimate"], m_cl[v, "Pr(>|t|)"]))
    }
  }
  cat(sprintf("  R2 = %.3f\n", summary(m)$r.squared))
}

# ==============================================================================
# PART 5: POPULATION TYPE SUMMARY
# ==============================================================================

cat("\n============================================================\n")
cat("PART 5: SUMMARY BY POPULATION TYPE\n")
cat("============================================================\n")

main %>%
  group_by(pop_type) %>%
  summarise(
    n             = n(),
    pct_school    = round(100 * mean(has_school, na.rm = TRUE), 1),
    mean_dist_km  = round(mean(mean_dist_km, na.rm = TRUE), 1),
    mean_fpm      = round(mean(fpm_dependence_mun, na.rm = TRUE), 3),
    mean_fiscal   = round(mean(fiscal_autonomy_mun, na.rm = TRUE), 3),
    mean_capacity = round(mean(adm_capacity_score_mun, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print()

cat("\nScript 96 complete.\n")