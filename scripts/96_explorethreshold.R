# ==================================================
# 96_explorethreshold.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Explore population threshold effects on
#       school presence and coverage. Test whether
#       there is a minimum population size below
#       which municipalities systematically lack
#       secondary schools. Also run rural-only
#       models where school provision theory
#       applies most directly.
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

# Add region variable
main <- main %>%
  mutate(
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
    # Mostly rural municipality: urban_share < 0.5
    mostly_rural = urban_share_mun < 0.5
  )

# ============================================================
# PART 1: POPULATION THRESHOLD ANALYSIS
# At what population size do municipalities get schools?
# ============================================================

cat("\n============================================================\n")
cat("PART 1: POPULATION THRESHOLD ANALYSIS\n")
cat("============================================================\n")

# Distribution of has_school by population bins
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
    n_mun         = n(),
    n_with_school = sum(has_school),
    pct_with_school = round(100 * n_with_school / n_mun, 1),
    mean_dist_km  = round(mean(dist_nearest_school_km,
                               na.rm = TRUE), 1),
    mean_coverage = round(mean(schools_per_10k,
                               na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\nSchool presence by population size:\n")
print(pop_bins)

# Where is the threshold?
cat("\nThreshold check — % with school by fine population bins:\n")
main %>%
  mutate(pop_k = pop_mun / 1000) %>%
  mutate(pop_bin = cut(pop_k,
                       breaks = c(0,1,2,3,4,5,7,10,15,20,Inf),
                       labels = c("<1k","1-2k","2-3k","3-4k",
                                  "4-5k","5-7k","7-10k",
                                  "10-15k","15-20k","20k+"))) %>%
  group_by(pop_bin) %>%
  summarise(
    n = n(),
    pct_school = round(100 * mean(has_school), 1),
    .groups = "drop"
  ) %>%
  print()

# ============================================================
# PART 2: REGRESSION DISCONTINUITY STYLE THRESHOLD
# Does school presence jump at a specific population cutoff?
# ============================================================

cat("\n============================================================\n")
cat("PART 2: THRESHOLD REGRESSION\n")
cat("============================================================\n")

# Test several cutoffs: 5k, 10k, 15k, 20k
thresholds <- c(5000, 10000, 15000, 20000)

for (thresh in thresholds) {
  main_thresh <- main %>%
    filter(!is.na(fiscal_autonomy_mun)) %>%
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
                 data = main_thresh)

  coefs <- summary(m_thresh)$coefficients
  cat(sprintf("\nThreshold at %dk pop:\n", thresh/1000))
  cat(sprintf("  above_thresh coef=%.3f p=%.4f\n",
              coefs["above_thresh", "Estimate"],
              coefs["above_thresh", "Pr(>|t|)"]))
  cat(sprintf("  R2=%.3f\n", summary(m_thresh)$r.squared))
}

# ============================================================
# PART 3: RURAL MUNICIPALITIES ONLY
# Theory: school provision most constrained in rural areas
# Urban municipalities almost always have schools
# ============================================================

cat("\n============================================================\n")
cat("PART 3: RURAL MUNICIPALITIES ONLY\n")
cat("============================================================\n")

rural <- main %>% filter(mostly_rural)
cat("Rural municipalities (urban_share < 0.5):",
    nrow(rural), "\n")
cat("No school among rural:", sum(!rural$has_school), "\n")
cat("Pct with school:", round(100 * mean(rural$has_school), 1), "%\n")

cat("\nRural profile by region:\n")
rural %>%
  group_by(regiao) %>%
  summarise(
    n = n(),
    no_school = sum(!has_school),
    pct_school = round(100 * mean(has_school), 1),
    mean_dist  = round(mean(dist_nearest_school_km,
                            na.rm = TRUE), 1),
    mean_coverage = round(mean(schools_per_10k,
                               na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()

# R1: Coverage model — rural only
cat("\n--- R1: Coverage ~ fiscal capacity (rural only) ---\n")
r1 <- lm(schools_per_10k ~
           fpm_dependence_mun +
           fiscal_autonomy_mun +
           dist_capital_km +
           log_pop_mun +
           log_gdp_pc_mun +
           factor(bioma),
         data = rural)
print(summary(r1))

# R2: Coverage ~ fiscal capacity, rural periphery only
cat("\n--- R2: Coverage ~ fiscal capacity (rural Norte+Nordeste) ---\n")
r2 <- lm(schools_per_10k ~
           fpm_dependence_mun +
           fiscal_autonomy_mun +
           dist_capital_km +
           log_pop_mun +
           log_gdp_pc_mun +
           factor(bioma),
         data = rural %>% filter(periphery))
print(summary(r2))

# R3: Distance model — rural no-school only
no_school_rural <- rural %>% filter(!has_school)
cat(sprintf("\n--- R3: Distance model (rural no-school, n=%d) ---\n",
            nrow(no_school_rural)))

if (nrow(no_school_rural) > 20) {
  r3 <- lm(dist_nearest_school_km ~
             fpm_dependence_mun +
             fiscal_autonomy_mun +
             log_pop_mun +
             log_gdp_pc_mun +
             factor(regiao),
           data = no_school_rural)
  print(summary(r3))
}

# ============================================================
# PART 4: URBAN vs RURAL COMPARISON
# Side by side key coefficients
# ============================================================

cat("\n============================================================\n")
cat("PART 4: URBAN vs RURAL COMPARISON\n")
cat("============================================================\n")

urban <- main %>% filter(!mostly_rural)
cat("Urban municipalities:", nrow(urban), "\n")
cat("Rural municipalities:", nrow(rural), "\n")

# Run same model on both
for (label in c("Urban", "Rural")) {
  df <- if (label == "Urban") urban else rural
  m <- lm(schools_per_10k ~
            fpm_dependence_mun +
            fiscal_autonomy_mun +
            log_pop_mun +
            log_gdp_pc_mun +
            factor(bioma),
          data = df)
  coefs <- summary(m)$coefficients
  cat(sprintf("\n%s (n=%d, no_school=%d):\n",
              label, nrow(df), sum(!df$has_school)))
  for (v in c("fpm_dependence_mun", "fiscal_autonomy_mun",
              "log_pop_mun", "log_gdp_pc_mun")) {
    if (v %in% rownames(coefs)) {
      cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
                  v,
                  coefs[v, "Estimate"],
                  coefs[v, "Pr(>|t|)"]))
    }
  }
  cat(sprintf("  R2=%.3f\n", summary(m)$r.squared))
}

cat("\nScript 96 complete.\n")