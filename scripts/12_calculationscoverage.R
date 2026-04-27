# ==================================================
# 12_calculationscoverage.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Compute school coverage indicators per
#       municipality:
#       1. n_schools: number of secondary schools
#       2. schools_per_10k: schools per 10,000 pop
#       3. enrollment_per_10k: secondary enrollment
#          per 10,000 inhabitants
# Input:  data/processed/cescola_media_clean.csv
#         data/processed/controls_municipios.csv
# Output: data/processed/coverage.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)

# ============================================================
# CONSTANTS
# ============================================================

PATH_ESCOLA  <- "data/processed/cescola_media_clean.csv"
PATH_CTRL    <- "data/processed/controls_municipios.csv"
PATH_OUT     <- "data/processed/coverage.csv"

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading escola...\n")
escola <- load_br_csv(PATH_ESCOLA) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
report_dims(escola, "escola")

cat("Loading controls...\n")
ctrl <- load_br_csv(PATH_CTRL) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
report_dims(ctrl, "controls")

# ============================================================
# COMPUTE SCHOOL COUNTS PER MUNICIPALITY
# ============================================================

cat("\nComputing school counts per municipality...\n")

school_counts <- escola %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(
    n_schools        = n(),
    n_schools_urban  = sum(TP_LOCALIZACAO == 1, na.rm = TRUE),
    n_schools_rural  = sum(TP_LOCALIZACAO == 2, na.rm = TRUE),
    n_schools_mun    = sum(TP_DEPENDENCIA == 3, na.rm = TRUE),
    n_schools_state  = sum(TP_DEPENDENCIA == 2, na.rm = TRUE),
    n_schools_fed    = sum(TP_DEPENDENCIA == 1, na.rm = TRUE),
    n_schools_priv   = sum(TP_DEPENDENCIA == 4, na.rm = TRUE),
    total_enrollment = sum(QT_MAT_MED, na.rm = TRUE),
    .groups = "drop"
  )

cat("Municipalities with schools:", nrow(school_counts), "\n")

# ============================================================
# MERGE WITH ALL MUNICIPALITIES
# Municipalities without schools get 0
# ============================================================

cat("\nMerging with full municipality list...\n")

coverage <- ctrl %>%
  select(CO_MUNICIPIO, SG_UF, pop_mun,
         log_pop_mun, log_gdp_pc_mun,
         bioma, dist_capital_km,
         urban_share_mun) %>%
  left_join(school_counts, by = "CO_MUNICIPIO") %>%
  mutate(
    across(c(n_schools, n_schools_urban, n_schools_rural,
             n_schools_mun, n_schools_state,
             n_schools_fed, n_schools_priv,
             total_enrollment),
           ~ ifelse(is.na(.), 0L, .))
  ) %>%
  mutate(
    has_school          = n_schools > 0,

    # Schools per 10,000 inhabitants
    schools_per_10k     = n_schools / (pop_mun / 10000),

    # Enrollment per 10,000 inhabitants
    enrollment_per_10k  = total_enrollment / (pop_mun / 10000),

    # Log transforms
    log_schools_per_10k    = log(schools_per_10k + 0.001),
    log_enrollment_per_10k = log(enrollment_per_10k + 0.001),

    # Coverage category
    coverage_cat = case_when(
      n_schools == 0  ~ "No school",
      n_schools == 1  ~ "1 school",
      n_schools <= 3  ~ "2-3 schools",
      n_schools <= 10 ~ "4-10 schools",
      TRUE            ~ "11+ schools"
    )
  )

# Fix missing biome for municipalities without schools
# Use the biome lookup already computed in script 11
acc <- load_br_csv("data/processed/accessibility.csv") %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO)) %>%
  select(CO_MUNICIPIO, bioma_fix = bioma, SG_UF_fix = SG_UF)

coverage <- coverage %>%
  left_join(acc, by = "CO_MUNICIPIO") %>%
  mutate(
    bioma = ifelse(is.na(bioma) | bioma == "", bioma_fix, bioma),
    SG_UF = ifelse(is.na(SG_UF) | SG_UF == "", SG_UF_fix, SG_UF)
  ) %>%
  select(-bioma_fix, -SG_UF_fix)

report_dims(coverage, "coverage")

# ============================================================
# DIAGNOSTICS
# ============================================================

cat("\n--- Coverage diagnostics ---\n")
report_indicators(coverage, c(
  "n_schools",
  "schools_per_10k",
  "enrollment_per_10k"
))

cat("\nCoverage category distribution:\n")
report_table(coverage, "coverage_cat")

cat("\nHas school distribution:\n")
report_table(coverage, "has_school")

cat("\nProfile by school presence:\n")
coverage %>%
  group_by(has_school) %>%
  summarise(
    n                  = n(),
    mean_pop           = round(mean(pop_mun, na.rm = TRUE)),
    mean_schools_10k   = round(mean(schools_per_10k,
                                    na.rm = TRUE), 2),
    mean_enrollment_10k = round(mean(enrollment_per_10k,
                                     na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% print()

cat("\nTop 10 best covered municipalities (schools per 10k):\n")
coverage %>%
  filter(has_school) %>%
  arrange(desc(schools_per_10k)) %>%
  select(CO_MUNICIPIO, SG_UF, pop_mun,
         n_schools, schools_per_10k) %>%
  head(10) %>%
  print()

cat("\nBiome distribution — municipalities without schools:\n")
coverage %>%
  filter(!has_school) %>%
  count(bioma, sort = TRUE) %>%
  print()

# ============================================================
# SAVE
# ============================================================

save_processed(coverage, PATH_OUT)

cat("\nScript 12 complete.\n")