# ==============================================================================
# 33_descriptivemaps.R
# Project: Municipal administrative capacity and secondary school
#          accessibility in Brazil
#
# Goal: Produce 6 descriptive choropleth maps for AQM paper.
#   Map 1: Municipalities with no secondary school (binary)
#   Map 2: Mean settlement-to-school distance (main DV)
#   Map 3: Administrative capacity score (main IV)
#   Map 4: Population density
#   Map 5: State school share by municipality (from TP_DEPENDENCIA)
#   Map 6: FPM dependence (most robust predictor)
#
# Design conventions:
#   - No subtitles (explained in paper text)
#   - Municipality fill: light grey background
#   - Borders: color = "grey40", linewidth = 0.08
#   - Continuous maps: darker/warmer = worse outcome (consistent logic)
#   - Map 2 distance scale capped at 60km to suppress outliers
#
# Inputs:  data/processed/master_mun_main.csv
#          data/processed/brazil_municipalities.gpkg
#          data/processed/cescola_media_clean.csv
# Output:  outputs/figures/map_no_school.pdf
#          outputs/figures/map_mean_dist.pdf
#          outputs/figures/map_adm_capacity.pdf
#          outputs/figures/map_pop_density.pdf
#          outputs/figures/map_state_school_share.pdf
#          outputs/figures/map_fpm_dependence.pdf
#
# Author: Ana Paula Muto
# ==============================================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(sf)
library(ggplot2)

PATH_FIGS <- "outputs/figures/"
dir.create(PATH_FIGS, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading master dataset...\n")
d <- load_br_csv("data/processed/master_mun_main.csv") %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
report_dims(d, "master_mun_main")

cat("Loading municipality boundaries...\n")
munic <- st_read("data/processed/brazil_municipalities.gpkg", quiet = TRUE) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
cat("Boundaries loaded:", nrow(munic), "municipalities\n")

cat("Loading school-level data for TP_DEPENDENCIA...\n")
escola <- load_br_csv("data/processed/cescola_media_clean.csv") %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))
report_dims(escola, "cescola_media_clean")

# ==============================================================================
# BUILD STATE SCHOOL SHARE (TP_DEPENDENCIA = 2 is state)
# ==============================================================================

cat("Aggregating TP_DEPENDENCIA to municipality level...\n")
dep_mun <- escola %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(
    n_schools_total = n(),
    n_state         = sum(TP_DEPENDENCIA == 2, na.rm = TRUE),
    state_share     = n_state / n_schools_total,
    .groups = "drop"
  )
cat("Municipalities with dependency data:", nrow(dep_mun), "\n")

# ==============================================================================
# MERGE ALL TO SPATIAL
# ==============================================================================

map_df <- munic %>%
  left_join(d,       by = "CO_MUNICIPIO") %>%
  left_join(dep_mun, by = "CO_MUNICIPIO")

cat("Map dataset:", nrow(map_df), "municipalities\n")

# ==============================================================================
# SHARED THEME AND HELPERS
# ==============================================================================

# Border style: thin dark grey lines on light grey fill background
BORDER_COLOR    <- "grey40"
BORDER_WIDTH    <- 0.08
NA_FILL         <- "grey85"

map_theme <- theme_void(base_size = 11) +
  theme(
    plot.title        = element_text(face = "bold", size = 12,
                                     margin = margin(b = 6)),
    plot.caption      = element_text(color = "grey55", size = 7,
                                     margin = margin(t = 6)),
    legend.position   = "bottom",
    legend.title      = element_text(size = 8),
    legend.text       = element_text(size = 7),
    legend.key.width  = unit(1.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    plot.margin       = margin(6, 6, 6, 6)
  )

save_map <- function(p, filename, w = 7, h = 7) {
  path <- paste0(PATH_FIGS, filename)
  ggsave(path, p, width = w, height = h, device = "pdf")
  cat("Saved", path, "\n")
}

# ==============================================================================
# MAP 1: NO-SCHOOL MUNICIPALITIES
# ==============================================================================

cat("\nBuilding Map 1: No-school municipalities...\n")

p1 <- ggplot(map_df) +
  geom_sf(aes(fill = case_when(
    is.na(has_school)                        ~ "No data",
    has_school == FALSE | has_school == 0    ~ "No school",
    TRUE                                     ~ "Has school"
  )),
  color = BORDER_COLOR, linewidth = BORDER_WIDTH) +
  scale_fill_manual(
    values = c(
      "Has school" = "#d1e5f0",
      "No school"  = "#b2182b",
      "No data"    = NA_FILL
    ),
    name = NULL
  ) +
  labs(
    title   = "Secondary school presence by municipality",
    caption = "Source: Censo Escolar 2025 (INEP)"
  ) +
  map_theme

save_map(p1, "map_no_school.pdf")

# ==============================================================================
# MAP 2: MEAN DISTANCE (main DV)
# Scale capped at 60km — extreme outlier suppressed via squish
# Darker/warmer = worse accessibility (consistent with other maps)
# ==============================================================================

cat("\nBuilding Map 2: Mean distance to school...\n")

p2 <- ggplot(map_df) +
  geom_sf(aes(fill = mean_dist_km),
          color = BORDER_COLOR, linewidth = BORDER_WIDTH) +
  scale_fill_viridis_c(
    name      = "Mean distance (km)",
    option    = "magma",
    direction = -1,
    na.value  = NA_FILL,
    limits    = c(0, 60),
    oob       = scales::squish,
    breaks    = c(0, 10, 20, 40, 60),
    labels    = c("0", "10", "20", "40", "60+")
  ) +
  labs(
    title   = "Mean settlement-to-school distance by municipality",
    caption = "Source: Censo Escolar 2025 (INEP); GHS-SMOD 2020"
  ) +
  map_theme

save_map(p2, "map_mean_dist.pdf")

# ==============================================================================
# MAP 3: ADMINISTRATIVE CAPACITY SCORE (main IV)
# Switched to YlOrRd so darker = worse capacity (higher score = worse)
# consistent with Map 2 and Map 6 color logic
# ==============================================================================

cat("\nBuilding Map 3: Administrative capacity score...\n")

p3 <- ggplot(map_df) +
  geom_sf(aes(fill = adm_capacity_score_mun),
          color = BORDER_COLOR, linewidth = BORDER_WIDTH) +
  scale_fill_distiller(
    name      = "Capacity score",
    palette   = "RdYlBu",
    direction = 1,
    na.value  = NA_FILL,
    limits    = c(-2, 2),
    oob       = scales::squish
  ) +
  labs(
    title   = "Municipal administrative capacity score",
    caption = "Source: FINBRA 2024 (STN)"
  ) +
  map_theme

save_map(p3, "map_adm_capacity.pdf")

# ==============================================================================
# MAP 4: POPULATION DENSITY
# ==============================================================================

cat("\nBuilding Map 4: Population density...\n")

p4 <- ggplot(map_df) +
  geom_sf(aes(fill = log_pop_density_mun),
          color = BORDER_COLOR, linewidth = BORDER_WIDTH) +
  scale_fill_viridis_c(
    name      = "Log pop. density",
    option    = "cividis",
    direction = 1,
    na.value  = NA_FILL
  ) +
  labs(
    title   = "Population density by municipality (log scale)",
    caption = "Source: IBGE SIDRA"
  ) +
  map_theme

save_map(p4, "map_pop_density.pdf")

# ==============================================================================
# MAP 5: STATE SCHOOL SHARE (TP_DEPENDENCIA)
# ==============================================================================

cat("\nBuilding Map 5: State school share...\n")

p5 <- ggplot(map_df) +
  geom_sf(aes(fill = state_share),
          color = BORDER_COLOR, linewidth = BORDER_WIDTH) +
  scale_fill_distiller(
    name      = "Share of state schools",
    palette   = "PuOr",
    direction = -1,
    na.value  = NA_FILL,
    limits    = c(0, 1),
    labels    = scales::percent
  ) +
  labs(
    title   = "Share of state-administered secondary schools by municipality",
    caption = "Source: Censo Escolar 2025 (INEP)"
  ) +
  map_theme

save_map(p5, "map_state_school_share.pdf")

# ==============================================================================
# MAP 6: FPM DEPENDENCE
# YlOrRd: darker = more dependent = worse fiscal autonomy
# consistent with Map 2 (worse = darker)
# ==============================================================================

cat("\nBuilding Map 6: FPM dependence...\n")

p6 <- ggplot(map_df) +
  geom_sf(aes(fill = fpm_dependence_mun),
          color = BORDER_COLOR, linewidth = BORDER_WIDTH) +
  scale_fill_distiller(
    name      = "FPM / total revenue",
    palette   = "YlOrRd",
    direction = 1,
    na.value  = NA_FILL,
    limits    = c(0, 0.7),
    oob       = scales::squish,
    labels    = scales::percent
  ) +
  labs(
    title   = "Municipal FPM transfer dependence",
    caption = "Source: FINBRA 2024 (STN)"
  ) +
  map_theme

save_map(p6, "map_fpm_dependence.pdf")
# ==============================================================================
# MAP 7: FISCAL AUTONOMY (M1 baseline)
# ==============================================================================

cat("\nBuilding Map: Fiscal autonomy...\n")

p_fiscal <- ggplot(map_df) +
  geom_sf(aes(fill = fiscal_autonomy_mun),
          color = BORDER_COLOR, linewidth = BORDER_WIDTH) +
  scale_fill_distiller(
    name      = "Fiscal autonomy\n(own rev / total rev)",
    palette   = "RdYlBu",
    direction = 1,
    na.value  = NA_FILL,
    limits    = c(0, 0.5),
    oob       = scales::squish,
    labels    = scales::percent
  ) +
  labs(
    title   = "Municipal fiscal autonomy",
    caption = "Source: FINBRA 2024 (STN)"
  ) +
  map_theme

save_map(p_fiscal, "map_fiscal_autonomy.pdf")
# ==============================================================================
# DONE
# ==============================================================================

cat("\n============================================================\n")
cat("Script 33 complete. Maps saved to", PATH_FIGS, "\n")
cat("  map_no_school.pdf\n")
cat("  map_mean_dist.pdf\n")
cat("  map_adm_capacity.pdf\n")
cat("  map_pop_density.pdf\n")
cat("  map_state_school_share.pdf\n")
cat("  map_fpm_dependence.pdf\n")
cat("============================================================\n")