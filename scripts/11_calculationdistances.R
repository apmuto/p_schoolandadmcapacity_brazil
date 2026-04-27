# ==================================================
# 11_calculationdistances.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Compute two accessibility measures per
#       municipality:
#       1. distance_to_school_km: distance from
#          municipality centroid to nearest
#          secondary school
#       2. Saved for use in script 11 master merge
# Input:  data/processed/cescola_media_clean.csv
#         data/processed/brazil_municipalities.gpkg
#         data/processed/settlement_centroids_13.gpkg
#         data/processed/controls_municipios.csv
# Output: data/processed/accessibility.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(sf)

# ============================================================
# CONSTANTS
# ============================================================

PATH_ESCOLA     <- "data/processed/cescola_media_clean.csv"
PATH_MUNIC      <- "data/processed/brazil_municipalities.gpkg"
PATH_SETTLE     <- "data/processed/settlement_centroids_13.gpkg"
PATH_CTRL       <- "data/processed/controls_municipios.csv"
PATH_OUT        <- "data/processed/accessibility.csv"

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading escola...\n")
escola <- load_br_csv(PATH_ESCOLA)
report_dims(escola, "escola")

cat("Loading municipality boundaries...\n")
munic <- st_read(PATH_MUNIC, quiet = TRUE)
report_dims(munic %>% st_drop_geometry(), "municipalities")

cat("Loading settlement centroids...\n")
settlements <- st_read(PATH_SETTLE, quiet = TRUE)
report_dims(settlements %>% st_drop_geometry(), "settlements")

cat("Loading controls...\n")
ctrl <- load_br_csv(PATH_CTRL) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

# ============================================================
# BUILD SCHOOL POINTS
# Convert school locations to sf
# ============================================================

cat("\nBuilding school points...\n")

school_pts <- escola %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

cat("School points:", nrow(school_pts), "\n")

# ============================================================
# BUILD MUNICIPALITY CENTROIDS
# Two sources:
# 1. Municipalities WITH schools: mean school coordinates
# 2. Municipalities WITHOUT schools: geobr polygon centroid
# ============================================================

cat("\nBuilding municipality centroids...\n")

# Municipalities with schools — use mean school coordinates
mun_with_schools <- escola %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO)) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(
    lon        = mean(LONGITUDE, na.rm = TRUE),
    lat        = mean(LATITUDE,  na.rm = TRUE),
    n_schools  = n(),
    .groups    = "drop"
  ) %>%
  mutate(has_school = TRUE)

cat("Municipalities with schools:", nrow(mun_with_schools), "\n")

# Municipalities without schools — use geobr polygon centroid
mun_no_school_codes <- ctrl %>%
  filter(!CO_MUNICIPIO %in% mun_with_schools$CO_MUNICIPIO) %>%
  pull(CO_MUNICIPIO)

cat("Municipalities without schools:", length(mun_no_school_codes), "\n")

# Get centroids from geobr polygons
sf::sf_use_s2(FALSE)

mun_no_school_pts <- munic %>%
  filter(CO_MUNICIPIO %in% mun_no_school_codes) %>%
  st_centroid() %>%
  mutate(
    lon       = st_coordinates(.)[, 1],
    lat       = st_coordinates(.)[, 2],
    n_schools = 0L,
    has_school = FALSE
  ) %>%
  st_drop_geometry() %>%
  select(CO_MUNICIPIO, lon, lat, n_schools, has_school)

cat("Centroids found for municipalities without schools:",
    nrow(mun_no_school_pts), "\n")

# Combine all municipality centroids
all_mun_pts <- bind_rows(
  mun_with_schools %>%
    select(CO_MUNICIPIO, lon, lat, n_schools, has_school),
  mun_no_school_pts
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cat("Total municipality centroids:", nrow(all_mun_pts), "\n")

# ============================================================
# PROJECT TO SIRGAS 2000 FOR ACCURATE DISTANCES
# ============================================================

cat("\nProjecting to SIRGAS 2000 (EPSG:5880)...\n")

mun_proj    <- st_transform(all_mun_pts,  crs = 5880)
school_proj <- st_transform(school_pts,   crs = 5880)

# ============================================================
# COMPUTE DISTANCE FROM EACH MUNICIPALITY
# TO ITS NEAREST SECONDARY SCHOOL
# ============================================================

cat("\nComputing distance to nearest school for each municipality...\n")
cat("(This may take a few minutes...)\n")

# Find index of nearest school for each municipality
nearest_idx <- st_nearest_feature(mun_proj, school_proj)

# Compute distance to that nearest school
dist_to_school <- map_dbl(
  seq_len(nrow(mun_proj)),
  function(i) {
    as.numeric(st_distance(
      mun_proj[i, ],
      school_proj[nearest_idx[i], ]
    )) / 1000  # convert to km
  }
)

cat("Distance calculation complete\n")

# ============================================================
# ASSEMBLE ACCESSIBILITY DATASET
# ============================================================

cat("\nAssembling accessibility dataset...\n")

accessibility <- all_mun_pts %>%
  st_drop_geometry() %>%
  mutate(
    dist_nearest_school_km = dist_to_school,
    log_dist_school        = log(dist_nearest_school_km + 1),
    access_cat = case_when(
      dist_nearest_school_km == 0          ~ "In municipality",
      dist_nearest_school_km <= 10         ~ "Within 10km",
      dist_nearest_school_km <= 30         ~ "10-30km",
      dist_nearest_school_km <= 60         ~ "30-60km",
      dist_nearest_school_km <= 100        ~ "60-100km",
      TRUE                                  ~ "Over 100km"
    )
  ) %>%
  left_join(
    ctrl %>%
      select(CO_MUNICIPIO, SG_UF, pop_mun, gdp_pc_mun,
             log_pop_mun, log_gdp_pc_mun, bioma,
             dist_capital_km, log_dist_capital,
             urban_share_mun),
    by = "CO_MUNICIPIO"
  )

sf::sf_use_s2(TRUE)
# ============================================================
# FIX MISSING BIOME AND SG_UF FOR MUNICIPALITIES WITHOUT SCHOOLS
# These municipalities had no school coordinates so script 04
# could not assign biome or SG_UF — use geobr boundaries instead
# ============================================================

cat("\nFixing missing biome and SG_UF for municipalities without schools...\n")

library(geobr)
sf::sf_use_s2(FALSE)

# Load biomes
biomas_raw <- st_read("data/raw/biomas/lm_bioma_250.shp", quiet = TRUE)
biomas     <- st_transform(biomas_raw, crs = 4326) %>%
              st_make_valid()

# Get centroids for municipalities missing biome
missing_codes <- accessibility %>%
  filter(is.na(bioma) | bioma == "") %>%
  pull(CO_MUNICIPIO)

cat("Municipalities missing biome:", length(missing_codes), "\n")

# Get centroids for municipalities missing biome
mun_missing <- munic %>%
  filter(CO_MUNICIPIO %in% missing_codes) %>%
  st_transform(crs = 4326) %>%  # add this line
  st_centroid()

# Spatial join to biomes
mun_missing_biome <- st_join(
  mun_missing,
  biomas %>% select(Bioma),
  join = st_within,
  left = TRUE
)

# Fallback nearest for any still missing
still_missing <- is.na(mun_missing_biome$Bioma)
if (sum(still_missing) > 0) {
  nearest_biome <- st_join(
    mun_missing[still_missing, ],
    biomas %>% select(Bioma),
    join = st_nearest_feature
  )
  mun_missing_biome$Bioma[still_missing] <- nearest_biome$Bioma
}

# Build lookup table
biome_lookup <- mun_missing_biome %>%
  st_drop_geometry() %>%
  select(CO_MUNICIPIO, 
         bioma_fix = Bioma,
         SG_UF_fix = SG_UF) %>%
  mutate(bioma_fix = remove_accents(bioma_fix))

# Apply fixes to accessibility dataset
accessibility <- accessibility %>%
  left_join(biome_lookup, by = "CO_MUNICIPIO") %>%
  mutate(
    bioma = ifelse(is.na(bioma) | bioma == "", bioma_fix, bioma),
    SG_UF = ifelse(is.na(SG_UF) | SG_UF == "", SG_UF_fix, SG_UF)
  ) %>%
  select(-bioma_fix, -SG_UF_fix)

sf::sf_use_s2(TRUE)

cat("Remaining missing biome after fix:",
    sum(is.na(accessibility$bioma) | accessibility$bioma == ""), "\n")
cat("Remaining missing SG_UF after fix:",
    sum(is.na(accessibility$SG_UF) | accessibility$SG_UF == ""), "\n")

report_dims(accessibility, "accessibility")

# ============================================================
# DIAGNOSTICS
# ============================================================

cat("\n--- Distance to nearest school diagnostics ---\n")
report_indicators(accessibility, c(
  "dist_nearest_school_km",
  "log_dist_school"
))

cat("\nAccess category distribution:\n")
print(table(accessibility$access_cat, useNA = "always"))

cat("\nMunicipalities with dist = 0 (has school):",
    sum(accessibility$dist_nearest_school_km == 0), "\n")
cat("Municipalities with dist > 0 (no school):",
    sum(accessibility$dist_nearest_school_km > 0), "\n")

cat("\nProfile by school presence:\n")
accessibility %>%
  group_by(has_school) %>%
  summarise(
    n                      = n(),
    mean_dist_school_km    = round(mean(dist_nearest_school_km,
                                        na.rm = TRUE), 1),
    max_dist_school_km     = round(max(dist_nearest_school_km,
                                       na.rm = TRUE), 1),
    mean_pop               = round(mean(pop_mun, na.rm = TRUE)),
    mean_gdp_pc            = round(mean(gdp_pc_mun, na.rm = TRUE)),
    .groups = "drop"
  ) %>% print()

cat("\nTop 10 most inaccessible municipalities:\n")
accessibility %>%
  filter(!has_school) %>%
  arrange(desc(dist_nearest_school_km)) %>%
  select(CO_MUNICIPIO, SG_UF, bioma,
         dist_nearest_school_km, pop_mun) %>%
  head(10) %>%
  print()

# ============================================================
# SAVE
# ============================================================

save_processed(accessibility, PATH_OUT)

cat("\nScript 05 complete.\n")