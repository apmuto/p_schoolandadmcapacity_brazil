# ==================================================
# 11_calculationdistances.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Compute travel distance from each settlement
#       centroid to nearest secondary school.
#       Unit: GHSL settlement points (76,265)
#       Aggregated to municipality level.
# Outcome variables:
#   mean_dist_to_school_km  — avg travel distance
#   median_dist_to_school_km — median travel distance
#   max_dist_to_school_km   — worst case distance
#   pct_over_10km           — % settlements >10km
#   pct_over_30km           — % settlements >30km
#   pct_over_50km           — % settlements >50km
#   n_settlements           — settlement count
# Input:  data/processed/settlement_centroids_13.gpkg
#         data/processed/brazil_municipalities.gpkg
#         data/processed/cescola_media_clean.csv
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

PATH_SETTLE  <- "data/processed/settlement_centroids_13.gpkg"
PATH_MUNIC   <- "data/processed/brazil_municipalities.gpkg"
PATH_ESCOLA  <- "data/processed/cescola_media_clean.csv"
PATH_CTRL    <- "data/processed/controls_municipios.csv"
PATH_BIOMAS  <- "data/raw/biomas/lm_bioma_250.shp"
PATH_OUT     <- "data/processed/accessibility.csv"

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading settlements...\n")
settlements <- st_read(PATH_SETTLE, quiet = TRUE)
report_dims(settlements %>% st_drop_geometry(), "settlements")

cat("Loading municipality boundaries...\n")
munic <- st_read(PATH_MUNIC, quiet = TRUE)
report_dims(munic %>% st_drop_geometry(), "municipalities")

cat("Loading escola...\n")
escola <- load_br_csv(PATH_ESCOLA) %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE))
report_dims(escola, "escola")

cat("Loading controls...\n")
ctrl <- load_br_csv(PATH_CTRL) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

# ============================================================
# PROJECT TO SIRGAS 2000 FOR ACCURATE DISTANCES
# ============================================================

cat("\nProjecting to SIRGAS 2000 (EPSG:5880)...\n")
sf::sf_use_s2(FALSE)

settle_proj <- st_transform(settlements, crs = 5880)
munic_proj  <- st_transform(munic,       crs = 5880)

# ============================================================
# STEP 1: ASSIGN MUNICIPALITY TO EACH SETTLEMENT
# Spatial join settlement points to municipality polygons
# ============================================================

cat("\nJoining settlements to municipalities...\n")
t1 <- proc.time()

settle_mun <- st_join(
  settle_proj,
  munic_proj %>% select(CO_MUNICIPIO),
  join = st_within,
  left = TRUE
)

t2 <- proc.time()
cat("Join complete in", round((t2-t1)["elapsed"], 1), "seconds\n")
cat("Settlements with municipality:", 
    sum(!is.na(settle_mun$CO_MUNICIPIO)), "\n")
cat("Settlements outside municipality polygons:",
    sum(is.na(settle_mun$CO_MUNICIPIO)), "\n")

# Fallback: nearest municipality for settlements outside polygons
missing_mun <- is.na(settle_mun$CO_MUNICIPIO)
if (sum(missing_mun) > 0) {
  cat("Fixing", sum(missing_mun), 
      "settlements outside polygons...\n")
  nearest_mun <- st_join(
    settle_proj[missing_mun, ],
    munic_proj %>% select(CO_MUNICIPIO),
    join = st_nearest_feature
  )
  settle_mun$CO_MUNICIPIO[missing_mun] <- 
    nearest_mun$CO_MUNICIPIO
  cat("All settlements assigned to municipality\n")
}

# ============================================================
# STEP 2: BUILD SCHOOL POINTS
# ============================================================

cat("\nBuilding school points...\n")

school_proj <- escola %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = 5880)

cat("School points:", nrow(school_proj), "\n")

# ============================================================
# STEP 3: DISTANCE FROM EACH SETTLEMENT TO NEAREST SCHOOL
# ============================================================

cat("\nComputing distance from each settlement to nearest school...\n")
t3 <- proc.time()

# Find nearest school index for each settlement
nearest_idx <- st_nearest_feature(settle_mun, school_proj)

# FAST - vectorized version
dist_km <- as.numeric(
  st_distance(
    settle_mun,
    school_proj[nearest_idx, ],
    by_element = TRUE
  )
) / 1000

t4 <- proc.time()
cat("Distance computation complete in",
    round((t4-t3)["elapsed"], 1), "seconds\n")

cat("Mean distance to nearest school:",
    round(mean(dist_km, na.rm = TRUE), 2), "km\n")
cat("Max distance to nearest school:",
    round(max(dist_km, na.rm = TRUE), 1), "km\n")
cat("Settlements >10km from school:",
    sum(dist_km > 10, na.rm = TRUE), "\n")
cat("Settlements >30km from school:",
    sum(dist_km > 30, na.rm = TRUE), "\n")
cat("Settlements >50km from school:",
    sum(dist_km > 50, na.rm = TRUE), "\n")

# ============================================================
# STEP 4: ATTACH DISTANCES TO SETTLEMENT DATA
# ============================================================

settle_dist <- settle_mun %>%
  st_drop_geometry() %>%
  mutate(
    dist_to_school_km = dist_km,
    CO_MUNICIPIO      = as.character(CO_MUNICIPIO)
  )

# ============================================================
# STEP 5: AGGREGATE TO MUNICIPALITY LEVEL
# ============================================================

cat("\nAggregating to municipality level...\n")

accessibility <- settle_dist %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(
    n_settlements          = n(),
    mean_dist_school_km    = mean(dist_to_school_km,
                                   na.rm = TRUE),
    median_dist_school_km  = median(dist_to_school_km,
                                     na.rm = TRUE),
    max_dist_school_km     = max(dist_to_school_km,
                                  na.rm = TRUE),
    pct_over_10km          = mean(dist_to_school_km > 10,
                                   na.rm = TRUE),
    pct_over_30km          = mean(dist_to_school_km > 30,
                                   na.rm = TRUE),
    pct_over_50km          = mean(dist_to_school_km > 50,
                                   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_mean_dist_school   = log(mean_dist_school_km + 1),
    log_median_dist_school = log(median_dist_school_km + 1),
    log_max_dist_school    = log(max_dist_school_km + 1)
  )

cat("Municipalities with settlement data:",
    nrow(accessibility), "\n")

# ============================================================
# MERGE WITH CONTROLS AND FIX BIOME/SG_UF
# ============================================================

cat("\nMerging with controls...\n")

# Load and fix biomes for municipalities without schools
biomas_raw <- st_read(PATH_BIOMAS, quiet = TRUE)
biomas     <- st_transform(biomas_raw, crs = 4326) %>%
              st_make_valid()

# Get SG_UF from municipality boundaries
mun_attrs <- munic %>%
  st_drop_geometry() %>%
  select(CO_MUNICIPIO, SG_UF)

accessibility <- accessibility %>%
  left_join(mun_attrs, by = "CO_MUNICIPIO") %>%
  left_join(
    ctrl %>%
      select(CO_MUNICIPIO, pop_mun, gdp_pc_mun,
             log_pop_mun, log_gdp_pc_mun,
             bioma, dist_capital_km, log_dist_capital,
             urban_share_mun),
    by = "CO_MUNICIPIO"
  )

# Fix missing biome using spatial join
missing_biome <- is.na(accessibility$bioma) | 
                 accessibility$bioma == ""

if (sum(missing_biome) > 0) {
  cat("Fixing", sum(missing_biome), "missing biomes...\n")
  
  # Get centroids for municipalities missing biome
  missing_codes <- accessibility$CO_MUNICIPIO[missing_biome]
  
  mun_missing <- munic %>%
    filter(CO_MUNICIPIO %in% missing_codes) %>%
    st_transform(crs = 4326) %>%
    st_centroid()
  
  biomas_wgs84 <- st_transform(biomas_raw, crs = 4326) %>%
                  st_make_valid()
  
  mun_biome_fix <- st_join(
    mun_missing,
    biomas_wgs84 %>% select(Bioma),
    join = st_within,
    left = TRUE
  )
  
  # Nearest fallback
  still_missing <- is.na(mun_biome_fix$Bioma)
  if (sum(still_missing) > 0) {
    nearest <- st_join(
      mun_missing[still_missing, ],
      biomas_wgs84 %>% select(Bioma),
      join = st_nearest_feature
    )
    mun_biome_fix$Bioma[still_missing] <- nearest$Bioma
  }
  
  biome_lookup <- mun_biome_fix %>%
    st_drop_geometry() %>%
    select(CO_MUNICIPIO, bioma_fix = Bioma) %>%
    mutate(bioma_fix = remove_accents(bioma_fix))
  
  accessibility <- accessibility %>%
    left_join(biome_lookup, by = "CO_MUNICIPIO") %>%
    mutate(bioma = ifelse(is.na(bioma) | bioma == "",
                          bioma_fix, bioma)) %>%
    select(-bioma_fix)
  
  cat("Remaining missing biome:",
      sum(is.na(accessibility$bioma) | 
          accessibility$bioma == ""), "\n")
}

# Check coverage
cat("\nMunicipalities in accessibility:",
    nrow(accessibility), "\n")
cat("Missing SG_UF:",
    sum(is.na(accessibility$SG_UF)), "\n")
cat("Missing bioma:",
    sum(is.na(accessibility$bioma) |
        accessibility$bioma == ""), "\n")

sf::sf_use_s2(TRUE)

# ============================================================
# DIAGNOSTICS
# ============================================================

report_dims(accessibility, "accessibility")

cat("\n--- Distance diagnostics ---\n")
report_indicators(accessibility, c(
  "mean_dist_school_km",
  "median_dist_school_km",
  "max_dist_school_km",
  "log_mean_dist_school",
  "pct_over_10km",
  "pct_over_30km",
  "pct_over_50km"
))

cat("\nTop 10 most inaccessible municipalities:\n")
accessibility %>%
  arrange(desc(mean_dist_school_km)) %>%
  select(CO_MUNICIPIO, SG_UF, bioma,
         mean_dist_school_km, max_dist_school_km,
         n_settlements, pop_mun) %>%
  head(10) %>%
  print()

cat("\nDistribution of mean distance:\n")
accessibility %>%
  mutate(dist_cat = case_when(
    mean_dist_school_km <= 5   ~ "0-5km",
    mean_dist_school_km <= 10  ~ "5-10km",
    mean_dist_school_km <= 20  ~ "10-20km",
    mean_dist_school_km <= 30  ~ "20-30km",
    mean_dist_school_km <= 50  ~ "30-50km",
    TRUE                        ~ "50km+"
  )) %>%
  count(dist_cat) %>%
  print()

# ============================================================
# SAVE
# ============================================================

save_processed(accessibility, PATH_OUT)

cat("\nScript 11 complete.\n")