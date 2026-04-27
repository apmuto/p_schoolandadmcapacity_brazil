# ==================================================
# 02_cleandata_settlementsandboundaries.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Load and prepare spatial data for analysis
#       - geobr boundaries (states + municipalities)
#         with official IBGE codes
#       - GHSL SMOD 2020 raster (settlement classes)
#       - Extract settlement centroids from SMOD
#       - Save prepared spatial objects
# Input:  geobr API (automatic download)
#         data/raw/GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0.tif
# Output: data/processed/brazil_states.gpkg
#         data/processed/brazil_municipalities.gpkg
#         data/processed/settlement_centroids_13.gpkg
#         data/processed/smod_brazil_wgs84.tif
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(sf)
library(terra)
library(tidyverse)
library(geobr)

# ============================================================
# CONSTANTS
# ============================================================

PATH_RAWSMOD    <- "data/raw/GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0.tif"
PATH_STATES     <- "data/processed/brazil_states.gpkg"
PATH_MUNIC      <- "data/processed/brazil_municipalities.gpkg"
PATH_SETTLE_13  <- "data/processed/settlement_centroids_13.gpkg"
PATH_SMOD_OUT   <- "data/processed/smod_brazil_wgs84.tif"

# SMOD classification thresholds:
# 10=water, 11=very low density, 12=low density rural
# 13=rural cluster, 21=suburban, 22=semi-dense urban
# 23=dense urban, 30=urban centre
# Settlement defined as >= 13 (rural cluster and above)

# ============================================================
# LOAD BOUNDARIES VIA GEOBR
# Official IBGE boundaries with municipality codes
# ============================================================

cat("Loading state boundaries from geobr...\n")
states_raw <- read_state(year = 2020,
                         simplified = TRUE,
                         showProgress = FALSE)
cat("States loaded:", nrow(states_raw), "\n")
cat("CRS:", st_crs(states_raw)$Name, "\n")

cat("\nLoading municipality boundaries from geobr...\n")
municipalities_raw <- read_municipality(year = 2020,
                                        simplified = TRUE,
                                        showProgress = FALSE)
cat("Municipalities loaded:", nrow(municipalities_raw), "\n")

# ============================================================
# CLEAN AND STANDARDIZE COLUMNS
# Rename to match project naming conventions
# ============================================================

states_clean <- states_raw %>%
  select(
    code_state,
    SG_UF      = abbrev_state,
    name_state,
    code_region,
    name_region,
    geom
  ) %>%
  mutate(name_state  = remove_accents(name_state),
         name_region = remove_accents(name_region))

municipalities_clean <- municipalities_raw %>%
  select(
    CO_MUNICIPIO = code_muni,
    NO_MUNICIPIO = name_muni,
    code_state,
    SG_UF        = abbrev_state,
    name_state,
    code_region,
    name_region,
    geom
  ) %>%
  mutate(
    CO_MUNICIPIO = as.character(CO_MUNICIPIO),
    NO_MUNICIPIO = remove_accents(NO_MUNICIPIO),
    name_state   = remove_accents(name_state),
    name_region  = remove_accents(name_region)
  )

cat("\nStates cleaned:", nrow(states_clean), "features\n")
cat("Municipalities cleaned:", nrow(municipalities_clean), "features\n")
cat("Municipality columns:", names(municipalities_clean), "\n")

# Verify CO_MUNICIPIO format
cat("CO_MUNICIPIO sample:", head(municipalities_clean$CO_MUNICIPIO), "\n")

# ============================================================
# LOAD GHSL SMOD RASTER
# ============================================================

cat("\nLoading GHSL SMOD raster...\n")
smod <- rast(PATH_RAWSMOD)
cat("Raster CRS:", crs(smod, describe = TRUE)$name, "\n")
cat("Raster resolution:", res(smod), "\n")
cat("Raster extent:", as.character(ext(smod)), "\n")

cat("Unique SMOD values (sample):\n")
print(freq(smod))

# ============================================================
# CROP RASTER TO BRAZIL
# Crop in native Mollweide first — much faster
# Then reproject to WGS84 for consistency with other layers
# ============================================================

cat("\nCropping to Brazil extent (in Mollweide)...\n")

# Use geobr Brazil boundary for crop
brazil_raw <- read_country(year = 2020,
                           simplified = TRUE,
                           showProgress = FALSE)
brazil_mol  <- st_transform(brazil_raw, crs = crs(smod))
brazil_vect <- vect(brazil_mol)

smod_brazil_mol <- crop(smod, brazil_vect, mask = TRUE)
cat("Crop done. Cells remaining:", ncell(smod_brazil_mol), "\n")

cat("\nReprojecting to WGS84...\n")
smod_brazil <- project(smod_brazil_mol, "EPSG:4326", method = "near")
cat("Reprojection done.\n")
cat("New CRS:", crs(smod_brazil, describe = TRUE)$name, "\n")

# Save raster for script 05
writeRaster(smod_brazil, PATH_SMOD_OUT, overwrite = TRUE)
cat("Saved Brazil SMOD raster to", PATH_SMOD_OUT, "\n")

# ============================================================
# EXTRACT SETTLEMENT CENTROIDS (THRESHOLD >= 13)
# threshold 13 = rural cluster and above
# 75,600 points — manageable in memory
# threshold 11 has 9M+ points — use raster directly in script 05
# ============================================================

cat("\nExtracting settlement centroids (SMOD >= 13)...\n")

smod_settle_13     <- smod_brazil >= 13
settlement_pts_13  <- as.points(
  mask(smod_brazil, smod_settle_13, maskvalues = FALSE)
)
settlement_sf_13 <- st_as_sf(settlement_pts_13)
names(settlement_sf_13)[1] <- "smod_class"

settlement_sf_13 <- settlement_sf_13 %>%
  mutate(
    smod_label = case_when(
      smod_class == 13 ~ "rural_cluster",
      smod_class == 21 ~ "suburban",
      smod_class == 22 ~ "semi_dense_urban",
      smod_class == 23 ~ "dense_urban",
      smod_class == 30 ~ "urban_centre",
      TRUE             ~ "other"
    )
  )

cat("Settlement points (threshold 13):", nrow(settlement_sf_13), "\n")
cat("Distribution:\n")
print(table(settlement_sf_13$smod_label))

# ============================================================
# SAVE ALL OUTPUTS
# ============================================================

st_write(states_clean, PATH_STATES,
         delete_dsn = TRUE, quiet = TRUE)
cat("\nSaved states to", PATH_STATES, "\n")

st_write(municipalities_clean, PATH_MUNIC,
         delete_dsn = TRUE, quiet = TRUE)
cat("Saved municipalities to", PATH_MUNIC, "\n")

st_write(settlement_sf_13, PATH_SETTLE_13,
         delete_dsn = TRUE, quiet = TRUE)
cat("Saved settlement centroids to", PATH_SETTLE_13, "\n")

cat("\nScript 02 complete.\n")