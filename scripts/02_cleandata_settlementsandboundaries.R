# ==================================================
# 02-cleandata-settlementsandboundaries.R
# Goal: Load and prepare spatial data for analysis
#       - GADM boundaries (states + municipalities)
#       - GHSL SMOD 2020 raster (settlement classification)
#       - Extract settlement centroids from SMOD
#       - Save prepared spatial objects
# Input:  data/raw/gadm41_BRA.gpkg
#         data/raw/GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0.tif
# Output: data/processed/brazil_states.gpkg
#         data/processed/brazil_municipalities.gpkg
#         data/processed/settlement_centroids.gpkg
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

library(sf)
library(terra)
library(tidyverse)

# ============================================================
# CONSTANTS
# ============================================================

PATH_RAWGADM    <- "data/raw/gadm41_BRA.gpkg"
PATH_RAWSMOD    <- "data/raw/GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0.tif"
PATH_STATES      <- "data/processed/brazil_states.gpkg"
PATH_MUNIC       <- "data/processed/brazil_municipalities.gpkg"
PATH_STATEMUNIC  <- "data/processed/brazil_statemunicipalities.gpkg"
PATH_SETTLE_13 <- "data/processed/settlement_centroids_13.gpkg"
# SMOD threshold: keep cells classified as rural cluster or above
# SMOD classes: 10=water, 11=very low density, 12=low density rural,
#               13=rural cluster, 21=suburban, 22=semi-dense urban,
#               23=dense urban, 30=urban centre
# We define "settlement" as >= 13 (rural cluster and above)

# ============================================================
# LOAD GADM BOUNDARIES
# ============================================================

cat("Loading GADM boundaries...\n")

states <- st_read(PATH_RAWGADM, layer = "ADM_ADM_1", quiet = TRUE)
# cat("States loaded:", nrow(states), "\n")
cat("CRS:", st_crs(states)$Name, "\n")

municipalities <- st_read(PATH_RAWGADM, layer = "ADM_ADM_2", quiet = TRUE)
cat("Municipalities loaded:", nrow(municipalities), "\n")

# Check key columns
cat("\nState columns:", names(states), "\n")
cat("Municipality columns:", names(municipalities), "\n")

# ============================================================
# CLEAN GADM COLUMNS
# Keep only what we need for merging later
# ============================================================

states_clean <- states %>%
  select(
    GID_1,
    NAME_1,
    geom
  )

municipalities_clean <- municipalities %>%
  select(
    GID_2,
    NAME_2,
    geom
  )

statemunicipalities_clean <- municipalities %>%
  select(
    GID_1,       # state ID
    NAME_1,      # state name
    GID_2,       # municipality ID
    NAME_2,      # municipality name
    geom
  )

cat("\nStates cleaned:", nrow(states_clean), "features\n")
cat("Municipalities cleaned:", nrow(municipalities_clean), "features\n")
cat("State+municipalities cleaned:", nrow(statemunicipalities_clean), "features\n")
# ============================================================
# LOAD GHSL SMOD RASTER
# ============================================================

cat("\nLoading GHSL SMOD raster...\n")
smod <- rast(PATH_RAWSMOD)
cat("Raster CRS:", crs(smod, describe = TRUE)$name, "\n")
cat("Raster resolution:", res(smod), "\n")

cat("Raster extent:", as.character(ext(smod)), "\n")

# Check unique values in raster
cat("Unique SMOD values (sample):\n")
print(freq(smod))
# ============================================================
# ============================================================
# SELECT BRAZIL DATA
# Crop first in Mollweide, then reproject to WGS84
# Always crop before reproject — much faster and lighter
# ============================================================

cat("\nCropping to Brazil extent first (in Mollweide)...\n")
brazil <- st_read(PATH_RAWGADM, layer = "ADM_ADM_0", quiet = TRUE)
brazil_mol <- st_transform(brazil, crs = crs(smod))
brazil_vect <- vect(brazil_mol)
smod_brazil_mol <- crop(smod, brazil_vect, mask = TRUE)
cat("Crop done. Cells remaining:", ncell(smod_brazil_mol), "\n")

cat("\nReprojecting Brazil raster to WGS84...\n")
smod_brazil <- project(smod_brazil_mol, "EPSG:4326", method = "near")
cat("Reprojection done.\n")
cat("New CRS:", crs(smod_brazil, describe = TRUE)$name, "\n")
# ============================================================
# EXTRACT SETTLEMENT CENTROIDS
# NOTE: threshold 11 has 9M+ points — too large to convert
# to points in memory. Will work directly with the raster
# in script 05 instead.
# Only extracting threshold 13 (75,600 points — manageable)
# ============================================================

cat("\nExtracting settlement cells (SMOD >= 13)...\n")
smod_settle_13 <- smod_brazil >= 13
settlement_points_13 <- as.points(
  mask(smod_brazil, smod_settle_13, maskvalues = FALSE)
)
settlement_sf_13 <- st_as_sf(settlement_points_13)
names(settlement_sf_13)[1] <- "smod_class"

settlement_sf_13 <- settlement_sf_13 %>%
  mutate(
    smod_label = case_when(
      smod_class == 13 ~ "rural_cluster",
      smod_class == 21 ~ "suburban",
      smod_class == 22 ~ "semi_dense_urban",
      smod_class == 23 ~ "dense_urban",
      smod_class == 30 ~ "urban_centre",
      TRUE ~ "other"
    )
  )

cat("Settlement points (threshold 13):", nrow(settlement_sf_13), "\n")
cat("Distribution:\n")
print(table(settlement_sf_13$smod_label))

# Save cropped Brazil raster for threshold 11 analysis in script 05
# Will use raster directly instead of converting to points
writeRaster(smod_brazil, "data/processed/smod_brazil_wgs84.tif", 
            overwrite = TRUE)
cat("Saved Brazil SMOD raster for script 05\n")
# ============================================================
# SAVE OUTPUTS
# ============================================================
st_write(states_clean, PATH_STATES, delete_dsn = TRUE, quiet = TRUE)
cat("Saved states to", PATH_STATES, "\n")

st_write(municipalities_clean, PATH_MUNIC, delete_dsn = TRUE, quiet = TRUE)
cat("Saved municipalities to", PATH_MUNIC, "\n")

st_write(statemunicipalities_clean, PATH_STATEMUNIC, delete_dsn = TRUE, quiet = TRUE)
cat("Saved state+municipalities to", PATH_STATEMUNIC, "\n")

st_write(settlement_sf_13, PATH_SETTLE_13, delete_dsn = TRUE, quiet = TRUE)
cat("Saved settlement centroids (threshold 13) to", PATH_SETTLE_13, "\n")

cat("\nScript 02 complete.\n")