# ==================================================
# 04_cleandata_controls.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Build control variables for analysis.
#       Population, GDP per capita, area from SIDRA.
#       Urban share from cescola_media.
#       Biome from spatial join with IBGE shapefile.
#       Distance to state capital from school coords.
# Input:  IBGE SIDRA API (automatic download)
#         data/raw/biomas/lm_bioma_250.shp
#         data/processed/cescola_media.csv
# Output: data/processed/controls_municipios.csv
#         data/processed/controls_estados.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(sidrar)
library(data.table)
library(sf)

# ============================================================
# CONSTANTS
# ============================================================

PATH_CESCOLA <- "data/processed/cescola_media.csv"
PATH_BIOMAS  <- "data/raw/biomas/lm_bioma_250.shp"
PATH_OUT_MUN <- "data/processed/controls_municipios.csv"
PATH_OUT_EST <- "data/processed/controls_estados.csv"

# ============================================================
# STATE CAPITALS LOOKUP TABLE
# ============================================================

state_capitals <- tribble(
  ~SG_UF, ~capital,           ~lon_cap,  ~lat_cap,
  "AC", "Rio Branco",         -67.8070,  -9.9754,
  "AL", "Maceió",             -35.7353,  -9.6660,
  "AM", "Manaus",             -60.0212,  -3.1019,
  "AP", "Macapá",             -51.0694,   0.0349,
  "BA", "Salvador",           -38.5108, -12.9714,
  "CE", "Fortaleza",          -38.5434,  -3.7172,
  "DF", "Brasília",           -47.9292, -15.7801,
  "ES", "Vitória",            -40.3128, -20.3155,
  "GO", "Goiânia",            -49.2532, -16.6869,
  "MA", "São Luís",           -44.3028,  -2.5297,
  "MG", "Belo Horizonte",     -43.9378, -19.9208,
  "MS", "Campo Grande",       -54.6162, -20.4428,
  "MT", "Cuiabá",             -56.0974, -15.5961,
  "PA", "Belém",              -48.4902,  -1.4558,
  "PB", "João Pessoa",        -34.8631,  -7.1195,
  "PE", "Recife",             -34.8810,  -8.0539,
  "PI", "Teresina",           -42.8016,  -5.0892,
  "PR", "Curitiba",           -49.2731, -25.4278,
  "RJ", "Rio de Janeiro",     -43.1729, -22.9068,
  "RN", "Natal",              -35.2094,  -5.7793,
  "RO", "Porto Velho",        -63.9004,  -8.7612,
  "RR", "Boa Vista",          -60.6714,   2.8235,
  "RS", "Porto Alegre",       -51.2177, -30.0346,
  "SC", "Florianópolis",      -48.5482, -27.5954,
  "SE", "Aracaju",            -37.0731, -10.9472,
  "SP", "São Paulo",          -46.6333, -23.5505,
  "TO", "Palmas",             -48.3336, -10.2491
)

cat("State capitals defined:", nrow(state_capitals), "\n")

# ============================================================
# LOAD CESCOLA
# ============================================================

cat("\nLoading cescola_media...\n")
cescola <- load_br_csv(PATH_CESCOLA)
report_dims(cescola, "cescola_media")

# ============================================================
# MUNICIPALITY REPRESENTATIVE POINTS
# Use mean school coordinates as municipality center
# Appropriate since unit of analysis is schools
# ============================================================

cat("\nComputing municipality representative coordinates...\n")
mun_coords <- cescola %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  group_by(CO_MUNICIPIO, SG_UF) %>%
  summarise(
    lon = mean(LONGITUDE, na.rm = TRUE),
    lat = mean(LATITUDE,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

cat("Municipality coordinates computed for:",
    nrow(mun_coords), "municipalities\n")

mun_pts <- st_as_sf(mun_coords,
                    coords = c("lon", "lat"),
                    crs = 4326)

# ============================================================
# URBAN SHARE PER MUNICIPALITY
# ============================================================

cat("\nComputing urban share...\n")
urban_share <- cescola %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(
    n_schools        = n(),
    n_urban          = sum(TP_LOCALIZACAO == 1, na.rm = TRUE),
    urban_share_mun  = n_urban / n_schools,
    .groups = "drop"
  ) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

cat("Urban share computed for:", nrow(urban_share),
    "municipalities\n")

# ============================================================
# BIOME — SPATIAL JOIN POINTS TO BIOME POLYGONS
# ============================================================

cat("\nLoading and joining biomes...\n")
sf::sf_use_s2(FALSE)
biomas_raw <- st_read(PATH_BIOMAS, quiet = TRUE)
biomas     <- st_transform(biomas_raw, crs = 4326) %>%
              st_make_valid()

mun_bioma <- st_join(mun_pts,
                     biomas %>% select(Bioma, CD_Bioma),
                     join = st_within,
                     left = TRUE)

missing <- is.na(mun_bioma$Bioma)
cat("Municipalities outside biome polygons:", sum(missing), "\n")

if (sum(missing) > 0) {
  nearest <- st_join(mun_pts[missing, ],
                     biomas %>% select(Bioma, CD_Bioma),
                     join = st_nearest_feature)
  mun_bioma$Bioma[missing]    <- nearest$Bioma
  mun_bioma$CD_Bioma[missing] <- nearest$CD_Bioma
  cat("Filled using nearest biome\n")
}

report_table(mun_bioma %>% st_drop_geometry(), "Bioma", "Biome")

# ============================================================
# DISTANCE TO STATE CAPITAL
# ============================================================

cat("\nComputing distance to state capital...\n")

capitals_sf <- st_as_sf(state_capitals,
                        coords = c("lon_cap", "lat_cap"),
                        crs = 4326)

mun_proj <- st_transform(mun_pts, crs = 5880)
cap_proj <- st_transform(capitals_sf, crs = 5880)

dist_km <- map_dbl(seq_len(nrow(mun_proj)), function(i) {
  uf  <- mun_proj$SG_UF[i]
  cap <- cap_proj %>% filter(SG_UF == uf)
  if (nrow(cap) == 0) return(NA_real_)
  dist_km_sf(mun_proj[i, ], cap)
})

cat("Distance calculation complete\n")
cat("Mean distance to capital:",
    round(mean(dist_km, na.rm = TRUE), 1), "km\n")
cat("Max distance to capital:",
    round(max(dist_km, na.rm = TRUE), 1), "km\n")

sf::sf_use_s2(TRUE)

# ============================================================
# ASSEMBLE SPATIAL VARIABLES
# ============================================================

spatial_vars <- mun_bioma %>%
  st_drop_geometry() %>%
  mutate(
    dist_capital_km = dist_km,
    bioma    = remove_accents(Bioma),
    cd_bioma = CD_Bioma
  ) %>%
  select(CO_MUNICIPIO, SG_UF, bioma, cd_bioma, dist_capital_km)

# ============================================================
# PULL MUNICIPAL CONTROLS FROM SIDRA
# ============================================================

cat("\nPulling SIDRA data...\n")

pop_raw  <- get_sidra(6579, variable = 9324,
                      geo = "City", period = "last")
gdp_raw  <- get_sidra(5938, variable = 37,
                      geo = "City", period = "last")
area_raw <- get_sidra(1301, variable = 615,
                      geo = "City", period = "2010")

cat("Population rows:", nrow(pop_raw), "\n")
cat("GDP rows:", nrow(gdp_raw),
    "| Year:", unique(gdp_raw$Ano), "\n")
cat("Area rows:", nrow(area_raw), "\n")

pop_clean <- pop_raw %>%
  select(CO_MUNICIPIO = `Município (Código)`,
         pop_mun = Valor, pop_year = Ano) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

gdp_clean <- gdp_raw %>%
  select(CO_MUNICIPIO = `Município (Código)`,
         gdp_mil_reais_mun = Valor, gdp_year = Ano) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

area_clean <- area_raw %>%
  select(CO_MUNICIPIO = `Município (Código)`,
         area_km2_mun = Valor) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

# ============================================================
# MERGE ALL MUNICIPAL CONTROLS
# ============================================================

cat("\nMerging all municipal controls...\n")

controls_mun <- pop_clean %>%
  left_join(gdp_clean,    by = "CO_MUNICIPIO") %>%
  left_join(area_clean,   by = "CO_MUNICIPIO") %>%
  left_join(urban_share,  by = "CO_MUNICIPIO") %>%
  left_join(spatial_vars, by = "CO_MUNICIPIO") %>%
  mutate(
    gdp_pc_mun          = (gdp_mil_reais_mun * 1000) / pop_mun,
    log_pop_mun         = log(pop_mun),
    log_gdp_pc_mun      = log(gdp_pc_mun),
    log_area_mun        = log(area_km2_mun),
    pop_density_mun     = pop_mun / area_km2_mun,
    log_pop_density_mun = log(pop_density_mun),
    log_dist_capital    = log(dist_capital_km + 1)
  )

report_dims(controls_mun, "controls_municipios")

report_indicators(controls_mun, c(
  "log_pop_mun", "log_gdp_pc_mun", "log_area_mun",
  "log_pop_density_mun", "urban_share_mun",
  "dist_capital_km", "log_dist_capital"
))

report_table(controls_mun, "bioma", "Biome in final dataset")

# ============================================================
# STATE CONTROLS FROM SIDRA
# ============================================================

cat("\nPulling state controls from SIDRA...\n")

pop_est  <- get_sidra(6579, variable = 9324,
                      geo = "State", period = "last")
gdp_est  <- get_sidra(5938, variable = 37,
                      geo = "State", period = "last")
area_est <- get_sidra(1301, variable = 615,
                      geo = "State", period = "2010")

controls_est <- pop_est %>%
  select(SG_UF   = `Unidade da Federação (Código)`,
         pop_est = Valor, pop_year = Ano) %>%
  mutate(SG_UF = as.character(SG_UF)) %>%
  left_join(
    gdp_est %>%
      select(SG_UF = `Unidade da Federação (Código)`,
             gdp_mil_reais_est = Valor, gdp_year = Ano) %>%
      mutate(SG_UF = as.character(SG_UF)),
    by = "SG_UF"
  ) %>%
  left_join(
    area_est %>%
      select(SG_UF = `Unidade da Federação (Código)`,
             area_km2_est = Valor) %>%
      mutate(SG_UF = as.character(SG_UF)),
    by = "SG_UF"
  ) %>%
  mutate(
    # Fix numeric state codes to UF abbreviations
    SG_UF               = state_code_to_uf(SG_UF),
    gdp_pc_est          = (gdp_mil_reais_est * 1000) / pop_est,
    log_pop_est         = log(pop_est),
    log_gdp_pc_est      = log(gdp_pc_est),
    log_area_est        = log(area_km2_est),
    pop_density_est     = pop_est / area_km2_est,
    log_pop_density_est = log(pop_density_est)
  )

cat("State controls built for:", nrow(controls_est), "states\n")

report_indicators(controls_est, c(
  "log_pop_est", "log_gdp_pc_est",
  "log_area_est", "log_pop_density_est"
))

# ============================================================
# FINAL CHECKS
# ============================================================

report_dims(controls_mun, "controls_municipios")
report_dims(controls_est, "controls_estados")

# Verify state codes converted correctly
cat("\nState UF codes sample:\n")
print(head(controls_est$SG_UF, 10))

# ============================================================
# SAVE
# ============================================================

save_processed(controls_mun, PATH_OUT_MUN)
save_processed(controls_est, PATH_OUT_EST)

cat("\nScript 04 complete.\n")