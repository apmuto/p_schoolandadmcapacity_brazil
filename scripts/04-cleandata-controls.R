# ==================================================
# 04-cleandata-controls.R
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
# LOAD CESCOLA — SOURCE FOR COORDINATES AND URBAN SHARE
# ============================================================

cat("\nLoading cescola_media...\n")
cescola <- fread(PATH_CESCOLA, encoding = "Latin-1", sep = ";")
cat("Rows:", nrow(cescola), "\n")

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

# Convert to sf points (WGS84)
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
    n_schools   = n(),
    n_urban     = sum(TP_LOCALIZACAO == 1, na.rm = TRUE),
    urban_share = n_urban / n_schools,
    .groups = "drop"
  ) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

cat("Urban share computed for:", nrow(urban_share),
    "municipalities\n")

# ============================================================
# BIOME — SPATIAL JOIN POINTS TO BIOME POLYGONS
# ============================================================

cat("\nLoading and joining biomes...\n")
sf::sf_use_s2(FALSE)  # disable s2 — fixes most topology errors
biomas_raw <- st_read(PATH_BIOMAS, quiet = TRUE)
biomas     <- st_transform(biomas_raw, crs = 4326) %>%
              st_make_valid()

# Join each municipality point to the biome it falls in
mun_bioma <- st_join(mun_pts,
                     biomas %>% select(Bioma, CD_Bioma),
                     join = st_within,
                     left = TRUE)

# Fallback: nearest biome for any point outside all polygons
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

cat("\nBiome distribution:\n")
print(table(mun_bioma$Bioma, useNA = "always"))

# ============================================================
# DISTANCE TO STATE CAPITAL
# ============================================================

cat("\nComputing distance to state capital...\n")

# Convert capitals to sf points
capitals_sf <- st_as_sf(state_capitals,
                        coords = c("lon_cap", "lat_cap"),
                        crs = 4326)

# Project both to SIRGAS 2000 Brazil Polyconic for
# accurate distance in metres
mun_proj <- st_transform(mun_pts, crs = 5880)
cap_proj <- st_transform(capitals_sf, crs = 5880)

# For each municipality compute distance to its state capital
dist_km <- map_dbl(seq_len(nrow(mun_proj)), function(i) {
  uf  <- mun_proj$SG_UF[i]
  cap <- cap_proj %>% filter(SG_UF == uf)
  if (nrow(cap) == 0) return(NA_real_)
  as.numeric(st_distance(mun_proj[i, ], cap)) / 1000
})

cat("Distance calculation complete\n")
cat("Mean distance to capital:",
    round(mean(dist_km, na.rm = TRUE), 1), "km\n")
cat("Max distance to capital:",
    round(max(dist_km, na.rm = TRUE), 1), "km\n")

# ============================================================
# ASSEMBLE SPATIAL VARIABLES INTO CLEAN DATAFRAME
# ============================================================

spatial_vars <- mun_bioma %>%
  st_drop_geometry() %>%
  mutate(dist_capital_km = dist_km) %>%
  select(CO_MUNICIPIO, SG_UF, Bioma, CD_Bioma, dist_capital_km) %>%
  rename(bioma    = Bioma,
         cd_bioma = CD_Bioma)

sf::sf_use_s2(TRUE)  # restore default
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
         pop = Valor, pop_year = Ano) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

gdp_clean <- gdp_raw %>%
  select(CO_MUNICIPIO = `Município (Código)`,
         gdp_mil_reais = Valor, gdp_year = Ano) %>%
  mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO))

area_clean <- area_raw %>%
  select(CO_MUNICIPIO = `Município (Código)`,
         area_km2 = Valor) %>%
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
    gdp_pc           = (gdp_mil_reais * 1000) / pop,
    log_pop          = log(pop),
    log_gdp_pc       = log(gdp_pc),
    log_area         = log(area_km2),
    pop_density      = pop / area_km2,
    log_pop_density  = log(pop_density),
    log_dist_capital = log(dist_capital_km + 1)
  )

cat("Final rows:", nrow(controls_mun), "\n")

# ============================================================
# DIAGNOSTICS
# ============================================================

cat("\n--- Municipal controls diagnostics ---\n")
diag_vars <- c("log_pop", "log_gdp_pc", "log_area",
               "log_pop_density", "urban_share",
               "dist_capital_km", "log_dist_capital")

for (v in diag_vars) {
  vals <- controls_mun[[v]]
  cat(sprintf("%-22s mean=%8.3f | sd=%7.3f | NA=%d\n",
              v, mean(vals, na.rm = TRUE),
              sd(vals, na.rm = TRUE),
              sum(is.na(vals))))
}

cat("\nBiome distribution in final dataset:\n")
print(table(controls_mun$bioma, useNA = "always"))

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
  select(SG_UF = `Unidade da Federação (Código)`,
         pop = Valor, pop_year = Ano) %>%
  mutate(SG_UF = as.character(SG_UF)) %>%
  left_join(
    gdp_est %>%
      select(SG_UF = `Unidade da Federação (Código)`,
             gdp_mil_reais = Valor, gdp_year = Ano) %>%
      mutate(SG_UF = as.character(SG_UF)),
    by = "SG_UF"
  ) %>%
  left_join(
    area_est %>%
      select(SG_UF = `Unidade da Federação (Código)`,
             area_km2 = Valor) %>%
      mutate(SG_UF = as.character(SG_UF)),
    by = "SG_UF"
  ) %>%
  mutate(
    gdp_pc          = (gdp_mil_reais * 1000) / pop,
    log_pop         = log(pop),
    log_gdp_pc      = log(gdp_pc),
    log_area        = log(area_km2),
    pop_density     = pop / area_km2,
    log_pop_density = log(pop_density)
  )

cat("State controls built for:", nrow(controls_est), "states\n")

cat("\n--- State controls diagnostics ---\n")
for (v in c("log_pop", "log_gdp_pc",
            "log_area", "log_pop_density")) {
  vals <- controls_est[[v]]
  cat(sprintf("%-22s mean=%8.3f | sd=%7.3f | NA=%d\n",
              v, mean(vals, na.rm = TRUE),
              sd(vals, na.rm = TRUE),
              sum(is.na(vals))))
}

# ============================================================
# FINAL CHECKS
# ============================================================

cat("\n--- Final dimensions ---\n")
cat("controls_municipios:", nrow(controls_mun), "rows |",
    ncol(controls_mun), "cols\n")
cat("controls_estados:", nrow(controls_est), "rows |",
    ncol(controls_est), "cols\n")

# ============================================================
# SAVE
# ============================================================

fwrite(controls_mun, PATH_OUT_MUN, sep = ";", bom = TRUE)
cat("\nSaved controls_municipios to", PATH_OUT_MUN, "\n")

fwrite(controls_est, PATH_OUT_EST, sep = ";", bom = TRUE)
cat("Saved controls_estados to", PATH_OUT_EST, "\n")

cat("\nScript 04 complete.\n")