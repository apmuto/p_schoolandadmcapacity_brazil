
# ==================================================
#  01-cleandata-cescola.R
# Project:Municipal administrative capacity and secondary school accessibility in Brazil
#  Goal: Load, merge, filter and select variables
#       from Censo Escolar 2025
# Input:  data/raw/Tabela_Escola_2025.csv
#         data/raw/Tabela_Matricula_2025.csv
# Output: data/processed/cescola_media.csv
#         data/processed/cescola_media_clean.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================
# Set up
library(tidyverse)
library(data.table)

# ============================================================
# CONSTANTS
# ============================================================

PATH_RAWESCOLA    <- "data/raw/Tabela_Escola_2025.csv"
PATH_RAWMATRICULA <- "data/raw/Tabela_Matricula_2025.csv"
PATH_OUT_MEDIA <- "data/processed/cescola_media.csv"
PATH_OUT_CLEAN <- "data/processed/cescola_media_clean.csv"
# ============================================================
# LOAD RAW DATA
# ============================================================

cat("Loading escola data...\n")
escola_raw <- fread(PATH_RAWESCOLA, encoding = "Latin-1", sep = ";")
cat("Escola rows:", nrow(escola_raw), "| Cols:", ncol(escola_raw), "\n")

cat("Loading matricula data...\n")
mat_raw <- fread(PATH_RAWMATRICULA, encoding = "Latin-1", sep = ";")
cat("Matricula rows:", nrow(mat_raw), "| Cols:", ncol(mat_raw), "\n")

# Sanity check: confirm merge key exists in both
stopifnot("CO_ENTIDADE" %in% names(escola_raw))
stopifnot("CO_ENTIDADE" %in% names(mat_raw))
cat("Merge key CO_ENTIDADE confirmed in both tables.\n")

# ============================================================
# SELECT COLUMNS FROM MATRICULA
# Only keep enrollment counts relevant to secondary education
# ============================================================

mat_sel <- mat_raw %>%
  select(
    CO_ENTIDADE,
    QT_MAT_MED,        # total secondary enrollments
    QT_MAT_MED_PROP,   # propedeutic secondary (regular ensino medio)
    QT_MAT_MED_NM,     # normal/magisterio
    QT_MAT_PROF,       # vocational education
    QT_MAT_EJA_MED     # EJA secondary level
  )

cat("Matricula columns selected:", ncol(mat_sel), "\n")

# ============================================================
# MERGE
# ============================================================

cat("Merging escola and matricula...\n")
escola_merged <- left_join(escola_raw, mat_sel, by = "CO_ENTIDADE")

cat("Merged rows:", nrow(escola_merged), "\n")

# Check merge didn't inflate rows (should match escola_raw)
if (nrow(escola_merged) != nrow(escola_raw)) {
  stop("Merge changed number of rows! Check for duplicate CO_ENTIDADE in matricula.")
}
cat("Row count check passed.\n")

# ============================================================
# FILTER: KEEP ONLY SCHOOLS WITH SECONDARY EDUCATION
# A school qualifies if it has:
# - Any regular secondary enrollment (QT_MAT_MED > 0), OR
# - Any vocational enrollment (QT_MAT_PROF > 0), OR
# - Any EJA secondary enrollment (QT_MAT_EJA_MED > 0)
# This excludes schools with only creche/pre-school/fundamental
# ============================================================

cat("Rows before filter:", nrow(escola_merged), "\n")

cescola_media <- escola_merged %>%
  filter(
    (QT_MAT_MED    > 0 & !is.na(QT_MAT_MED))    |
    (QT_MAT_PROF   > 0 & !is.na(QT_MAT_PROF))   |
    (QT_MAT_EJA_MED > 0 & !is.na(QT_MAT_EJA_MED))
  )

cat("Rows after secondary filter:", nrow(cescola_media), "\n")

# ============================================================
# SELECT VARIABLES OF INTEREST
# ============================================================

cescola_media <- cescola_media %>%
  select(

    # --- IDENTIFIERS ---
    CO_ENTIDADE, NO_ENTIDADE,

    # --- GEOLOCATION ---
    CO_REGIAO, NO_REGIAO,
    CO_UF, NO_UF, SG_UF,
    CO_MUNICIPIO, NO_MUNICIPIO,
    NO_DISTRITO, CO_DISTRITO,
    DS_ENDERECO, NU_ENDERECO, NO_BAIRRO, CO_CEP,
    LATITUDE, LONGITUDE,
    TP_LOCALIZACAO,               # 1=urban, 2=rural
    TP_LOCALIZACAO_DIFERENCIADA,  # indigenous/quilombola/settlement

    # --- SCHOOL TYPE ---
    TP_DEPENDENCIA,               # federal/state/municipal/private
    TP_SITUACAO_FUNCIONAMENTO,    # active/closed

    # --- EDUCATION LEVELS OFFERED (from escola table) ---
    IN_REGULAR, IN_EJA, IN_PROFISSIONALIZANTE,
    IN_COMUM_MEDIO_MEDIO,
    IN_COMUM_MEDIO_INTEGRADO,
    IN_COMUM_MEDIO_NORMAL,
    IN_ESP_EXCLUSIVA_MEDIO_MEDIO,
    IN_ESP_EXCLUSIVA_MEDIO_INTEGR,
    IN_ESP_EXCLUSIVA_MEDIO_NORMAL,

    # --- ENROLLMENT COUNTS (from matricula) ---
    QT_MAT_MED,
    QT_MAT_MED_PROP,
    QT_MAT_MED_NM,
    QT_MAT_PROF,
    QT_MAT_EJA_MED,

    # --- BASIC AMENITIES ---
    IN_AGUA_POTAVEL,
    IN_AGUA_REDE_PUBLICA,
    IN_AGUA_INEXISTENTE,
    IN_ENERGIA_REDE_PUBLICA,
    IN_ENERGIA_INEXISTENTE,
    IN_ESGOTO_REDE_PUBLICA,
    IN_ESGOTO_FOSSA,
    IN_ESGOTO_INEXISTENTE,
    IN_LIXO_SERVICO_COLETA,
    IN_BANHEIRO,
    IN_BANHEIRO_PNE,

    # --- EDUCATIONAL AMENITIES ---
    IN_BIBLIOTECA_SALA_LEITURA,
    IN_LABORATORIO_CIENCIAS,
    IN_LABORATORIO_INFORMATICA,
    IN_LABORATORIO_EDUC_PROF,
    IN_QUADRA_ESPORTES,
    IN_SALA_PROFESSOR,
    IN_SECRETARIA,
    IN_INTERNET,
    IN_INTERNET_ALUNOS,
    IN_INTERNET_APRENDIZAGEM,
    IN_BANDA_LARGA,
    IN_COMPUTADOR,
    IN_DESKTOP_ALUNO,
    IN_COMP_PORTATIL_ALUNO,
    IN_TABLET_ALUNO,
    IN_EQUIP_MULTIMIDIA,
    IN_SALA_DIRETORIA,
    IN_REFEITORIO,
    IN_COZINHA
  )

cat("Columns selected:", ncol(cescola_media), "\n")

# ============================================================
# CREATE BINARY INDICATORS
# ============================================================

cescola_media <- cescola_media %>%
  mutate(

    # --- BASIC AMENITIES INDEX ---
    # 1 = all critical infrastructure present
    # Sewage: public network OR septic tank both acceptable
    # (rural schools penalized unfairly if we require public network only)
    infra_ok = as.integer(
      IN_AGUA_POTAVEL      == 1 &
      IN_ENERGIA_REDE_PUBLICA == 1 &
      (IN_ESGOTO_REDE_PUBLICA == 1 | IN_ESGOTO_FOSSA == 1) &
      IN_LIXO_SERVICO_COLETA  == 1 &
      IN_BANHEIRO             == 1 &
      IN_AGUA_INEXISTENTE     == 0 &
      IN_ENERGIA_INEXISTENTE  == 0 &
      IN_ESGOTO_INEXISTENTE   == 0
    ),

    # --- EDUCATIONAL AMENITIES INDEX ---
    # Strict binary: 1 = all present
    edu_ok = as.integer(
      IN_BIBLIOTECA_SALA_LEITURA == 1 &
      IN_LABORATORIO_CIENCIAS    == 1 &
      IN_LABORATORIO_INFORMATICA == 1 &
      IN_INTERNET                == 1 &
      IN_QUADRA_ESPORTES         == 1 &
      IN_SALA_PROFESSOR          == 1 &
      IN_SECRETARIA              == 1
    ),

    # --- EDUCATIONAL AMENITIES SCORE ---
    # Additive: 0-7, useful for regression
    edu_score =
      (IN_BIBLIOTECA_SALA_LEITURA == 1) +
      (IN_LABORATORIO_CIENCIAS    == 1) +
      (IN_LABORATORIO_INFORMATICA == 1) +
      (IN_INTERNET                == 1) +
      (IN_QUADRA_ESPORTES         == 1) +
      (IN_SALA_PROFESSOR          == 1) +
      (IN_SECRETARIA              == 1)
  )

# Quick check on indicators
cat("infra_ok distribution:\n")
print(table(cescola_media$infra_ok, useNA = "always"))

cat("edu_ok distribution:\n")
print(table(cescola_media$edu_ok, useNA = "always"))

cat("edu_score distribution:\n")
print(table(cescola_media$edu_score, useNA = "always"))

# ============================================================
# SAVE cescola_media (with NAs still present)
# ============================================================

fwrite(cescola_media, PATH_OUT_MEDIA, sep = ";", bom = TRUE)
cat("Saved cescola_media to", PATH_OUT_MEDIA, "\n")
cat("Final rows:", nrow(cescola_media), "| Cols:", ncol(cescola_media), "\n")

# ============================================================
# INSPECT MISSINGNESS BEFORE DROPPING
# ============================================================

cat("\nMissing values per key variable:\n")
key_vars <- c(
  "LATITUDE", "LONGITUDE",
  "CO_MUNICIPIO", "CO_UF",
  "TP_LOCALIZACAO",
  "infra_ok", "edu_ok", "edu_score",
  "QT_MAT_MED", "QT_MAT_PROF", "QT_MAT_EJA_MED"
)

for (v in key_vars) {
  cat(sprintf("  %-35s %d missing\n", v, sum(is.na(cescola_media[[v]]))))
}

# ============================================================
# DROP NAs ON VARIABLES ESSENTIAL FOR ANALYSIS
# Keeping: schools must have geolocation + municipality +
#          localization type + both indicator variables
# ============================================================

cat("\nRows before NA drop:", nrow(cescola_media), "\n")

cescola_media_clean <- cescola_media %>%
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(CO_MUNICIPIO),
    !is.na(TP_LOCALIZACAO),
    !is.na(infra_ok),
    !is.na(edu_ok)
  )

cat("Rows after NA drop:", nrow(cescola_media_clean), "\n")
cat("Rows dropped:", nrow(cescola_media) - nrow(cescola_media_clean), "\n")

# ============================================================
# FILTER: ACTIVE SCHOOLS ONLY
# TP_SITUACAO_FUNCIONAMENTO == 1 means "Em Atividade"
# ============================================================

cat("\nFunctioning status distribution:\n")
print(table(cescola_media_clean$TP_SITUACAO_FUNCIONAMENTO, useNA = "always"))

cescola_media_clean <- cescola_media_clean %>%
  filter(TP_SITUACAO_FUNCIONAMENTO == 1)
# ============================================================
# SELECT COLUMNS FOR CLEAN DATAFRAME
# ============================================================
cat("Rows after keeping only active schools:", nrow(cescola_media_clean), "\n")
cescola_media_clean <- cescola_media_clean %>%
  select(
    # Identifiers
    CO_ENTIDADE, NO_ENTIDADE,

    # Geolocation (essential for distance analysis)
    CO_UF, SG_UF,
    CO_MUNICIPIO, NO_MUNICIPIO,
    LATITUDE, LONGITUDE,
    TP_LOCALIZACAO,              # urban/rural — key control variable
    TP_LOCALIZACAO_DIFERENCIADA, # indigenous/quilombola — important for context

    # School admin type (useful control)
    TP_DEPENDENCIA,

    # Secondary enrollment (total only)
    QT_MAT_MED,

    # Indicators (your main outcome variables)
    infra_ok,
    edu_ok,
    edu_score
  )
# ============================================================
# FINAL CHECKS
# ============================================================

cat("\nFinal infra_ok distribution:\n")
print(table(cescola_media_clean$infra_ok, useNA = "always"))

cat("\nFinal edu_ok distribution:\n")
print(table(cescola_media_clean$edu_ok, useNA = "always"))

cat("\nFinal edu_score distribution:\n")
print(table(cescola_media_clean$edu_score, useNA = "always"))

cat("\nLocalization type distribution:\n")
print(table(cescola_media_clean$TP_LOCALIZACAO, useNA = "always"))

cat("\nRegion distribution:\n")
print(table(cescola_media_clean$NO_REGIAO, useNA = "always"))

cat("\nTP_DEPENDENCIA distribution in cescola_media_clean:\n")
print(table(cescola_media_clean$TP_DEPENDENCIA, useNA = "always"))
# 1=Federal, 2=State, 3=Municipal, 4=Private

cat("\nRegion distribution:\n")
print(table(cescola_media_clean$NO_REGIAO %>% 
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT"), useNA = "always"))

cat("\nFinal dimensions:", nrow(cescola_media_clean), "rows |", 
    ncol(cescola_media_clean), "cols\n")
# ============================================================
# SAVE
# ============================================================

fwrite(cescola_media_clean, PATH_OUT_CLEAN, sep = ";", bom = TRUE)
cat("Saved cescola_media_clean to", PATH_OUT_CLEAN, "\n")

cat("\nScript 01 complete.\n")