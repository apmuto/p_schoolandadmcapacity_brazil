# ==================================================
# 01_cleandata_cescola.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Load, merge, filter and select variables
#       from Censo Escolar 2025
# Input:  data/raw/Tabela_Escola_2025.csv
#         data/raw/Tabela_Matricula_2025.csv
# Output: data/processed/cescola_media.csv
#         data/processed/cescola_media_clean.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)

# ============================================================
# CONSTANTS
# ============================================================

PATH_RAWESCOLA    <- "data/raw/Tabela_Escola_2025.csv"
PATH_RAWMATRICULA <- "data/raw/Tabela_Matricula_2025.csv"
PATH_OUT_MEDIA    <- "data/processed/cescola_media.csv"
PATH_OUT_CLEAN    <- "data/processed/cescola_media_clean.csv"

# ============================================================
# LOAD RAW DATA
# ============================================================

cat("Loading escola data...\n")
escola_raw <- load_br_csv(PATH_RAWESCOLA)
report_dims(escola_raw, "Escola raw")

cat("Loading matricula data...\n")
mat_raw <- load_br_csv(PATH_RAWMATRICULA)
report_dims(mat_raw, "Matricula raw")

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
    QT_MAT_MED,      # total secondary enrollments
    QT_MAT_MED_PROP, # propedeutic secondary (regular ensino medio)
    QT_MAT_MED_NM,   # normal/magisterio
    QT_MAT_PROF,     # vocational education
    QT_MAT_EJA_MED   # EJA secondary level
  )

cat("Matricula columns selected:", ncol(mat_sel), "\n")

# ============================================================
# MERGE
# ============================================================

cat("Merging escola and matricula...\n")
escola_merged <- left_join(escola_raw, mat_sel, by = "CO_ENTIDADE")
check_merge(escola_merged, nrow(escola_raw), "Escola + Matricula")

# ============================================================
# FILTER: SECONDARY SCHOOLS ONLY
# Keeps schools with any secondary enrollment:
# regular ensino medio, vocational, or EJA secondary
# Excludes schools with only creche/pre-school/fundamental
# ============================================================

cat("Rows before filter:", nrow(escola_merged), "\n")

cescola_media <- escola_merged %>%
  filter(
    (QT_MAT_MED     > 0 & !is.na(QT_MAT_MED))    |
    (QT_MAT_PROF    > 0 & !is.na(QT_MAT_PROF))   |
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
    TP_LOCALIZACAO,              # 1=urban, 2=rural
    TP_LOCALIZACAO_DIFERENCIADA, # indigenous/quilombola/settlement

    # --- SCHOOL TYPE ---
    TP_DEPENDENCIA,              # federal/state/municipal/private
    TP_SITUACAO_FUNCIONAMENTO,   # active/closed

    # --- EDUCATION LEVELS OFFERED ---
    IN_REGULAR, IN_EJA, IN_PROFISSIONALIZANTE,
    IN_COMUM_MEDIO_MEDIO,
    IN_COMUM_MEDIO_INTEGRADO,
    IN_COMUM_MEDIO_NORMAL,
    IN_ESP_EXCLUSIVA_MEDIO_MEDIO,
    IN_ESP_EXCLUSIVA_MEDIO_INTEGR,
    IN_ESP_EXCLUSIVA_MEDIO_NORMAL,

    # --- ENROLLMENT COUNTS ---
    QT_MAT_MED, QT_MAT_MED_PROP, QT_MAT_MED_NM,
    QT_MAT_PROF, QT_MAT_EJA_MED,

    # --- BASIC AMENITIES ---
    IN_AGUA_POTAVEL, IN_AGUA_REDE_PUBLICA, IN_AGUA_INEXISTENTE,
    IN_ENERGIA_REDE_PUBLICA, IN_ENERGIA_INEXISTENTE,
    IN_ESGOTO_REDE_PUBLICA, IN_ESGOTO_FOSSA, IN_ESGOTO_INEXISTENTE,
    IN_LIXO_SERVICO_COLETA,
    IN_BANHEIRO, IN_BANHEIRO_PNE,

    # --- EDUCATIONAL AMENITIES ---
    IN_BIBLIOTECA_SALA_LEITURA,
    IN_LABORATORIO_CIENCIAS, IN_LABORATORIO_INFORMATICA,
    IN_LABORATORIO_EDUC_PROF,
    IN_QUADRA_ESPORTES,
    IN_SALA_PROFESSOR, IN_SECRETARIA,
    IN_INTERNET, IN_INTERNET_ALUNOS, IN_INTERNET_APRENDIZAGEM,
    IN_BANDA_LARGA,
    IN_COMPUTADOR, IN_DESKTOP_ALUNO,
    IN_COMP_PORTATIL_ALUNO, IN_TABLET_ALUNO,
    IN_EQUIP_MULTIMIDIA,
    IN_SALA_DIRETORIA, IN_REFEITORIO, IN_COZINHA
  )

cat("Variables selected:", ncol(cescola_media), "\n")

# ============================================================
# CREATE BINARY INDICATORS
# ============================================================

cescola_media <- cescola_media %>%
  mutate(

    # Basic infrastructure: all critical amenities present
    # Sewage accepts public network OR septic tank —
    # requiring only public network unfairly penalizes rural schools
    infra_ok = as.integer(
      IN_AGUA_POTAVEL         == 1 &
      IN_ENERGIA_REDE_PUBLICA == 1 &
      (IN_ESGOTO_REDE_PUBLICA == 1 | IN_ESGOTO_FOSSA == 1) &
      IN_LIXO_SERVICO_COLETA  == 1 &
      IN_BANHEIRO             == 1 &
      IN_AGUA_INEXISTENTE     == 0 &
      IN_ENERGIA_INEXISTENTE  == 0 &
      IN_ESGOTO_INEXISTENTE   == 0
    ),

    # Educational amenities: strict binary 1 = all 7 present
    edu_ok = as.integer(
      IN_BIBLIOTECA_SALA_LEITURA == 1 &
      IN_LABORATORIO_CIENCIAS    == 1 &
      IN_LABORATORIO_INFORMATICA == 1 &
      IN_INTERNET                == 1 &
      IN_QUADRA_ESPORTES         == 1 &
      IN_SALA_PROFESSOR          == 1 &
      IN_SECRETARIA              == 1
    ),

    # Educational amenities: additive score 0-7
    # More flexible for regression analysis
    edu_score =
      (IN_BIBLIOTECA_SALA_LEITURA == 1) +
      (IN_LABORATORIO_CIENCIAS    == 1) +
      (IN_LABORATORIO_INFORMATICA == 1) +
      (IN_INTERNET                == 1) +
      (IN_QUADRA_ESPORTES         == 1) +
      (IN_SALA_PROFESSOR          == 1) +
      (IN_SECRETARIA              == 1)
  )

report_table(cescola_media, "infra_ok")
report_table(cescola_media, "edu_ok")
report_table(cescola_media, "edu_score")

# ============================================================
# SAVE cescola_media
# All secondary schools, no rows dropped
# Use for papers that don't require geolocation
# ============================================================

save_processed(cescola_media, PATH_OUT_MEDIA)

# ============================================================
# INSPECT MISSINGNESS
# ============================================================

report_missing(cescola_media, c(
  "LATITUDE", "LONGITUDE",
  "CO_MUNICIPIO", "CO_UF", "TP_LOCALIZACAO",
  "infra_ok", "edu_ok", "edu_score",
  "QT_MAT_MED", "QT_MAT_PROF", "QT_MAT_EJA_MED"
))

# ============================================================
# CREATE cescola_media_clean
# Drops schools missing coordinates, inactive schools
# Use for distance-based spatial analysis
# ============================================================

cat("\nRows before clean filter:", nrow(cescola_media), "\n")

cescola_media_clean <- cescola_media %>%
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(CO_MUNICIPIO),
    !is.na(TP_LOCALIZACAO),
    !is.na(infra_ok),
    !is.na(edu_ok),
    TP_SITUACAO_FUNCIONAMENTO == 1
  ) %>%
  select(
    CO_ENTIDADE, NO_ENTIDADE,
    CO_UF, SG_UF,
    CO_MUNICIPIO, NO_MUNICIPIO,
    LATITUDE, LONGITUDE,
    TP_LOCALIZACAO,
    TP_LOCALIZACAO_DIFERENCIADA,
    TP_DEPENDENCIA,
    QT_MAT_MED,
    infra_ok, edu_ok, edu_score
  )

cat("Rows dropped:", nrow(cescola_media) - nrow(cescola_media_clean), "\n")
report_dims(cescola_media_clean, "cescola_media_clean")

# ============================================================
# FINAL CHECKS
# ============================================================

report_table(cescola_media_clean, "infra_ok",      "Final infra_ok")
report_table(cescola_media_clean, "edu_ok",        "Final edu_ok")
report_table(cescola_media_clean, "edu_score",     "Final edu_score")
report_table(cescola_media_clean, "TP_LOCALIZACAO","Localization")
report_table(cescola_media_clean, "TP_DEPENDENCIA","TP_DEPENDENCIA")
# 1=Federal, 2=State, 3=Municipal, 4=Private

cat("\nRegion distribution:\n")
print(table(remove_accents(cescola_media$NO_REGIAO), useNA = "always"))

# ============================================================
# SAVE cescola_media_clean
# ============================================================

save_processed(cescola_media_clean, PATH_OUT_CLEAN)

cat("\nScript 01 complete.\n")