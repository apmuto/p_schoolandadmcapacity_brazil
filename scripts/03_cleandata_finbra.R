# ==================================================
# 03_cleandata_finbra.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Load and clean FINBRA 2024 data for
#       municipalities and states. Build fiscal
#       capacity indicators for main model and
#       robustness checks.
# Input:  data/raw/finbra2024_mun_IC.csv
#         data/raw/finbra2024_mun_IE.csv
#         data/raw/finbra2024_mun_IAB.csv
#         data/raw/finbra2024_est_IC.csv
#         data/raw/finbra2024_est_IE.csv
#         data/raw/finbra2024_est_IAB.csv
#         data/raw/capag-municipios-posicao-2025-fev-19.xlsx
#         data/raw/capagdosestados2025.csv
# Output: data/processed/finbra_municipios.csv
#         data/processed/finbra_estados.csv
#         data/processed/capag_municipios.csv
#         data/processed/capag_estados.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(readxl)

# ============================================================
# CONSTANTS
# ============================================================

PATH_MUN_IC    <- "data/raw/finbra2024_mun_IC.csv"
PATH_MUN_IE    <- "data/raw/finbra2024_mun_IE.csv"
PATH_MUN_IAB   <- "data/raw/finbra2024_mun_IAB.csv"
PATH_EST_IC    <- "data/raw/finbra2024_est_IC.csv"
PATH_EST_IE    <- "data/raw/finbra2024_est_IE.csv"
PATH_EST_IAB   <- "data/raw/finbra2024_est_IAB.csv"
PATH_CAPAG_MUN <- "data/raw/capag-municipios-posicao-2025-fev-19.xlsx"
PATH_CAPAG_EST <- "data/raw/capagdosestados2025.csv"
PATH_OUT_MUN       <- "data/processed/finbra_municipios.csv"
PATH_OUT_EST       <- "data/processed/finbra_estados.csv"
PATH_OUT_CAPAG_MUN <- "data/processed/capag_municipios.csv"
PATH_OUT_CAPAG_EST <- "data/processed/capag_estados.csv"

# ============================================================
# LOAD MUNICIPAL FINBRA FILES
# load_finbra() auto-cleans Conta, Coluna, Valor
# ============================================================

cat("Loading municipal FINBRA files...\n")

mun_ic  <- load_finbra(PATH_MUN_IC)
mun_ie  <- load_finbra(PATH_MUN_IE)
mun_iab <- load_finbra(PATH_MUN_IAB)

# Rename merge key to CO_MUNICIPIO for consistency
mun_ic$CO_MUNICIPIO  <- as.character(mun_ic$Cod.IBGE)
mun_ie$CO_MUNICIPIO  <- as.character(mun_ie$Cod.IBGE)
mun_iab$CO_MUNICIPIO <- as.character(mun_iab$Cod.IBGE)

cat("Municipal I-C  rows:", nrow(mun_ic),
    "| Municipalities:", n_distinct(mun_ic$CO_MUNICIPIO), "\n")
cat("Municipal I-E  rows:", nrow(mun_ie),
    "| Municipalities:", n_distinct(mun_ie$CO_MUNICIPIO), "\n")
cat("Municipal I-AB rows:", nrow(mun_iab),
    "| Municipalities:", n_distinct(mun_iab$CO_MUNICIPIO), "\n")

# ============================================================
# EXTRACT MUNICIPAL REVENUE INDICATORS (I-C)
# Filter: Receitas Brutas Realizadas only
# ============================================================

cat("\nExtracting municipal revenue indicators...\n")

mun_receitas <- mun_ic %>%
  filter(Coluna == "Receitas Brutas Realizadas") %>%
  filter(Conta %in% c(
    "RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)",
    "1.1.0.0.00.0.0 - Impostos, Taxas e Contribuições de Melhoria",
    "1.7.0.0.00.0.0 - Transferências Correntes",
    "1.7.1.0.00.0.0 - Transferências da União e de suas Entidades",
    "1.7.1.1.51.0.0 -Cota-Parte do Fundo de Participação dos Municípios - FPM",
    "1.7.2.0.00.0.0 - Transferências dos Estados e do Distrito Federal e de suas Entidades"
  )) %>%
  select(CO_MUNICIPIO, UF, Populacao = População, Conta, Valor) %>%
  pivot_wider(names_from = Conta, values_from = Valor) %>%
  rename(
    receita_total  = `RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)`,
    receita_propria= `1.1.0.0.00.0.0 - Impostos, Taxas e Contribuições de Melhoria`,
    transf_total   = `1.7.0.0.00.0.0 - Transferências Correntes`,
    transf_uniao   = `1.7.1.0.00.0.0 - Transferências da União e de suas Entidades`,
    fpm            = `1.7.1.1.51.0.0 -Cota-Parte do Fundo de Participação dos Municípios - FPM`,
    transf_estados = `1.7.2.0.00.0.0 - Transferências dos Estados e do Distrito Federal e de suas Entidades`
  )

cat("Revenue indicators extracted for:", nrow(mun_receitas), "municipalities\n")

# ============================================================
# EXTRACT MUNICIPAL EXPENDITURE INDICATORS (I-E)
# ============================================================

cat("\nExtracting municipal expenditure indicators...\n")

mun_exec <- mun_ie %>%
  filter(Conta == "Despesas Exceto Intraorçamentárias") %>%
  filter(Coluna %in% c("Despesas Empenhadas", "Despesas Liquidadas")) %>%
  select(CO_MUNICIPIO, Coluna, Valor) %>%
  pivot_wider(names_from = Coluna, values_from = Valor) %>%
  rename(
    desp_empenhadas = `Despesas Empenhadas`,
    desp_liquidadas = `Despesas Liquidadas`
  )

mun_edu <- mun_ie %>%
  filter(Conta == "12 - Educação") %>%
  filter(Coluna == "Despesas Liquidadas") %>%
  select(CO_MUNICIPIO, desp_educacao = Valor)

mun_despesas <- mun_exec %>%
  left_join(mun_edu, by = "CO_MUNICIPIO")

cat("Expenditure indicators extracted for:", nrow(mun_despesas), "municipalities\n")

# ============================================================
# EXTRACT MUNICIPAL DEBT INDICATORS (I-AB)
# ============================================================

cat("\nExtracting municipal debt indicators...\n")

mun_divida <- mun_iab %>%
  filter(Coluna == "31/12/2024") %>%
  filter(Conta %in% c(
    "1.0.0.0.0.00.00 - Ativo",
    "2.1.0.0.0.00.00 - Passivo Circulante",
    "2.2.0.0.0.00.00 - Passivo Não-Circulante",
    "2.2.2.0.0.00.00 - Empréstimos e Financiamentos a Longo Prazo"
  )) %>%
  select(CO_MUNICIPIO, Conta, Valor) %>%
  pivot_wider(names_from = Conta, values_from = Valor) %>%
  rename(
    ativo_total        = `1.0.0.0.0.00.00 - Ativo`,
    passivo_circulante = `2.1.0.0.0.00.00 - Passivo Circulante`,
    passivo_nao_circ   = `2.2.0.0.0.00.00 - Passivo Não-Circulante`,
    divida_lp          = `2.2.2.0.0.00.00 - Empréstimos e Financiamentos a Longo Prazo`
  )

cat("Debt indicators extracted for:", nrow(mun_divida), "municipalities\n")

# ============================================================
# MERGE MUNICIPAL INDICATORS
# ============================================================

finbra_mun <- mun_receitas %>%
  left_join(mun_despesas, by = "CO_MUNICIPIO") %>%
  left_join(mun_divida,   by = "CO_MUNICIPIO")

check_merge(finbra_mun, nrow(mun_receitas), "Municipal merge")

# ============================================================
# BUILD MUNICIPAL FISCAL CAPACITY INDICATORS
# All variables suffixed with _mun for clarity in master merge
# ============================================================

cat("\nBuilding municipal fiscal capacity indicators...\n")

finbra_mun <- finbra_mun %>%
  mutate(
    # Share of revenue generated locally
    fiscal_autonomy_mun     = receita_propria / receita_total,

    # Share of revenue from intergovernmental transfers (inverted in score)
    transfer_dependence_mun = transf_total / receita_total,

    # Share from FPM federal transfer (inverted in score)
    fpm_dependence_mun      = fpm / receita_total,

    # Share of spending going to education
    edu_share_mun           = desp_educacao / desp_liquidadas,

    # Share of committed spending actually delivered
    budget_execution_mun    = desp_liquidadas / desp_empenhadas,

    # Total liabilities relative to revenue (inverted in score)
    debt_ratio_mun          = (passivo_circulante + passivo_nao_circ) /
                               receita_total,

    # Composite score: standardised mean, higher = more capable
    adm_capacity_score_mun  = rowMeans(
      scale(cbind(
        fiscal_autonomy_mun,
        -transfer_dependence_mun,
        -fpm_dependence_mun,
        edu_share_mun,
        budget_execution_mun,
        -debt_ratio_mun
      )), na.rm = TRUE
    )
  )

report_indicators(finbra_mun, c(
  "fiscal_autonomy_mun", "transfer_dependence_mun",
  "fpm_dependence_mun",  "edu_share_mun",
  "budget_execution_mun","debt_ratio_mun",
  "adm_capacity_score_mun"
))

# ============================================================
# LOAD STATE FINBRA FILES
# load_finbra() auto-cleans Conta, Coluna, Valor
# ============================================================

cat("\nLoading state FINBRA files...\n")

est_ic  <- load_finbra(PATH_EST_IC)
est_ie  <- load_finbra(PATH_EST_IE)
est_iab <- load_finbra(PATH_EST_IAB)

cat("State I-C  rows:", nrow(est_ic),
    "| States:", n_distinct(est_ic$UF), "\n")
cat("State I-E  rows:", nrow(est_ie),
    "| States:", n_distinct(est_ie$UF), "\n")
cat("State I-AB rows:", nrow(est_iab),
    "| States:", n_distinct(est_iab$UF), "\n")

# ============================================================
# EXTRACT STATE REVENUE INDICATORS (I-C)
# ============================================================

cat("\nExtracting state revenue indicators...\n")

est_receitas <- est_ic %>%
  filter(Coluna == "Receitas Brutas Realizadas") %>%
  filter(Conta %in% c(
    "RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)",
    "1.1.0.0.00.0.0 - Impostos, Taxas e Contribuições de Melhoria",
    "1.7.0.0.00.0.0 - Transferências Correntes",
    "1.7.1.0.00.0.0 - Transferências da União e de suas Entidades"
  )) %>%
  select(UF, Conta, Valor) %>%
  pivot_wider(names_from = Conta, values_from = Valor) %>%
  rename(
    receita_total   = `RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)`,
    receita_propria = `1.1.0.0.00.0.0 - Impostos, Taxas e Contribuições de Melhoria`,
    transf_total    = `1.7.0.0.00.0.0 - Transferências Correntes`,
    transf_uniao    = `1.7.1.0.00.0.0 - Transferências da União e de suas Entidades`
  )

# ============================================================
# EXTRACT STATE EXPENDITURE INDICATORS (I-E)
# ============================================================

cat("\nExtracting state expenditure indicators...\n")

est_exec <- est_ie %>%
  filter(Conta == "Despesas Exceto Intraorçamentárias") %>%
  filter(Coluna %in% c("Despesas Empenhadas", "Despesas Liquidadas")) %>%
  select(UF, Coluna, Valor) %>%
  pivot_wider(names_from = Coluna, values_from = Valor) %>%
  rename(
    desp_empenhadas = `Despesas Empenhadas`,
    desp_liquidadas = `Despesas Liquidadas`
  )

est_edu <- est_ie %>%
  filter(Conta == "12 - Educação") %>%
  filter(Coluna == "Despesas Liquidadas") %>%
  select(UF, desp_educacao = Valor)

est_despesas <- est_exec %>%
  left_join(est_edu, by = "UF")

# ============================================================
# EXTRACT STATE DEBT INDICATORS (I-AB)
# ============================================================

cat("\nExtracting state debt indicators...\n")

est_divida <- est_iab %>%
  filter(Coluna == "31/12/2024") %>%
  filter(Conta %in% c(
    "1.0.0.0.0.00.00 - Ativo",
    "2.1.0.0.0.00.00 - Passivo Circulante",
    "2.2.0.0.0.00.00 - Passivo Não-Circulante",
    "2.2.2.0.0.00.00 - Empréstimos e Financiamentos a Longo Prazo"
  )) %>%
  select(UF, Conta, Valor) %>%
  pivot_wider(names_from = Conta, values_from = Valor) %>%
  rename(
    ativo_total        = `1.0.0.0.0.00.00 - Ativo`,
    passivo_circulante = `2.1.0.0.0.00.00 - Passivo Circulante`,
    passivo_nao_circ   = `2.2.0.0.0.00.00 - Passivo Não-Circulante`,
    divida_lp          = `2.2.2.0.0.00.00 - Empréstimos e Financiamentos a Longo Prazo`
  )

# ============================================================
# MERGE AND BUILD STATE INDICATORS
# All variables suffixed with _est for clarity in master merge
# ============================================================

cat("\nMerging and building state indicators...\n")

finbra_est <- est_receitas %>%
  left_join(est_despesas, by = "UF") %>%
  left_join(est_divida,   by = "UF") %>%
  mutate(
    fiscal_autonomy_est     = receita_propria / receita_total,
    transfer_dependence_est = transf_total / receita_total,
    edu_share_est           = desp_educacao / desp_liquidadas,
    budget_execution_est    = desp_liquidadas / desp_empenhadas,
    debt_ratio_est          = (passivo_circulante + passivo_nao_circ) /
                               receita_total,
    adm_capacity_score_est  = rowMeans(
      scale(cbind(
        fiscal_autonomy_est,
        -transfer_dependence_est,
        edu_share_est,
        budget_execution_est,
        -debt_ratio_est
      )), na.rm = TRUE
    )
  )

finbra_est <- finbra_est %>%
  rename(SG_UF = UF) 

cat("State indicators built for:", nrow(finbra_est), "states\n")

report_indicators(finbra_est, c(
  "fiscal_autonomy_est", "transfer_dependence_est",
  "edu_share_est",       "budget_execution_est",
  "debt_ratio_est",      "adm_capacity_score_est"
))

# ============================================================
# LOAD AND CLEAN CAPAG (ROBUSTNESS CHECK VARIABLES)
# ============================================================

cat("\nLoading CAPAG data...\n")

capag_mun <- read_excel(PATH_CAPAG_MUN, skip = 2) %>%
  rename(
    CO_MUNICIPIO    = `Código Município Completo`,
    nome_municipio  = Nome_Município,
    uf              = UF,
    capag           = CAPAG,
    ind1_valor      = `Indicador 1`,
    ind1_nota       = `Nota 1`,
    ind2_valor      = `Indicador 2`,
    ind2_nota       = `Nota 2`,
    ind3_valor      = `Indicador 3`,
    ind3_nota       = `Nota 3`,
    icf             = ICF,
    dca_2024        = `Possui DCA 2024?`,
    capag_rebaixada = `CAPAG rebaixada`
  ) %>%
  select(CO_MUNICIPIO, nome_municipio, uf, capag,
         ind1_valor, ind1_nota, ind2_valor, ind2_nota,
         ind3_valor, ind3_nota, icf, dca_2024,
         capag_rebaixada) %>%
  mutate(
    CO_MUNICIPIO  = as.character(CO_MUNICIPIO),
    capag_numeric = capag_to_numeric(capag)
  )

cat("Municipal CAPAG rows:", nrow(capag_mun), "\n")
report_table(capag_mun, "capag", "Municipal CAPAG grade")

capag_est <- load_br_csv(PATH_CAPAG_EST) %>%
  rename(SG_UF = UF) %>%
  rename_with(~ "capag_estado",   matches("Classifica")) %>%
  rename_with(~ "qualidade_info", matches("Qualidade")) %>%
  rename_with(~ "observacao",     matches("Observa")) %>%
  select(SG_UF, capag_estado, qualidade_info) %>%
  mutate(
    capag_estado_base    = str_extract(capag_estado, "^[A-D]"),
    capag_estado_numeric = capag_to_numeric(capag_estado_base)
  )

cat("State CAPAG rows:", nrow(capag_est), "\n")
report_table(capag_est, "capag_estado", "State CAPAG grade")

# ============================================================
# FINAL CHECKS
# ============================================================

report_dims(finbra_mun,  "finbra_municipios")
report_dims(finbra_est,  "finbra_estados")
report_dims(capag_mun,   "capag_municipios")
report_dims(capag_est,   "capag_estados")

# ============================================================
# SAVE
# ============================================================

save_processed(finbra_mun,  PATH_OUT_MUN)
save_processed(finbra_est,  PATH_OUT_EST)
save_processed(capag_mun,   PATH_OUT_CAPAG_MUN)
save_processed(capag_est,   PATH_OUT_CAPAG_EST)

cat("\nScript 03 complete.\n")