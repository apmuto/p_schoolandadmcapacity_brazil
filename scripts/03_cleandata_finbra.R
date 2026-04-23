# ==================================================
# 03-cleandata-finbra.R
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

library(tidyverse)
library(data.table)
library(readxl)

# ============================================================
# CONSTANTS
# ============================================================

PATH_MUN_IC  <- "data/raw/finbra2024_mun_IC.csv"
PATH_MUN_IE  <- "data/raw/finbra2024_mun_IE.csv"
PATH_MUN_IAB <- "data/raw/finbra2024_mun_IAB.csv"
PATH_EST_IC  <- "data/raw/finbra2024_est_IC.csv"
PATH_EST_IE  <- "data/raw/finbra2024_est_IE.csv"
PATH_EST_IAB <- "data/raw/finbra2024_est_IAB.csv"
PATH_CAPAG_MUN <- "data/raw/capag-municipios-posicao-2025-fev-19.xlsx"
PATH_CAPAG_EST <- "data/raw/capagdosestados2025.csv"

PATH_OUT_MUN   <- "data/processed/finbra_municipios.csv"
PATH_OUT_EST   <- "data/processed/finbra_estados.csv"
PATH_OUT_CAPAG_MUN <- "data/processed/capag_municipios.csv"
PATH_OUT_CAPAG_EST <- "data/processed/capag_estados.csv"

# Helper function to load FINBRA files
# All files have 3 header rows before actual data
load_finbra <- function(path) {
  fread(path, encoding = "Latin-1", quote = "",
        fill = TRUE, sep = ";", skip = 3)
}

# Helper function to clean account names
# Removes surrounding quotes added by FINBRA export
clean_conta <- function(x) {
  gsub('^"|"$', '', x)
}

# ============================================================
# LOAD MUNICIPAL FINBRA FILES
# ============================================================

cat("Loading municipal FINBRA files...\n")

mun_ic  <- load_finbra(PATH_MUN_IC)
mun_ie  <- load_finbra(PATH_MUN_IE)
mun_iab <- load_finbra(PATH_MUN_IAB)

cat("Municipal I-C rows:", nrow(mun_ic),
    "| Municipalities:", n_distinct(mun_ic$Cod.IBGE), "\n")
cat("Municipal I-E rows:", nrow(mun_ie),
    "| Municipalities:", n_distinct(mun_ie$Cod.IBGE), "\n")
cat("Municipal I-AB rows:", nrow(mun_iab),
    "| Municipalities:", n_distinct(mun_iab$Cod.IBGE), "\n")

# Clean account names in all files
mun_ic$Conta  <- clean_conta(mun_ic$Conta)
mun_ic$Coluna <- clean_conta(mun_ic$Coluna)
mun_ie$Conta  <- clean_conta(mun_ie$Conta)
mun_ie$Coluna <- clean_conta(mun_ie$Coluna)
mun_iab$Conta  <- clean_conta(mun_iab$Conta)
mun_iab$Coluna <- clean_conta(mun_iab$Coluna)

# Fix Valor: replace comma decimal separator with period
mun_ic$Valor  <- as.numeric(gsub(",", ".", mun_ic$Valor))
mun_ie$Valor  <- as.numeric(gsub(",", ".", mun_ie$Valor))
mun_iab$Valor <- as.numeric(gsub(",", ".", mun_iab$Valor))

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
  select(Cod.IBGE, UF, Populacao = População, Conta, Valor) %>%
  pivot_wider(names_from = Conta, values_from = Valor) %>%
  rename(
    receita_total     = `RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)`,
    receita_propria   = `1.1.0.0.00.0.0 - Impostos, Taxas e Contribuições de Melhoria`,
    transf_total      = `1.7.0.0.00.0.0 - Transferências Correntes`,
    transf_uniao      = `1.7.1.0.00.0.0 - Transferências da União e de suas Entidades`,
    fpm               = `1.7.1.1.51.0.0 -Cota-Parte do Fundo de Participação dos Municípios - FPM`,
    transf_estados    = `1.7.2.0.00.0.0 - Transferências dos Estados e do Distrito Federal e de suas Entidades`
  )

cat("Revenue indicators extracted for:", nrow(mun_receitas), "municipalities\n")

# ============================================================
# EXTRACT MUNICIPAL EXPENDITURE INDICATORS (I-E)
# ============================================================

cat("\nExtracting municipal expenditure indicators...\n")

# Budget execution: liquidadas / empenhadas
mun_exec <- mun_ie %>%
  filter(Conta == "Despesas Exceto Intraorçamentárias") %>%
  filter(Coluna %in% c("Despesas Empenhadas", "Despesas Liquidadas")) %>%
  select(Cod.IBGE, Coluna, Valor) %>%
  pivot_wider(names_from = Coluna, values_from = Valor) %>%
  rename(
    desp_empenhadas  = `Despesas Empenhadas`,
    desp_liquidadas  = `Despesas Liquidadas`
  )

# Education expenditure
mun_edu <- mun_ie %>%
  filter(Conta == "12 - Educação") %>%
  filter(Coluna == "Despesas Liquidadas") %>%
  select(Cod.IBGE, desp_educacao = Valor)

# Merge expenditure indicators
mun_despesas <- mun_exec %>%
  left_join(mun_edu, by = "Cod.IBGE")

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
  select(Cod.IBGE, Conta, Valor) %>%
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

# Ensure Cod.IBGE is character in all dataframes before joining
mun_receitas$Cod.IBGE <- as.character(mun_receitas$Cod.IBGE)
mun_despesas$Cod.IBGE <- as.character(mun_despesas$Cod.IBGE)
mun_divida$Cod.IBGE   <- as.character(mun_divida$Cod.IBGE)

finbra_mun <- mun_receitas %>%
  left_join(mun_despesas, by = "Cod.IBGE") %>%
  left_join(mun_divida,   by = "Cod.IBGE")

cat("Merged rows:", nrow(finbra_mun), "\n")
# ============================================================
# BUILD MUNICIPAL FISCAL CAPACITY INDICATORS
# ============================================================

cat("\nBuilding municipal fiscal capacity indicators...\n")

finbra_mun <- finbra_mun %>%
  mutate(

    # --- FISCAL AUTONOMY ---
    # Share of revenue generated locally vs received from transfers
    fiscal_autonomy = receita_propria / receita_total,

    # --- TRANSFER DEPENDENCE ---
    # Share of revenue coming from intergovernmental transfers
    transfer_dependence = transf_total / receita_total,

    # --- FPM DEPENDENCE ---
    # Share of revenue coming specifically from FPM
    # Key federal equalisation transfer — higher = more dependent on Brasília
    fpm_dependence = fpm / receita_total,

    # --- EDUCATION SHARE ---
    # Share of liquidated expenditure going to education
    edu_share = desp_educacao / desp_liquidadas,

    # --- BUDGET EXECUTION ---
    # Share of committed expenditure actually liquidated
    # Higher = better administrative delivery capacity
    budget_execution = desp_liquidadas / desp_empenhadas,

    # --- DEBT RATIO ---
    # Short-term liabilities relative to total revenue
    # Higher = more fiscally stressed
    debt_ratio = passivo_circulante / receita_total,

    # --- COMPOSITE ADMINISTRATIVE CAPACITY SCORE ---
    # Standardised mean of all indicators
    # Invert transfer_dependence, fpm_dependence, debt_ratio
    # so higher score always = more capable
    adm_capacity_score = rowMeans(
      scale(cbind(
        fiscal_autonomy,
        -transfer_dependence,
        -fpm_dependence,
        edu_share,
        budget_execution,
        -debt_ratio
      )), na.rm = TRUE
    )
  )

# ============================================================
# DIAGNOSTICS ON MUNICIPAL INDICATORS
# ============================================================

cat("\n--- Municipal indicator diagnostics ---\n")

indicators <- c("fiscal_autonomy", "transfer_dependence",
                "fpm_dependence", "edu_share",
                "budget_execution", "debt_ratio",
                "adm_capacity_score")

for (ind in indicators) {
  vals <- finbra_mun[[ind]]
  cat(sprintf("%-25s mean=%.3f | sd=%.3f | NA=%d\n",
              ind, mean(vals, na.rm=TRUE),
              sd(vals, na.rm=TRUE), sum(is.na(vals))))
}

# ============================================================
# LOAD STATE FINBRA FILES
# ============================================================

cat("\nLoading state FINBRA files...\n")

est_ic  <- load_finbra(PATH_EST_IC)
est_ie  <- load_finbra(PATH_EST_IE)
est_iab <- load_finbra(PATH_EST_IAB)

# Clean account names and values
for (df in list(est_ic, est_ie, est_iab)) {
  df$Conta  <- clean_conta(df$Conta)
  df$Coluna <- clean_conta(df$Coluna)
  df$Valor  <- as.numeric(gsub(",", ".", df$Valor))
}

# Re-assign after cleaning (list doesn't modify in place)
est_ic$Conta   <- clean_conta(est_ic$Conta)
est_ic$Coluna  <- clean_conta(est_ic$Coluna)
est_ic$Valor   <- as.numeric(gsub(",", ".", est_ic$Valor))
est_ie$Conta   <- clean_conta(est_ie$Conta)
est_ie$Coluna  <- clean_conta(est_ie$Coluna)
est_ie$Valor   <- as.numeric(gsub(",", ".", est_ie$Valor))
est_iab$Conta  <- clean_conta(est_iab$Conta)
est_iab$Coluna <- clean_conta(est_iab$Coluna)
est_iab$Valor  <- as.numeric(gsub(",", ".", est_iab$Valor))

cat("State I-C rows:", nrow(est_ic),
    "| States:", n_distinct(est_ic$UF), "\n")
cat("State I-E rows:", nrow(est_ie),
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
# ============================================================

cat("\nMerging and building state indicators...\n")

finbra_est <- est_receitas %>%
  left_join(est_despesas, by = "UF") %>%
  left_join(est_divida,   by = "UF") %>%
  mutate(
    fiscal_autonomy     = receita_propria / receita_total,
    transfer_dependence = transf_total / receita_total,
    edu_share           = desp_educacao / desp_liquidadas,
    budget_execution    = desp_liquidadas / desp_empenhadas,
    debt_ratio          = passivo_circulante / receita_total,
    adm_capacity_score  = rowMeans(
      scale(cbind(
        fiscal_autonomy,
        -transfer_dependence,
        edu_share,
        budget_execution,
        -debt_ratio
      )), na.rm = TRUE
    )
  )

cat("State indicators built for:", nrow(finbra_est), "states\n")

cat("\n--- State indicator diagnostics ---\n")
for (ind in c("fiscal_autonomy", "transfer_dependence",
              "edu_share", "budget_execution",
              "debt_ratio", "adm_capacity_score")) {
  vals <- finbra_est[[ind]]
  cat(sprintf("%-25s mean=%.3f | sd=%.3f | NA=%d\n",
              ind, mean(vals, na.rm=TRUE),
              sd(vals, na.rm=TRUE), sum(is.na(vals))))
}

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
         ind3_valor, ind3_nota, icf, dca_2024, capag_rebaixada) %>%
  mutate(
    capag_numeric = case_when(
      capag == "A"  ~ 4,
      capag == "B"  ~ 3,
      capag == "C"  ~ 2,
      capag == "D"  ~ 1,
      TRUE          ~ NA_real_
    )
  )

cat("Municipal CAPAG rows:", nrow(capag_mun), "\n")
cat("CAPAG grade distribution:\n")
print(table(capag_mun$capag, useNA = "always"))

capag_est <- fread(PATH_CAPAG_EST, encoding = "Latin-1") %>%
  rename(sg_uf = UF) %>%
  rename_with(~ "capag_estado",   matches("Classifica")) %>%
  rename_with(~ "qualidade_info", matches("Qualidade")) %>%
  rename_with(~ "observacao",     matches("Observa")) %>%
  select(sg_uf, capag_estado, qualidade_info) %>%
  mutate(
    capag_estado_base    = str_extract(capag_estado, "^[A-D]"),
    capag_estado_numeric = case_when(
      capag_estado_base == "A" ~ 4,
      capag_estado_base == "B" ~ 3,
      capag_estado_base == "C" ~ 2,
      capag_estado_base == "D" ~ 1,
      TRUE ~ NA_real_
    )
  )

cat("State CAPAG rows:", nrow(capag_est), "\n")
cat("State CAPAG distribution:\n")
print(table(capag_est$capag_estado, useNA = "always"))

# ============================================================
# FINAL CHECKS
# ============================================================

cat("\n--- Final dimensions ---\n")
cat("finbra_municipios:", nrow(finbra_mun), "rows |",
    ncol(finbra_mun), "cols\n")
cat("finbra_estados:", nrow(finbra_est), "rows |",
    ncol(finbra_est), "cols\n")
cat("capag_municipios:", nrow(capag_mun), "rows |",
    ncol(capag_mun), "cols\n")
cat("capag_estados:", nrow(capag_est), "rows |",
    ncol(capag_est), "cols\n")

# ============================================================
# SAVE
# ============================================================

fwrite(finbra_mun, PATH_OUT_MUN, sep = ";", bom = TRUE)
cat("\nSaved finbra_municipios to", PATH_OUT_MUN, "\n")

fwrite(finbra_est, PATH_OUT_EST, sep = ";", bom = TRUE)
cat("Saved finbra_estados to", PATH_OUT_EST, "\n")

fwrite(capag_mun, PATH_OUT_CAPAG_MUN, sep = ";", bom = TRUE)
cat("Saved capag_municipios to", PATH_OUT_CAPAG_MUN, "\n")

fwrite(capag_est, PATH_OUT_CAPAG_EST, sep = ";", bom = TRUE)
cat("Saved capag_estados to", PATH_OUT_CAPAG_EST, "\n")

cat("\nScript 03 complete.\n")