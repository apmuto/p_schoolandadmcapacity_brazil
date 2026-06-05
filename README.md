# State Capacity and Secondary School Accessibility in Brazil

**Author:** Ana Paula Muto  
**Course:** Applied Quantitative Methods for Social Sciences 2  
**Date:** June 2026

## Overview

This project examines whether municipal state capacity explains variation in mean settlement-to-school distances across Brazilian municipalities. It uses georeferenced data from the Censo Escolar 2025 and GHS-SMOD 2020 settlement layers to calculate Euclidean distances from settlement centroids to the nearest secondary school, and estimates OLS and spatial models with two complementary operationalizations of state capacity: fiscal autonomy and a behavioral administrative capacity composite.

---

## Repository Structure

```
.
├── data/
│   ├── raw/          — raw input files (not tracked by Git)
│   └── processed/    — cleaned, analysis-ready files (not tracked by Git)
├── scripts/
│   ├── archive/      — exploratory scripts (not part of main pipeline)
│   └── ...           — numbered pipeline scripts (see below)
├── results/          — LaTeX tables and model output
├── outputs/          — figures and maps
└── README.md
```

---

## Script Pipeline

Run scripts in the following order to reproduce the analysis:

| Script | Description |
|--------|-------------|
| `00_functions_misc.R` | Shared functions, formula builders, helper utilities |
| `01_cleandata_cescola.R` | Clean Censo Escolar 2025 school locations |
| `02_cleandata_settlementsandboundaries.R` | Process GHS-SMOD settlement layer and municipality boundaries |
| `03_cleandata_finbra.R` | Clean FINBRA 2024 fiscal data; construct administrative capacity composite via PCA |
| `04_cleandata_controls.R` | Clean IBGE SIDRA socioeconomic controls |
| `06_cleandata_munic.R` | Clean IBGE MUNIC 2023 organizational capacity indicators |
| `11_calculationdistances.R` | Calculate Euclidean distances from settlement centroids to nearest secondary school |
| `21_mastermerge_mun.R` | Merge all municipal-level datasets into master analysis file |
| `31_models_main.R` | Estimate main OLS models (M1–M4) |
| `32_descriptivestat.R` | Descriptive statistics and summary tables |
| `33_descriptivemaps.R` | Maps of distance, fiscal autonomy, and capacity score |
| `81_robustcapag.R` | CAPAG validation; composite construction validation (PCA loadings, correlations) |
| `82_robustness.R` | Geographic fixed effects robustness (biome, region, state FE) |
| `83_robustness_spatial.R` | Spatial diagnostics (Moran's I, LM tests, SEM) |

### PE Paper Scripts (separate pipeline, `_pe` suffix)

| Script | Description |
|--------|-------------|
| `11_calculationdistances_pe.R` | Distance calculations for rural settlements only |
| `21_mastermerge_mun_pe.R` | Master merge for PE paper sample |
| `31_models_pe.R` | PE paper models (rural subsample) |
| `81_explore_pe.R` | Exploratory analysis for PE paper |

---

## Data Sources

### Censo Escolar 2025
- **Source:** INEP
- **URL:** https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar
- **Files:** `Tabela_Escola_2025.csv`, `Tabela_Matricula_2025.csv`
- **Note:** 5,334 schools (15%) excluded from distance analysis due to missing georeferencing

### GHS-SMOD — Global Human Settlement Layer
- **Source:** European Commission Joint Research Centre
- **URL:** https://ghsl.jrc.ec.europa.eu/ghs_smod2023.php
- **Product:** GHS-SMOD R2023A, Epoch 2020, 1km resolution, Mollweide projection
- **File:** `GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0.tif`
- **Settlement threshold:** SMOD class ≥ 12 (rural cluster and above)
- **Usage:** Settlement centroid extraction in script 02

### FINBRA — Finanças do Brasil 2024
- **Source:** Secretaria do Tesouro Nacional (STN) / SICONFI
- **URL:** https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra/finbra_list.jsf
- **Scope:** Municípios and Estados/DF, Exercício 2024
- **Files:**
  - `finbra2024_mun_IC.csv` — Receitas Orçamentárias (Anexo I-C)
  - `finbra2024_mun_IE.csv` — Despesas por Função (Anexo I-E)
  - `finbra2024_mun_IAB.csv` — Balanço Patrimonial DCA (Anexo I-AB)
  - `finbra2024_est_IC.csv` — Receitas Orçamentárias estados (Anexo I-C)
  - `finbra2024_est_IE.csv` — Despesas por Função estados (Anexo I-E)
  - `finbra2024_est_IAB.csv` — Balanço Patrimonial estados (Anexo I-AB)
- **Usage:** Fiscal autonomy (M1), administrative capacity composite (M2/M3), FPM dependence (M4)

### CAPAG — Capacidade de Pagamento 2025
- **Source:** Secretaria do Tesouro Nacional (STN)
- **URL:** https://www.tesourotransparente.gov.br/temas/estados-e-municipios/capag
- **Base year:** 2024 (February 2025 release)
- **Files:**
  - `capag-municipios-posicao-2025-fev-19.xlsx` — Municipal CAPAG grades
  - `capagdosestados2025.csv` — State CAPAG grades
- **Note:** Used as robustness check only (covers 65.9% of municipalities). Main capacity indicators built from FINBRA.

### IBGE SIDRA — Socioeconomic Controls
- **Source:** Instituto Brasileiro de Geografia e Estatística (IBGE)
- **URL:** https://sidra.ibge.gov.br
- **Access:** Pulled automatically via `sidrar` R package
- **Tables:**
  - Table 6579: População residente estimada (2025)
  - Table 5938: PIB municipal a preços correntes (2023)
  - Table 1301: Área territorial municipal (2010)

### IBGE MUNIC 2023 — Pesquisa de Informações Básicas Municipais
- **Source:** Instituto Brasileiro de Geografia e Estatística (IBGE)
- **URL:** https://www.ibge.gov.br/estatisticas/sociais/educacao/10586-pesquisa-de-informacoes-basicas-municipais.html
- **Usage:** Organizational capacity indicators (secretariat presence, administrative structure) — PE paper

### IBGE Biomas do Brasil
- **Source:** Instituto Brasileiro de Geografia e Estatística (IBGE)
- **URL:** https://geoftp.ibge.gov.br/informacoes_ambientais/estudos_ambientais/biomas/vetores/
- **Scale:** 1:250,000
- **File:** `lm_bioma_250.shp`
- **Biomes:** Amazônia, Caatinga, Cerrado, Mata Atlântica, Pampa, Pantanal
- **Usage:** Biome fixed effects; assigned via spatial join in script 02

### geobr — Brazilian Administrative Boundaries
- **Source:** IBGE via `geobr` R package
- **URL:** https://github.com/ipeaGIT/geobr
- **Year:** 2020
- **Access:** Pulled automatically via `geobr::read_municipality()` and `geobr::read_state()`
- **Usage:** Municipality boundaries for spatial weights matrix; IBGE codes as merge keys throughout pipeline

---

## R Dependencies

```r
# Core
tidyverse, data.table

# Spatial
sf, geobr, spdep, spatialreg

# Modelling
fixest, lmtest, sandwich

# Data access
sidrar

# Utilities
geobr, readxl, janitor
```

---

## Key Variables

| Variable | Description | Source |
|----------|-------------|--------|
| `log_mean_dist` | Log mean Euclidean distance (km) from settlement centroids to nearest secondary school | Censo Escolar 2025 + GHS-SMOD 2020 |
| `fiscal_autonomy_mun` | Own-source revenue / total revenue | FINBRA 2024 |
| `adm_capacity_score_mun` | PCA composite of budget execution, education spending share, inverted debt ratio | FINBRA 2024 |
| `fpm_dependence_mun` | FPM transfers / total revenue | FINBRA 2024 |
| `capag_numeric` | Official fiscal health grade (A=4 to D=1) | CAPAG 2025 |
| `log_pop_mun` | Log municipal population | IBGE SIDRA |
| `log_gdp_pc_mun` | Log GDP per capita | IBGE SIDRA |
| `log_area_mun` | Log municipal area | IBGE SIDRA |
| `urban_share_mun` | Urban population share | IBGE SIDRA |
| `log_dist_capital` | Log distance to state capital (km) | Calculated |
| `bioma` | Biome fixed effect (reference: Cerrado) | IBGE Biomas |

---

## Notes

- `data/raw/` and `data/processed/` are excluded from Git via `.gitignore`
- All outputs (tables, figures) are saved to `results/` and `outputs/`
- Standard errors clustered by state throughout unless noted
- Distances logged due to right skew; log transformation justified in appendix
- 280 municipalities (5%) have no secondary school and are excluded from distance analysis