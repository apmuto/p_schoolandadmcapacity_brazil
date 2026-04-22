Project AQMSS
Ana Paula Muto





\#1. Schools and Administrative Capacity in Brazil



\##2.  Project Structure

\- `data/raw/` — raw files (not tracked by Git)

\- `data/processed/` — cleaned analysis-ready files (not tracked by Git)

\- `scripts/` — R cleaning and analysis scripts

\- `outputs/` — tables and figures



\##3. Data Source


### Censo Escolar 2025
- Source: INEP
- https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar
- Files: Tabela_Escola_2025.csv, Tabela_Matricula_2025.csv

### GHSL - Global Human Settlement Layer
- Product: GHS-SMOD R2023A, Epoch 2020, 1km resolution, Mollweide
- Source: https://human-settlement.emergency.copernicus.eu
- File: GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0.tif

### GADM - Global Administrative Areas
- Version: 4.1, Brazil, GeoPackage
- Source: https://gadm.org
- File: gadm41_BRA.gpkg

### FINBRA - Finanças do Brasil 2024
- Source: Secretaria do Tesouro Nacional (STN) / SICONFI
- https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra/finbra_list.jsf
- Scope: Municípios and Estados/DF | Exercício: 2024
- Files:
  - finbra2024_mun_IC.csv — Receitas Orçamentárias (Anexo I-C)
  - finbra2024_mun_IE.csv — Despesas por Função (Anexo I-E)
  - finbra2024_mun_IAB.csv — Balanço Patrimonial DCA (Anexo I-AB)
  - finbra2024_est_IC.csv — Receitas Orçamentárias estados (Anexo I-C)
  - finbra2024_est_IE.csv — Despesas por Função estados (Anexo I-E)
  - finbra2024_est_IAB.csv — Balanço Patrimonial estados (Anexo I-AB)

### CAPAG - Capacidade de Pagamento 2025
- Source: Secretaria do Tesouro Nacional (STN)
- https://www.tesourotransparente.gov.br/temas/estados-e-municipios/capag
- Base year: 2024 (February 2025 release)
- Files:
  - capag-municipios-posicao-2025-fev-19.xlsx — Municipal CAPAG grades
  - capagdosestados2025.csv — State CAPAG grades
- Note: Used as robustness check variable only. Main administrative
  capacity indicators are built from FINBRA.

### IBGE SIDRA - Socioeconomic Controls
- Source: Instituto Brasileiro de Geografia e Estatística (IBGE)
- https://sidra.ibge.gov.br
- Pulled automatically via sidrar R package
- Tables used:
  - Table 6579: População residente estimada (most recent = 2025)
  - Table 5938: PIB municipal a preços correntes (most recent = 2023)
  - Table 1301: Área territorial municipal (2010)

### IBGE Biomas do Brasil
- Source: Instituto Brasileiro de Geografia e Estatística (IBGE)
- https://geoftp.ibge.gov.br/informacoes_ambientais/estudos_ambientais/biomas/vetores/
- Scale: 1:250,000
- File: lm_bioma_250.shp
- 6 biomes: Amazônia, Caatinga, Cerrado, Mata Atlântica, Pampa, Pantanal
