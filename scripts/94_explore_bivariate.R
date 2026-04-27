# ==================================================
# 94_explore_bivariate.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Explore bivariate relationships between
#       main model variables to inform model spec
# Input:  data/processed/master_main.csv
# Output: outputs/figures/
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)

# ============================================================
# CONSTANTS
# ============================================================

PATH_MAIN   <- "data/processed/master_main.csv"
PATH_FIGS   <- "outputs/figures/"

# Create output folder if it doesn't exist
dir.create(PATH_FIGS, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading master_main...\n")
main <- load_br_csv(PATH_MAIN)
report_dims(main, "master_main")

# ============================================================
# PREPARE MUNICIPALITY-LEVEL AGGREGATES
# Many relationships are better examined at municipality level
# to avoid pseudoreplication (many schools per municipality)
# ============================================================

cat("\nAggregating to municipality level...\n")

mun <- main %>%
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, SG_UF,
           bioma, TP_LOCALIZACAO) %>%
  summarise(
    n_schools            = n(),
    infra_ok_rate        = mean(infra_ok, na.rm = TRUE),
    edu_ok_rate          = mean(edu_ok, na.rm = TRUE),
    edu_score_mean       = mean(edu_score, na.rm = TRUE),
    adm_capacity_score_mun = first(adm_capacity_score_mun),
    fiscal_autonomy_mun  = first(fiscal_autonomy_mun),
    transfer_dependence_mun = first(transfer_dependence_mun),
    fpm_dependence_mun   = first(fpm_dependence_mun),
    dist_capital_km      = first(dist_capital_km),
    log_dist_capital     = first(log_dist_capital),
    log_pop_mun          = first(log_pop_mun),
    log_gdp_pc_mun       = first(log_gdp_pc_mun),
    urban_share_mun      = first(urban_share_mun),
    capag_numeric        = first(capag_numeric),
    .groups = "drop"
  ) %>%
  mutate(
    urban_rural = ifelse(urban_share_mun >= 0.5,
                         "Mostly urban", "Mostly rural"),
    bioma = factor(bioma, levels = c("Amazonia", "Caatinga",
                                      "Cerrado", "Mata Atlantica",
                                      "Pampa", "Pantanal"))
  )

cat("Municipality-level dataset:", nrow(mun), "municipalities\n")

# ============================================================
# PLOT 1: ADM CAPACITY vs EDU SCORE
# Core relationship of interest
# ============================================================

cat("\nPlot 1: Adm capacity vs edu score...\n")

p1 <- ggplot(mun %>% filter(!is.na(adm_capacity_score_mun)),
             aes(x = adm_capacity_score_mun,
                 y = edu_score_mean)) +
  geom_point(aes(color = bioma), alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", color = "black",
              linewidth = 0.8, se = TRUE) +
  geom_smooth(method = "loess", color = "red",
              linewidth = 0.8, se = FALSE, linetype = "dashed") +
  scale_color_brewer(palette = "Set2", name = "Biome") +
  labs(
    title = "Administrative capacity and educational amenities",
    subtitle = "Municipality-level means | Black = linear fit | Red = LOESS",
    x = "Administrative capacity score (municipal)",
    y = "Mean educational amenities score (0-7)",
    caption = "Source: Censo Escolar 2025, FINBRA 2024"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(paste0(PATH_FIGS, "01_adm_capacity_vs_edu_score.png"),
       p1, width = 8, height = 6, dpi = 150)
cat("Saved plot 1\n")

# ============================================================
# PLOT 2: ADM CAPACITY vs INFRA OK RATE
# ============================================================

cat("Plot 2: Adm capacity vs infra ok rate...\n")

p2 <- ggplot(mun %>% filter(!is.na(adm_capacity_score_mun)),
             aes(x = adm_capacity_score_mun,
                 y = infra_ok_rate)) +
  geom_point(aes(color = bioma), alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", color = "black",
              linewidth = 0.8, se = TRUE) +
  geom_smooth(method = "loess", color = "red",
              linewidth = 0.8, se = FALSE, linetype = "dashed") +
  scale_color_brewer(palette = "Set2", name = "Biome") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Administrative capacity and basic infrastructure",
    subtitle = "Municipality-level means | Black = linear fit | Red = LOESS",
    x = "Administrative capacity score (municipal)",
    y = "Share of schools with full infrastructure (%)",
    caption = "Source: Censo Escolar 2025, FINBRA 2024"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(paste0(PATH_FIGS, "02_adm_capacity_vs_infra_ok.png"),
       p2, width = 8, height = 6, dpi = 150)
cat("Saved plot 2\n")

# ============================================================
# PLOT 3: DISTANCE TO CAPITAL vs EDU SCORE
# ============================================================

cat("Plot 3: Distance to capital vs edu score...\n")

p3 <- ggplot(mun,
             aes(x = dist_capital_km,
                 y = edu_score_mean)) +
  geom_point(aes(color = bioma), alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", color = "black",
              linewidth = 0.8, se = TRUE) +
  geom_smooth(method = "loess", color = "red",
              linewidth = 0.8, se = FALSE, linetype = "dashed") +
  scale_color_brewer(palette = "Set2", name = "Biome") +
  labs(
    title = "Distance to state capital and educational amenities",
    subtitle = "Municipality-level means | Black = linear fit | Red = LOESS",
    x = "Distance to state capital (km)",
    y = "Mean educational amenities score (0-7)",
    caption = "Source: Censo Escolar 2025, FINBRA 2024"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(paste0(PATH_FIGS, "03_dist_capital_vs_edu_score.png"),
       p3, width = 8, height = 6, dpi = 150)
cat("Saved plot 3\n")

# ============================================================
# PLOT 4: EDU SCORE BY BIOME — BOX PLOT
# ============================================================

cat("Plot 4: Edu score by biome...\n")

p4 <- ggplot(main %>% filter(!is.na(bioma)),
             aes(x = reorder(bioma, edu_score, median),
                 y = edu_score,
                 fill = bioma)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "Educational amenities score by biome",
    subtitle = "School level | Median and interquartile range",
    x = NULL,
    y = "Educational amenities score (0-7)",
    caption = "Source: Censo Escolar 2025"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave(paste0(PATH_FIGS, "04_edu_score_by_biome.png"),
       p4, width = 8, height = 5, dpi = 150)
cat("Saved plot 4\n")

# ============================================================
# PLOT 5: INFRA OK BY REGION — BAR CHART
# ============================================================

cat("Plot 5: Infra ok by region...\n")

region_summary <- main %>%
  mutate(NO_REGIAO = case_when(
    CO_UF %in% c(11,12,13,14,15,16,17) ~ "Norte",
    CO_UF %in% c(21,22,23,24,25,26,27,28,29) ~ "Nordeste",
    CO_UF %in% c(31,32,33,35) ~ "Sudeste",
    CO_UF %in% c(41,42,43) ~ "Sul",
    CO_UF %in% c(50,51,52,53) ~ "Centro-Oeste",
    TRUE ~ "Other"
  )) %>%
  group_by(NO_REGIAO) %>%
  summarise(
    infra_ok_rate = mean(infra_ok, na.rm = TRUE),
    edu_ok_rate   = mean(edu_ok,   na.rm = TRUE),
    n             = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(infra_ok_rate, edu_ok_rate),
               names_to = "indicator",
               values_to = "rate") %>%
  mutate(indicator = recode(indicator,
    "infra_ok_rate" = "Basic infrastructure",
    "edu_ok_rate"   = "Educational amenities"
  ))

p5 <- ggplot(region_summary,
             aes(x = reorder(NO_REGIAO, rate),
                 y = rate, fill = indicator)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1", name = NULL) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(
    title = "School infrastructure by region",
    subtitle = "Share of secondary schools meeting all requirements",
    x = NULL,
    y = "Share of schools (%)",
    caption = "Source: Censo Escolar 2025"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(paste0(PATH_FIGS, "05_infra_edu_by_region.png"),
       p5, width = 8, height = 5, dpi = 150)
cat("Saved plot 5\n")

# ============================================================
# PLOT 6: CORRELATION HEATMAP — MAIN MODEL VARIABLES
# ============================================================

cat("Plot 6: Correlation heatmap...\n")

cor_vars <- main %>%
  select(
    edu_score, infra_ok,
    adm_capacity_score_mun, fiscal_autonomy_mun,
    transfer_dependence_mun, fpm_dependence_mun,
    dist_capital_km, log_pop_mun, log_gdp_pc_mun,
    log_area_mun, urban_share_mun
  ) %>%
  rename(
    `Edu score`         = edu_score,
    `Infra ok`          = infra_ok,
    `Adm capacity`      = adm_capacity_score_mun,
    `Fiscal autonomy`   = fiscal_autonomy_mun,
    `Transfer dep.`     = transfer_dependence_mun,
    `FPM dep.`          = fpm_dependence_mun,
    `Dist. capital`     = dist_capital_km,
    `Log population`    = log_pop_mun,
    `Log GDP pc`        = log_gdp_pc_mun,
    `Log area`          = log_area_mun,
    `Urban share`       = urban_share_mun
  )

cor_matrix <- cor(cor_vars, use = "pairwise.complete.obs")
cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Var1 = Var1, Var2 = Var2, correlation = Freq)

p6 <- ggplot(cor_long, aes(x = Var1, y = Var2,
                            fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)),
            size = 2.5, color = "black") +
  scale_fill_gradient2(low = "#d73027", mid = "white",
                       high = "#1a9850", midpoint = 0,
                       limits = c(-1, 1),
                       name = "Correlation") +
  labs(
    title = "Correlation matrix — main model variables",
    subtitle = "Pairwise complete observations",
    x = NULL, y = NULL,
    caption = "Source: Censo Escolar 2025, FINBRA 2024"
  ) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

ggsave(paste0(PATH_FIGS, "06_correlation_heatmap.png"),
       p6, width = 9, height = 8, dpi = 150)
cat("Saved plot 6\n")

# ============================================================
# PLOT 7: URBAN VS RURAL — EDU SCORE DISTRIBUTIONS
# ============================================================

cat("Plot 7: Urban vs rural edu score...\n")

p7 <- ggplot(main %>%
               mutate(localizacao = ifelse(TP_LOCALIZACAO == 1,
                                           "Urban", "Rural")),
             aes(x = edu_score, fill = localizacao)) +
  geom_histogram(binwidth = 1, position = "dodge",
                 alpha = 0.8) +
  scale_fill_manual(values = c("Urban" = "#2166ac",
                                "Rural" = "#d6604d"),
                    name = NULL) +
  labs(
    title = "Educational amenities score: urban vs rural schools",
    subtitle = "School level distribution",
    x = "Educational amenities score (0-7)",
    y = "Number of schools",
    caption = "Source: Censo Escolar 2025"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

ggsave(paste0(PATH_FIGS, "07_edu_score_urban_rural.png"),
       p7, width = 8, height = 5, dpi = 150)
cat("Saved plot 7\n")

# ============================================================
# PLOT 8: ADM CAPACITY vs CAPAG — VALIDATION CHECK
# Do our FINBRA composite and CAPAG agree?
# ============================================================

cat("Plot 8: FINBRA composite vs CAPAG validation...\n")

p8 <- ggplot(mun %>%
               filter(!is.na(adm_capacity_score_mun),
                      !is.na(capag_numeric)) %>%
               mutate(capag_label = case_when(
                 capag_numeric == 4 ~ "A",
                 capag_numeric == 3 ~ "B",
                 capag_numeric == 2 ~ "C",
                 capag_numeric == 1 ~ "D"
               )),
             aes(x = factor(capag_label,
                            levels = c("A","B","C","D")),
                 y = adm_capacity_score_mun,
                 fill = factor(capag_label))) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("A" = "#1a9850",
                                "B" = "#91cf60",
                                "C" = "#fee08b",
                                "D" = "#d73027")) +
  labs(
    title = "FINBRA composite score vs CAPAG grade",
    subtitle = "Validation: do both measures agree?",
    x = "CAPAG grade (A = best, D = worst)",
    y = "Administrative capacity score (FINBRA)",
    caption = "Source: FINBRA 2024, STN CAPAG 2025"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave(paste0(PATH_FIGS, "08_finbra_vs_capag.png"),
       p8, width = 7, height = 5, dpi = 150)
cat("Saved plot 8\n")

# ============================================================
# SUMMARY STATISTICS TABLE
# ============================================================

cat("\n--- Summary statistics (main model variables) ---\n")

summary_vars <- c(
  "edu_score", "infra_ok", "edu_ok",
  "adm_capacity_score_mun", "adm_capacity_score_est",
  "fiscal_autonomy_mun", "transfer_dependence_mun",
  "fpm_dependence_mun", "dist_capital_km",
  "log_pop_mun", "log_gdp_pc_mun", "urban_share_mun"
)

summary_tbl <- map_dfr(summary_vars, function(v) {
  x <- main[[v]]
  tibble(
    variable = v,
    n        = sum(!is.na(x)),
    mean     = round(mean(x, na.rm = TRUE), 3),
    sd       = round(sd(x,   na.rm = TRUE), 3),
    min      = round(min(x,  na.rm = TRUE), 3),
    median   = round(median(x, na.rm = TRUE), 3),
    max      = round(max(x,  na.rm = TRUE), 3),
    na       = sum(is.na(x))
  )
})

print(summary_tbl, n = Inf)

cat("\nAll plots saved to", PATH_FIGS, "\n")
cat("\nScript 94 complete.\n")