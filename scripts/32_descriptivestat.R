# ==================================================
# 32_descriptivestat.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Produce descriptive statistics for paper:
#       - Table 1: Summary statistics all variables
#       - Table 2: Municipality profile by school
#                  presence
#       - Table 3: Accessibility by region and biome
#       - Key figures for paper
# Input:  data/processed/master_mun_main.csv
# Output: outputs/tables/
#         outputs/figures/
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(modelsummary)
library(kableExtra)

# ============================================================
# CONSTANTS
# ============================================================

PATH_MAIN   <- "data/processed/master_mun_main.csv"
PATH_TABLES <- "outputs/tables/"
PATH_FIGS   <- "outputs/figures/"

dir.create(PATH_TABLES, recursive = TRUE,
           showWarnings = FALSE)
dir.create(PATH_FIGS,   recursive = TRUE,
           showWarnings = FALSE)

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading master_mun_main...\n")
main <- load_br_csv(PATH_MAIN) %>%
  mutate(
    CO_MUNICIPIO = as.character(CO_MUNICIPIO),
    CO_UF = as.integer(substr(CO_MUNICIPIO, 1, 2)),
    regiao = case_when(
      CO_UF %in% c(11,12,13,14,15,16,17) ~ "Norte",
      CO_UF %in% c(21,22,23,24,25,26,27,28,29) ~ "Nordeste",
      CO_UF %in% c(31,32,33,35) ~ "Sudeste",
      CO_UF %in% c(41,42,43) ~ "Sul",
      CO_UF %in% c(50,51,52,53) ~ "Centro-Oeste",
      TRUE ~ NA_character_
    ),
    periphery = regiao %in% c("Norte", "Nordeste")
  )

report_dims(main, "master_mun_main")

# ============================================================
# TABLE 1: SUMMARY STATISTICS
# All main model variables
# ============================================================

cat("\nBuilding Table 1: Summary statistics...\n")

sum_vars <- list(
  # Outcomes
  list(var="mean_dist_km",
       label="Mean distance to school (km)"),
  list(var="log_mean_dist",
       label="Log mean distance to school"),
  list(var="pct_over_10km",
       label="% settlements >10km from school"),
  list(var="pct_over_30km",
       label="% settlements >30km from school"),
  list(var="schools_per_10k",
       label="Schools per 10,000 inhabitants"),
  list(var="log_schools_per_10k",
       label="Log schools per 10,000 inhabitants"),
  list(var="has_school",
       label="Has secondary school (binary)"),
  
  # Main IVs
  list(var="adm_capacity_score_mun",
       label="Adm. capacity score (behavioral)"),
  list(var="fiscal_autonomy_mun",
       label="Fiscal autonomy (own rev/total rev)"),
  list(var="fpm_dependence_mun",
       label="FPM dependence (FPM/total rev)"),
  list(var="transfer_dependence_mun",
       label="Transfer dependence"),
  list(var="edu_share_mun",
       label="Education spending share"),
  list(var="budget_execution_mun",
       label="Budget execution rate"),
  list(var="debt_ratio_mun",
       label="Debt ratio"),
  
  # State capacity
  list(var="adm_capacity_score_est",
       label="State adm. capacity score"),
  list(var="fiscal_autonomy_est",
       label="State fiscal autonomy"),
  
  # Controls
  list(var="pop_mun",
       label="Population"),
  list(var="log_pop_mun",
       label="Log population"),
  list(var="log_gdp_pc_mun",
       label="Log GDP per capita"),
  list(var="log_area_mun",
       label="Log area (km²)"),
  list(var="urban_share_mun",
       label="Urban share"),
  list(var="dist_capital_km",
       label="Distance to state capital (km)"),
  
  # Robustness
  list(var="capag_numeric",
       label="CAPAG grade (1-4)")
)

table1 <- map_dfr(sum_vars, function(v) {
  x <- main[[v$var]]
  tibble(
    Variable = v$label,
    N        = sum(!is.na(x)),
    Mean     = round(mean(x, na.rm = TRUE), 3),
    SD       = round(sd(x,   na.rm = TRUE), 3),
    Min      = round(min(x,  na.rm = TRUE), 3),
    P25      = round(quantile(x, 0.25, na.rm = TRUE), 3),
    Median   = round(median(x, na.rm = TRUE), 3),
    P75      = round(quantile(x, 0.75, na.rm = TRUE), 3),
    Max      = round(max(x,  na.rm = TRUE), 3),
    Missing  = sum(is.na(x))
  )
})

print(table1, n = Inf)
fwrite(table1, paste0(PATH_TABLES, "table1_summary_stats.csv"))
cat("Saved Table 1\n")

# ============================================================
# TABLE 2: MUNICIPALITY PROFILE BY SCHOOL PRESENCE
# ============================================================

cat("\nBuilding Table 2: Profile by school presence...\n")

table2 <- main %>%
  group_by(has_school) %>%
  summarise(
    n                 = n(),
    mean_pop          = round(mean(pop_mun, na.rm = TRUE)),
    median_pop        = round(median(pop_mun, na.rm = TRUE)),
    mean_gdp_pc       = round(mean(exp(log_gdp_pc_mun), na.rm = TRUE)),
    mean_fiscal_auto  = round(mean(fiscal_autonomy_mun, na.rm = TRUE), 3),
    mean_fpm_dep      = round(mean(fpm_dependence_mun, na.rm = TRUE), 3),
    mean_adm_cap      = round(mean(adm_capacity_score_mun, na.rm = TRUE), 3),
    mean_dist_capital = round(mean(dist_capital_km, na.rm = TRUE), 1),
    mean_urban_share  = round(mean(urban_share_mun, na.rm = TRUE), 3),
    pct_periphery     = round(100 * mean(periphery, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(has_school = ifelse(has_school, "Has school", "No school")) %>%
  rename(
    " "                  = has_school,
    "N"                  = n,
    "Mean pop."          = mean_pop,
    "Median pop."        = median_pop,
    "Mean GDP pc"        = mean_gdp_pc,
    "Fiscal autonomy"    = mean_fiscal_auto,
    "FPM dependence"     = mean_fpm_dep,
    "Adm. capacity"      = mean_adm_cap,
    "Dist. capital (km)" = mean_dist_capital,
    "Urban share"        = mean_urban_share,
    "% periphery"        = pct_periphery
  )

kable(table2,
      format   = "latex",
      booktabs = TRUE,
      caption  = "Municipal profile by secondary school presence",
      label    = "tab:profile") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  save_kable(paste0(PATH_TABLES, "tab_profile.tex"))

cat("Saved tab_profile.tex\n")

# ============================================================
# TABLE 3: ACCESSIBILITY BY REGION AND BIOME
# ============================================================

cat("\nBuilding Table 3: Accessibility by region...\n")

# Pre-compute medians separately to avoid name-masking in summarise
region_medians <- main %>%
  group_by(regiao) %>%
  summarise(median_dist_km = round(median(mean_dist_km, na.rm=TRUE), 2),
            .groups = "drop")

table3_region <- main %>%
  group_by(regiao) %>%
  summarise(
    n_mun              = n(),
    pct_no_school      = round(100 * mean(!has_school), 1),
    mean_dist_km       = round(mean(mean_dist_km, na.rm=TRUE), 2),
    pct_over_10km      = round(100 * mean(pct_over_10km, na.rm=TRUE), 1),
    mean_schools_10k   = round(mean(schools_per_10k, na.rm=TRUE), 2),
    mean_fpm_dep       = round(mean(fpm_dependence_mun, na.rm=TRUE), 3),
    mean_fiscal_auto   = round(mean(fiscal_autonomy_mun, na.rm=TRUE), 3),
    .groups = "drop"
  ) %>%
  left_join(region_medians, by = "regiao") %>%
  select(regiao, n_mun, pct_no_school, mean_dist_km, median_dist_km,
         pct_over_10km, mean_schools_10k, mean_fpm_dep, mean_fiscal_auto) %>%
  arrange(desc(mean_dist_km))

cat("\nBy region:\n")
print(table3_region)
fwrite(table3_region,
       paste0(PATH_TABLES, "table3a_access_by_region.csv"))

# Pre-compute biome medians separately
biome_medians <- main %>%
  filter(!is.na(bioma), bioma != "") %>%
  group_by(bioma) %>%
  summarise(median_dist_km = round(median(mean_dist_km, na.rm=TRUE), 2),
            .groups = "drop")

table3_biome <- main %>%
  filter(!is.na(bioma), bioma != "") %>%
  group_by(bioma) %>%
  summarise(
    n_mun            = n(),
    pct_no_school    = round(100 * mean(!has_school), 1),
    mean_dist_km     = round(mean(mean_dist_km, na.rm=TRUE), 2),
    pct_over_10km    = round(100 * mean(pct_over_10km, na.rm=TRUE), 1),
    mean_schools_10k = round(mean(schools_per_10k, na.rm=TRUE), 2),
    .groups = "drop"
  ) %>%
  left_join(biome_medians, by = "bioma") %>%
  select(bioma, n_mun, pct_no_school, mean_dist_km, median_dist_km,
         pct_over_10km, mean_schools_10k) %>%
  arrange(desc(mean_dist_km))

cat("\nBy biome:\n")
print(table3_biome)
fwrite(table3_biome,
       paste0(PATH_TABLES, "table3b_access_by_biome.csv"))
cat("Saved Table 3\n")

# ============================================================
# TABLE 4: URBAN TYPE COMPARISON
# ============================================================

cat("\nBuilding Table 4: By urban type...\n")

table4 <- main %>%
  filter(!is.na(urban_type)) %>%
  group_by(urban_type) %>%
  summarise(
    n                = n(),
    pct_no_school    = round(100 * mean(!has_school), 1),
    mean_dist_km     = round(mean(mean_dist_km,
                                  na.rm=TRUE), 2),
    mean_schools_10k = round(mean(schools_per_10k,
                                  na.rm=TRUE), 2),
    mean_pop         = round(mean(pop_mun, na.rm=TRUE)),
    mean_fpm_dep     = round(mean(fpm_dependence_mun,
                                  na.rm=TRUE), 3),
    mean_fiscal_auto = round(mean(fiscal_autonomy_mun,
                                  na.rm=TRUE), 3),
    .groups = "drop"
  )

print(table4)
fwrite(table4,
       paste0(PATH_TABLES, "table4_urban_type.csv"))
cat("Saved Table 4\n")

# ============================================================
# FIGURE 1: DISTRIBUTION OF KEY OUTCOMES
# ============================================================

cat("\nBuilding Figure 1: Outcome distributions...\n")

p1 <- main %>%
  filter(!is.na(mean_dist_km)) %>%
  mutate(dist_cap = pmin(mean_dist_km, 30)) %>%
  ggplot(aes(x = dist_cap)) +
  geom_histogram(bins = 60, fill = "#2166ac",
                 color = "white", alpha = 0.8) +
  geom_vline(xintercept = c(5, 10, 20),
             linetype = "dashed",
             color = c("#d73027","#f46d43","#fdae61"),
             linewidth = 0.8) +
  annotate("text", x = c(5.3,10.3,20.3),
           y = Inf, vjust = 2, hjust = 0,
           label = c("5km","10km","20km"),
           color = c("#d73027","#f46d43","#fdae61"),
           size = 3) +
  scale_x_continuous(breaks = seq(0,30,5)) +
  labs(
    title = "Distribution of mean settlement-to-school distance",
    subtitle = paste0("Municipality level (n=",
                      sum(!is.na(main$mean_dist_km)),
                      ") | Capped at 30km"),
    x = "Mean distance to nearest school (km)",
    y = "Number of municipalities",
    caption = "Source: Censo Escolar 2025, GHSL-SMOD 2020"
  ) +
  theme_minimal(base_size = 11)

ggsave(paste0(PATH_FIGS, "31_01_dist_distribution.pdf"),
       p1, width = 8, height = 5)
cat("Saved Figure 1\n")

# ============================================================
# FIGURE 2: ACCESSIBILITY BY REGION — BOX PLOT
# ============================================================

cat("Building Figure 2: Distance by region...\n")

p2 <- main %>%
  filter(!is.na(mean_dist_km),
         !is.na(regiao),
         mean_dist_km <= 30) %>%
  ggplot(aes(x = reorder(regiao,
                         mean_dist_km,
                         median),
             y = mean_dist_km,
             fill = regiao)) +
  geom_boxplot(outlier.size = 0.5,
               outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "School accessibility by region",
    subtitle = "Mean settlement-to-school distance | Capped at 30km",
    x = NULL,
    y = "Mean distance to nearest school (km)",
    caption = "Source: Censo Escolar 2025, GHSL-SMOD 2020"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave(paste0(PATH_FIGS, "31_02_dist_by_region.pdf"),
       p2, width = 8, height = 5)
cat("Saved Figure 2\n")

# ============================================================
# FIGURE 3: FISCAL CAPACITY DISTRIBUTIONS
# ============================================================

cat("Building Figure 3: Fiscal capacity distributions...\n")

p3 <- main %>%
  filter(!is.na(fpm_dependence_mun),
         !is.na(fiscal_autonomy_mun)) %>%
  select(fpm_dependence_mun, fiscal_autonomy_mun,
         adm_capacity_score_mun) %>%
  pivot_longer(everything(),
               names_to = "indicator",
               values_to = "value") %>%
  mutate(indicator = recode(indicator,
                            "fpm_dependence_mun"     = "FPM dependence",
                            "fiscal_autonomy_mun"    = "Fiscal autonomy",
                            "adm_capacity_score_mun" = "Adm. capacity score"
  )) %>%
  ggplot(aes(x = value, fill = indicator)) +
  geom_histogram(bins = 50, color = "white",
                 alpha = 0.8) +
  facet_wrap(~indicator, scales = "free") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Distribution of fiscal capacity indicators",
    subtitle = "Municipal level (n≈5,533)",
    x = NULL, y = "Number of municipalities",
    caption = "Source: FINBRA 2024"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

ggsave(paste0(PATH_FIGS, "31_03_fiscal_distributions.pdf"),
       p3, width = 9, height = 4)
cat("Saved Figure 3\n")

# ============================================================
# FIGURE 4: FPM DEPENDENCE vs DISTANCE — SCATTER
# ============================================================

cat("Building Figure 4: FPM vs distance scatter...\n")

# Figure 4: FPM vs distance — separate line per biome
# Figure 4: OLS lines per biome — Simpson's paradox check
p4 <- main %>%
  filter(!is.na(fpm_dependence_mun),
         !is.na(mean_dist_km),
         !is.na(bioma),
         mean_dist_km <= 20) %>%
  ggplot(aes(x = fpm_dependence_mun,
             y = mean_dist_km,
             color = bioma,
             fill  = bioma)) +
  geom_smooth(method = "lm", se = TRUE,
              linewidth = 0.9, alpha = 0.12) +
  scale_color_brewer(palette = "Dark2", name = "Biome") +
  scale_fill_brewer(palette = "Dark2",  name = "Biome") +
  labs(
    title    = "FPM dependence and school accessibility by biome",
    subtitle = "Separate OLS lines per biome. Shaded = 95% CI.",
    x        = "FPM dependence (FPM / total revenue)",
    y        = "Mean settlement-to-school distance (km)",
    caption  = "Source: Censo Escolar 2025, FINBRA 2024, GHSL-SMOD 2020."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(color = "grey40", size = 9),
    plot.caption     = element_text(color = "grey55", size = 7),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

ggsave(paste0(PATH_FIGS, "31_04_fpm_vs_distance.pdf"),
       p4, width = 8, height = 5)
cat("Saved Figure 4\n")

# ============================================================
# FIGURE 5: POPULATION THRESHOLD
# ============================================================

cat("Building Figure 5: Population threshold...\n")

p5 <- main %>%
  mutate(pop_bin = cut(pop_mun,
                       breaks = c(0,1000,2000,3000,
                                  5000,10000,20000,
                                  50000,Inf),
                       labels = c("<1k","1-2k","2-3k",
                                  "3-5k","5-10k","10-20k",
                                  "20-50k","50k+"))) %>%
  group_by(pop_bin) %>%
  summarise(
    n = n(),
    pct_school = 100 * mean(has_school, na.rm=TRUE),
    mean_dist  = mean(mean_dist_km, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(pop_bin)) %>%
  ggplot(aes(x = pop_bin, y = pct_school)) +
  geom_col(fill = "#2166ac", alpha = 0.8) +
  geom_hline(yintercept = 95, linetype = "dashed",
             color = "red") +
  annotate("text", x = 1, y = 96,
           label = "95% threshold", hjust = 0,
           color = "red", size = 3) +
  scale_y_continuous(limits = c(0, 101)) +
  labs(
    title = "School presence by population size",
    subtitle = "% of municipalities with at least one secondary school",
    x = "Municipality population",
    y = "% with secondary school",
    caption = "Source: Censo Escolar 2025, IBGE SIDRA 2025"
  ) +
  theme_minimal(base_size = 11)

ggsave(paste0(PATH_FIGS, "31_05_population_threshold.pdf"),
       p5, width = 8, height = 5)
cat("Saved Figure 5\n")

# ============================================================
# FIGURE 6: BIOME COMPARISON
# ============================================================

cat("Building Figure 6: Distance by biome...\n")

p6 <- main %>%
  filter(!is.na(bioma), bioma != "",
         !is.na(mean_dist_km),
         mean_dist_km <= 20) %>%
  ggplot(aes(x = reorder(bioma,
                         mean_dist_km,
                         median),
             y = mean_dist_km,
             fill = bioma)) +
  geom_boxplot(outlier.size = 0.5,
               outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "School accessibility by biome",
    subtitle = "Mean settlement-to-school distance | Capped at 20km",
    x = NULL,
    y = "Mean distance to nearest school (km)",
    caption = "Source: Censo Escolar 2025, GHSL-SMOD 2020, IBGE"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave(paste0(PATH_FIGS, "31_06_dist_by_biome.pdf"),
       p6, width = 8, height = 5)
cat("Saved Figure 6\n")
# ============================================================
# FIGURE 7: IV COMPARISON — Fiscal autonomy vs composite
# Side-by-side density by region
# ============================================================

cat("\nBuilding Figure 7: IV comparison...\n")

iv_long <- main %>%
  filter(!is.na(fiscal_autonomy_mun),
         !is.na(adm_capacity_score_mun),
         !is.na(regiao)) %>%
  select(regiao, fiscal_autonomy_mun, adm_capacity_score_mun) %>%
  pivot_longer(
    cols = c(fiscal_autonomy_mun, adm_capacity_score_mun),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = case_when(
      indicator == "fiscal_autonomy_mun"    ~ "M1: Fiscal autonomy\n(own revenue / total revenue)",
      indicator == "adm_capacity_score_mun" ~ "M2-M4: Behavioral composite\n(higher = worse capacity)"
    ),
    regiao = factor(regiao,
                    levels = c("Norte", "Nordeste",
                               "Centro-Oeste", "Sul", "Sudeste"))
  )

p7 <- ggplot(iv_long,
             aes(x = value, fill = regiao, color = regiao)) +
  geom_density(alpha = 0.3, linewidth = 0.5) +
  facet_wrap(~ indicator, scales = "free_x") +
  scale_fill_brewer(palette = "Set1", name = "Region") +
  scale_color_brewer(palette = "Set1", name = "Region") +
  labs(
    title   = "Distribution of capacity indicators by region",
    x       = NULL,
    y       = "Density",
    caption = "Source: FINBRA 2024 (STN)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.caption    = element_text(color = "grey55", size = 7),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text      = element_text(face = "bold", size = 9)
  )

ggsave(paste0(PATH_FIGS, "31_07_iv_comparison.pdf"),
       p7, width = 10, height = 5)
cat("Saved Figure 7\n")

# ============================================================
# PRINT FINAL SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("DESCRIPTIVE SUMMARY\n")
cat("============================================================\n")
cat("Total municipalities:", nrow(main), "\n")
cat("With secondary school:",
    sum(main$has_school, na.rm=TRUE), "\n")
cat("Without secondary school:",
    sum(!main$has_school, na.rm=TRUE), "\n")
cat("With settlement data:",
    sum(!is.na(main$mean_dist_km)), "\n")
cat("Mean distance to school:",
    round(mean(main$mean_dist_km, na.rm=TRUE), 2),
    "km\n")
cat("Municipalities >10km mean distance:",
    sum(main$mean_dist_km > 10, na.rm=TRUE), "\n")
cat("Municipalities >30km mean distance:",
    sum(main$mean_dist_km > 30, na.rm=TRUE), "\n")

cat("\nTables saved to:", PATH_TABLES, "\n")
cat("Figures saved to:", PATH_FIGS, "\n")
# ==============================================================================
# FIGURE: SCATTER — Fiscal autonomy vs behavioral composite
# ==============================================================================
# compute region means
region_means <- scatter_data %>%
  group_by(regiao) %>%
  summarise(
    x = mean(fiscal_autonomy_mun, na.rm = TRUE),
    y = mean(adm_capacity_score_mun, na.rm = TRUE)
  )

ggplot(scatter_data,
       aes(x = fiscal_autonomy_mun,
           y = adm_capacity_score_mun)) +
  geom_point(alpha = 0.15, size = 0.6, color = "grey60") +
  geom_smooth(method = "lm", se = FALSE,
              color = "grey30", linewidth = 0.7,
              linetype = "dashed") +
  geom_point(data = region_means,
             aes(x = x, y = y, color = regiao),
             size = 4, shape = 18) +
  geom_text(data = region_means,
            aes(x = x, y = y, label = regiao, color = regiao),
            nudge_y = 0.15, size = 3, fontface = "bold") +
  scale_color_brewer(palette = "Set1", guide = "none") +
  annotate("text", x = 0.45, y = 7.5,
           label = paste0("r = ", r_val),
           size = 3.5, color = "grey30", hjust = 1) +
  labs(
    title   = "Fiscal autonomy vs behavioral composite",
    x       = "Fiscal autonomy (own revenue / total revenue)",
    y       = "Behavioral composite (higher = worse capacity)",
    caption = "Source: FINBRA 2024 (STN). Each point is a municipality. Diamonds show regional means."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.caption     = element_text(color = "grey55", size = 7),
    panel.grid.minor = element_blank()
  )
# ============================================================
# APPENDIX: DV TRANSFORMATION DIAGNOSTICS
# Justifies choice of log(mean_dist_km) over raw distance
# Saved to outputs/figures/appendix/
# ============================================================

cat("\nBuilding appendix diagnostics...\n")

PATH_APPENDIX <- "outputs/figures/appendix/"
dir.create(PATH_APPENDIX, recursive = TRUE,
           showWarnings = FALSE)

diag_data <- main %>%
  filter(!is.na(mean_dist_km),
         !is.na(log_mean_dist),
         has_school == TRUE)

# --- A1: Raw vs log DV distributions ---
p_raw <- ggplot(diag_data, aes(x = mean_dist_km)) +
  geom_histogram(bins = 60, fill = "steelblue",
                 color = "white", alpha = 0.8) +
  labs(
    title    = "Raw DV: mean settlement-to-school distance",
    subtitle = paste0("n = ", nrow(diag_data),
                      " | Skewness = ",
                      round(moments::skewness(
                        diag_data$mean_dist_km), 2)),
    x = "Mean distance (km)",
    y = "Municipalities"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_log <- ggplot(diag_data, aes(x = log_mean_dist)) +
  geom_histogram(bins = 60, fill = "steelblue",
                 color = "white", alpha = 0.8) +
  labs(
    title    = "Log DV: log mean settlement-to-school distance",
    subtitle = paste0("n = ", nrow(diag_data),
                      " | Skewness = ",
                      round(moments::skewness(
                        diag_data$log_mean_dist), 2)),
    x = "Log mean distance",
    y = "Municipalities"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_dv_compare <- gridExtra::grid.arrange(p_raw, p_log, ncol = 2)
ggsave(paste0(PATH_APPENDIX, "appendix_a1_dv_distributions.pdf"),
       p_dv_compare, width = 10, height = 4)
cat("Saved appendix_a1_dv_distributions.pdf\n")

# --- A2: Q-Q plots raw vs log ---
p_qq_raw <- ggplot(diag_data, aes(sample = mean_dist_km)) +
  stat_qq(color = "steelblue", alpha = 0.4, size = 0.6) +
  stat_qq_line(color = "grey30", linewidth = 0.7) +
  labs(
    title    = "Q-Q plot: raw distance",
    subtitle = "Departure from normality",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_qq_log <- ggplot(diag_data, aes(sample = log_mean_dist)) +
  stat_qq(color = "steelblue", alpha = 0.4, size = 0.6) +
  stat_qq_line(color = "grey30", linewidth = 0.7) +
  labs(
    title    = "Q-Q plot: log distance",
    subtitle = "Closer to normality after transformation",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_qq_compare <- gridExtra::grid.arrange(p_qq_raw, p_qq_log,
                                        ncol = 2)
ggsave(paste0(PATH_APPENDIX, "appendix_a2_qq_plots.pdf"),
       p_qq_compare, width = 10, height = 4)
cat("Saved appendix_a2_qq_plots.pdf\n")

# --- A3: Residual diagnostics from main model (m2_main equivalent) ---
# Fit lm() version for base R diagnostic plots
lm_diag <- lm(log_mean_dist ~ adm_capacity_score_mun +
                log_pop_mun + log_gdp_pc_mun + log_area_mun +
                urban_share_mun + log_dist_capital +
                factor(bioma) +
                adm_capacity_score_est + fiscal_autonomy_est,
              data = diag_data)

# Residuals vs fitted
resid_df <- data.frame(
  fitted    = fitted(lm_diag),
  residuals = residuals(lm_diag),
  std_resid = rstandard(lm_diag),
  cooks_d   = cooks.distance(lm_diag)
) %>%
  mutate(obs = row_number(),
         influential = cooks_d > 4 / nrow(diag_data))

p_resid_fit <- ggplot(resid_df,
                      aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.2, size = 0.6,
             color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40") +
  geom_smooth(method = "loess", se = FALSE,
              color = "darkred", linewidth = 0.7) +
  labs(
    title    = "Residuals vs fitted values",
    subtitle = "Main model: composite + controls + biome FE",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# Scale-location
p_scale_loc <- ggplot(resid_df,
                      aes(x = fitted,
                          y = sqrt(abs(std_resid)))) +
  geom_point(alpha = 0.2, size = 0.6,
             color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE,
              color = "darkred", linewidth = 0.7) +
  labs(
    title    = "Scale-location plot",
    subtitle = "Homoskedasticity check",
    x = "Fitted values",
    y = "√|Standardized residuals|"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p_resid_compare <- gridExtra::grid.arrange(
  p_resid_fit, p_scale_loc, ncol = 2)
ggsave(paste0(PATH_APPENDIX, "appendix_a3_residual_diagnostics.pdf"),
       p_resid_compare, width = 10, height = 4)
cat("Saved appendix_a3_residual_diagnostics.pdf\n")

# --- A4: Influential observations (Cook's distance) ---
p_cooks <- ggplot(resid_df, aes(x = obs, y = cooks_d)) +
  geom_point(aes(color = influential),
             size = 0.7, alpha = 0.5) +
  geom_hline(yintercept = 4 / nrow(diag_data),
             linetype = "dashed", color = "darkred",
             linewidth = 0.6) +
  scale_color_manual(
    values = c("FALSE" = "steelblue",
               "TRUE"  = "darkred"),
    labels = c("FALSE" = "Normal",
               "TRUE"  = "Influential"),
    name = NULL
  ) +
  annotate("text",
           x = nrow(resid_df) * 0.02,
           y = (4 / nrow(diag_data)) * 1.15,
           label = paste0("Threshold: 4/n = ",
                          round(4 / nrow(diag_data), 4)),
           hjust = 0, size = 3, color = "darkred") +
  labs(
    title    = "Cook's distance — influential observations",
    subtitle = paste0(sum(resid_df$influential),
                      " observations above threshold (4/n)"),
    x = "Observation index",
    y = "Cook's distance"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(paste0(PATH_APPENDIX, "appendix_a4_cooks_distance.pdf"),
       p_cooks, width = 8, height = 4)
cat("Saved appendix_a4_cooks_distance.pdf\n")

cat("\nAppendix diagnostics saved to:", PATH_APPENDIX, "\n")
cat("\nScript 32 complete.\n")