# ==================================================
# 31_descriptivestat.R
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
  list(var="mean_dist_school_km",
       label="Mean distance to school (km)"),
  list(var="log_mean_dist_school",
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
    n                    = n(),
    mean_pop             = round(mean(pop_mun,
                                      na.rm=TRUE)),
    median_pop           = round(median(pop_mun,
                                         na.rm=TRUE)),
    mean_gdp_pc          = round(mean(exp(log_gdp_pc_mun),
                                       na.rm=TRUE)),
    mean_fiscal_auto     = round(mean(fiscal_autonomy_mun,
                                       na.rm=TRUE), 3),
    mean_fpm_dep         = round(mean(fpm_dependence_mun,
                                       na.rm=TRUE), 3),
    mean_adm_cap         = round(mean(adm_capacity_score_mun,
                                       na.rm=TRUE), 3),
    mean_dist_capital    = round(mean(dist_capital_km,
                                       na.rm=TRUE), 1),
    mean_urban_share     = round(mean(urban_share_mun,
                                       na.rm=TRUE), 3),
    pct_periphery        = round(100 * mean(periphery,
                                             na.rm=TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(has_school = ifelse(has_school,
                              "Has school", "No school"))

print(table2)
fwrite(table2,
       paste0(PATH_TABLES, "table2_profile_school_presence.csv"))
cat("Saved Table 2\n")

# ============================================================
# TABLE 3: ACCESSIBILITY BY REGION AND BIOME
# ============================================================

cat("\nBuilding Table 3: Accessibility by region...\n")

table3_region <- main %>%
  group_by(regiao) %>%
  summarise(
    n_mun              = n(),
    pct_no_school      = round(100 * mean(!has_school), 1),
    mean_dist_km       = round(mean(mean_dist_school_km,
                                     na.rm=TRUE), 2),
    median_dist_km     = round(median(mean_dist_school_km,
                                       na.rm=TRUE), 2),
    pct_over_10km      = round(100 * mean(pct_over_10km,
                                           na.rm=TRUE), 1),
    mean_schools_10k   = round(mean(schools_per_10k,
                                     na.rm=TRUE), 2),
    mean_fpm_dep       = round(mean(fpm_dependence_mun,
                                     na.rm=TRUE), 3),
    mean_fiscal_auto   = round(mean(fiscal_autonomy_mun,
                                     na.rm=TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_dist_km))

cat("\nBy region:\n")
print(table3_region)
fwrite(table3_region,
       paste0(PATH_TABLES, "table3a_access_by_region.csv"))

table3_biome <- main %>%
  filter(!is.na(bioma), bioma != "") %>%
  group_by(bioma) %>%
  summarise(
    n_mun            = n(),
    pct_no_school    = round(100 * mean(!has_school), 1),
    mean_dist_km     = round(mean(mean_dist_school_km,
                                   na.rm=TRUE), 2),
    median_dist_km   = round(median(mean_dist_school_km,
                                     na.rm=TRUE), 2),
    pct_over_10km    = round(100 * mean(pct_over_10km,
                                         na.rm=TRUE), 1),
    mean_schools_10k = round(mean(schools_per_10k,
                                   na.rm=TRUE), 2),
    .groups = "drop"
  ) %>%
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
    mean_dist_km     = round(mean(mean_dist_school_km,
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
  filter(!is.na(mean_dist_school_km)) %>%
  mutate(dist_cap = pmin(mean_dist_school_km, 30)) %>%
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
                      sum(!is.na(main$mean_dist_school_km)),
                      ") | Capped at 30km"),
    x = "Mean distance to nearest school (km)",
    y = "Number of municipalities",
    caption = "Source: Censo Escolar 2025, GHSL-SMOD 2020"
  ) +
  theme_minimal(base_size = 11)

ggsave(paste0(PATH_FIGS, "31_01_dist_distribution.png"),
       p1, width = 8, height = 5, dpi = 150)
cat("Saved Figure 1\n")

# ============================================================
# FIGURE 2: ACCESSIBILITY BY REGION — BOX PLOT
# ============================================================

cat("Building Figure 2: Distance by region...\n")

p2 <- main %>%
  filter(!is.na(mean_dist_school_km),
         !is.na(regiao),
         mean_dist_school_km <= 30) %>%
  ggplot(aes(x = reorder(regiao,
                          mean_dist_school_km,
                          median),
             y = mean_dist_school_km,
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

ggsave(paste0(PATH_FIGS, "31_02_dist_by_region.png"),
       p2, width = 8, height = 5, dpi = 150)
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

ggsave(paste0(PATH_FIGS, "31_03_fiscal_distributions.png"),
       p3, width = 9, height = 4, dpi = 150)
cat("Saved Figure 3\n")

# ============================================================
# FIGURE 4: FPM DEPENDENCE vs DISTANCE — SCATTER
# ============================================================

cat("Building Figure 4: FPM vs distance scatter...\n")

p4 <- main %>%
  filter(!is.na(fpm_dependence_mun),
         !is.na(mean_dist_school_km),
         mean_dist_school_km <= 20) %>%
  ggplot(aes(x = fpm_dependence_mun,
             y = mean_dist_school_km,
             color = bioma)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "lm", color = "black",
              se = TRUE, linewidth = 0.8) +
  scale_color_brewer(palette = "Set2",
                     name = "Biome") +
  labs(
    title = "FPM dependence and school accessibility",
    subtitle = "Higher FPM dependence → shorter distances | Capped at 20km",
    x = "FPM dependence (FPM / total revenue)",
    y = "Mean settlement-to-school distance (km)",
    caption = "Source: Censo Escolar 2025, FINBRA 2024, GHSL-SMOD 2020"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(paste0(PATH_FIGS, "31_04_fpm_vs_distance.png"),
       p4, width = 8, height = 6, dpi = 150)
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
    mean_dist  = mean(mean_dist_school_km, na.rm=TRUE),
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

ggsave(paste0(PATH_FIGS, "31_05_population_threshold.png"),
       p5, width = 8, height = 5, dpi = 150)
cat("Saved Figure 5\n")

# ============================================================
# FIGURE 6: BIOME COMPARISON
# ============================================================

cat("Building Figure 6: Distance by biome...\n")

p6 <- main %>%
  filter(!is.na(bioma), bioma != "",
         !is.na(mean_dist_school_km),
         mean_dist_school_km <= 20) %>%
  ggplot(aes(x = reorder(bioma,
                          mean_dist_school_km,
                          median),
             y = mean_dist_school_km,
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

ggsave(paste0(PATH_FIGS, "31_06_dist_by_biome.png"),
       p6, width = 8, height = 5, dpi = 150)
cat("Saved Figure 6\n")

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
    sum(!is.na(main$mean_dist_school_km)), "\n")
cat("Mean distance to school:",
    round(mean(main$mean_dist_school_km, na.rm=TRUE), 2),
    "km\n")
cat("Municipalities >10km mean distance:",
    sum(main$mean_dist_school_km > 10, na.rm=TRUE), "\n")
cat("Municipalities >30km mean distance:",
    sum(main$mean_dist_school_km > 30, na.rm=TRUE), "\n")

cat("\nTables saved to:", PATH_TABLES, "\n")
cat("Figures saved to:", PATH_FIGS, "\n")
cat("\nScript 31 complete.\n")