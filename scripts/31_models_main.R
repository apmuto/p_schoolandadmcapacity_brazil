# ==============================================================================
# 31_models_main.R
# Project: Municipal administrative capacity and secondary school
#          accessibility in Brazil
#
# Goal: Estimate main OLS models with clustered SE by state.
#       DV: log_mean_dist (log mean settlement-to-school distance)
#
# Model structure:
#   m1_main: Fiscal autonomy, no FE       -- Tilly baseline
#   m2_main: Behavioral composite, no FE  -- main result
#   m3_main: Behavioral composite, state FE -- main result with FE
#   m4_main: Disaggregated components, no FE -- opens black box
#
# Output:
#   tab_main.tex              -- all four models side by side (coefs + fit stats)
#   model_fit_comparison.csv  -- model fit stats (diagnostic use)
#   fig_coef_main.pdf         -- IV distribution boxplot
#   fig_smooth_models.pdf     -- composite smooth plot (m2 vs m3)
#   fig_smooth_m1.pdf         -- fiscal autonomy smooth plot (m1)
#
# SE: clustered by state (SG_UF) throughout
#
# Inputs:  data/processed/master_mun_main.csv
# Author:  Ana Paula Muto
# ==============================================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(fixest)
library(lmtest)
library(sandwich)
library(modelsummary)
library(kableExtra)

# ==============================================================================
# CONSTANTS
# ==============================================================================

PATH_MAIN   <- "data/processed/master_mun_main.csv"
PATH_TABLES <- "outputs/tables/"
PATH_FIGS   <- "outputs/figures/"

dir.create(PATH_TABLES, recursive = TRUE, showWarnings = FALSE)
dir.create(PATH_FIGS,   recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading master_mun_main...\n")
df_main <- load_br_csv(PATH_MAIN) %>%
  mutate(
    CO_MUNICIPIO = as.character(CO_MUNICIPIO),
    bioma        = relevel(factor(bioma), ref = "Cerrado")
  ) %>%
  filter(has_school == TRUE)
report_dims(df_main, "restricted sample (has_school)")

cat("\nDV availability:\n")
cat("  Valid log_mean_dist:",
    sum(!is.na(df_main$log_mean_dist)), "\n")

# Region order for plots
REGION_ORDER <- c("Norte", "Nordeste", "Centro-Oeste", "Sul", "Sudeste")

# ==============================================================================
# MAIN MODELS
# ==============================================================================

# m1: Fiscal autonomy baseline (Tilly) — no state FE
m1_main <- lm(
  log_mean_dist ~
    fiscal_autonomy_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + log_dist_capital + bioma +
    fiscal_autonomy_est,
  data = df_main
)

m1_clustered <- coeftest(m1_main,
                         vcov = vcovCL(m1_main, cluster = ~SG_UF))

cat("m1 — fiscal autonomy (clustered SE):\n")
print(m1_clustered["fiscal_autonomy_mun", , drop = FALSE])
cat("N:", nobs(m1_main), "| R2:", round(summary(m1_main)$r.squared, 3), "\n")

# ------------------------------------------------------------------------------

# m2: Behavioral composite — no state FE
m2_main <- lm(
  log_mean_dist ~
    adm_capacity_score_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + log_dist_capital + bioma +
    fiscal_autonomy_est,
  data = df_main
)

m2_clustered <- coeftest(m2_main,
                         vcov = vcovCL(m2_main, cluster = ~SG_UF))

cat("\nm2 — behavioral composite (clustered SE):\n")
print(m2_clustered["adm_capacity_score_mun", , drop = FALSE])
cat("N:", nobs(m2_main), "| R2:", round(summary(m2_main)$r.squared, 3), "\n")

# ------------------------------------------------------------------------------

# m3: Behavioral composite — state FE (feols)
# State-level controls dropped — collinear with state FE
m3_main <- feols(
  log_mean_dist ~
    adm_capacity_score_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + log_dist_capital + bioma
  | SG_UF,
  data    = df_main,
  cluster = ~SG_UF
)

cat("\nm3 — behavioral composite, state FE (feols, clustered SE):\n")
print(coeftable(m3_main)["adm_capacity_score_mun", , drop = FALSE])
cat("N:", m3_main$nobs, "| R2:", round(r2(m3_main)["r2"], 3), "\n")

# ------------------------------------------------------------------------------

# m4: Disaggregated components — no state FE
m4_main <- lm(
  log_mean_dist ~
    edu_share_mun + budget_execution_mun +
    debt_ratio_mun + fpm_dependence_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + log_dist_capital + bioma +
    fiscal_autonomy_est,
  data = df_main
)

m4_clustered <- coeftest(m4_main,
                         vcov = vcovCL(m4_main, cluster = ~SG_UF))
#m5: Interaction
m_5 <- lm(
  log_mean_dist ~
    fpm_dependence_mun * log_dist_capital +
    adm_capacity_score_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + bioma + fiscal_autonomy_est,
  data = df_main
)

cat("\nm4 — disaggregated components (clustered SE):\n")
print(m4_clustered[c("edu_share_mun", "budget_execution_mun",
                     "debt_ratio_mun", "fpm_dependence_mun"), ])
cat("N:", nobs(m4_main), "| R2:", round(summary(m4_main)$r.squared, 3), "\n")
#m5: Interaction
m5_main <- lm(
  log_mean_dist ~
    fpm_dependence_mun * log_dist_capital +
    adm_capacity_score_mun +
    log_pop_mun + log_gdp_pc_mun + log_area_mun +
    urban_share_mun + bioma + fiscal_autonomy_est,
  data = df_main
)
m5_clustered <- coeftest(m5_main,
                         vcov = vcovCL(m5_main, cluster = ~SG_UF))

cat("\nm5 — FPM x distance interaction (clustered SE):\n")
print(m5_clustered[c("fpm_dependence_mun",
                     "log_dist_capital",
                     "fpm_dependence_mun:log_dist_capital"), , drop = FALSE])
cat("N:", nobs(m5_main), "| R2:", round(summary(m5_main)$r.squared, 3), "\n")
# ==============================================================================
# TABLE EXPORT — single table with coefficients + fit stats
# Uses kableExtra backend to avoid tabularray/talltblr
# AIC note: not directly comparable between m3 (feols) and lm models
# ==============================================================================

cat("\nExporting LaTeX table...\n")

options(modelsummary_factory_latex = "kableExtra")
options("modelsummary_format_numeric_latex" = "plain")

coef_map <- c(
  "fiscal_autonomy_mun"    = "Fiscal autonomy",
  "adm_capacity_score_mun" = "Administrative capacity",
  "edu_share_mun"          = "Education spending share",
  "budget_execution_mun"   = "Budget execution rate",
  "debt_ratio_mun"         = "Debt ratio",
  "fpm_dependence_mun"     = "FPM dependence"
)

modelsummary(
  list(
    "Model 1"   = m1_main,
    "Model 2" = m2_main,
    "Model 3"    = m3_main,
    "Model 4"     = m4_main
  ),
  coef_map  = coef_map,
  vcov      = list(~SG_UF, ~SG_UF, ~SG_UF, ~SG_UF),
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map   = list(
    list(raw = "nobs",          clean = "N",       fmt = 0),
    list(raw = "r.squared",     clean = "R2",      fmt = 3),
    list(raw = "adj.r.squared", clean = "R2 Adj.", fmt = 3)
  ),
  escape = FALSE,
  notes     = c(
    "*** p < 0.01, ** p < 0.05, * p < 0.1.",
    "Controls: log population, log GDP pc, log area, urban share, log distance to capital, biome FE.",
    "State-level fiscal autonomy included in M1--M4 without state FE.",
    "Clustered SE by state in parentheses.",
    "AIC not directly comparable between M3 (feols) and M1/M2/M4 (lm)."
  ),
  output    = paste0(PATH_TABLES, "tab_main.tex"),
  booktabs  = TRUE,
  title     = "Administrative Capacity and Secondary School Accessibility"
)

cat("Saved tab_main.tex\n")

# ==============================================================================
# MODEL FIT COMPARISON (console + csv — diagnostic use)
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("MODEL FIT COMPARISON\n")
cat(strrep("=", 60), "\n")

fit_table <- data.frame(
  Model = c("m1: Fiscal autonomy (no FE)",
            "m2: Composite (no FE)",
            "m3: Composite (state FE)",
            "m4: Disaggregated (no FE)"),
  N = c(nobs(m1_main),
        nobs(m2_main),
        m3_main$nobs,
        nobs(m4_main)),
  R2 = c(summary(m1_main)$r.squared,
         summary(m2_main)$r.squared,
         r2(m3_main)["r2"],
         summary(m4_main)$r.squared),
  R2_adj = c(summary(m1_main)$adj.r.squared,
             summary(m2_main)$adj.r.squared,
             r2(m3_main)["ar2"],
             summary(m4_main)$adj.r.squared),
  AIC = c(AIC(m1_main),
          AIC(m2_main),
          AIC(m3_main),
          AIC(m4_main))
)

fit_table$R2     <- round(fit_table$R2, 3)
fit_table$R2_adj <- round(fit_table$R2_adj, 3)
fit_table$AIC    <- round(fit_table$AIC, 1)

print(fit_table)
fwrite(fit_table, paste0(PATH_TABLES, "model_fit_comparison.csv"))
cat("Saved model_fit_comparison.csv\n")

# ==============================================================================
# DIAGNOSTICS — IV correlation matrix
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("IV CORRELATION MATRIX\n")
cat(strrep("=", 60), "\n")

iv_cors <- df_main %>%
  select(fiscal_autonomy_mun, adm_capacity_score_mun,
         edu_share_mun, budget_execution_mun,
         debt_ratio_mun, fpm_dependence_mun,
         log_pop_mun) %>%
  cor(use = "complete.obs")
print(round(iv_cors, 3))

# ==============================================================================
# FIGURE 1: FACETED BOXPLOT — key IV distributions
# ==============================================================================

cat("\nBuilding Figure 1: faceted IV boxplot...\n")

var_labels <- c(
  "fiscal_autonomy_mun"      = "Fiscal autonomy\n(m1 baseline)",
  "adm_capacity_score_mun"   = "Capacity score\n(composite)",
  "edu_share_mun"            = "Education\nspending share",
  "budget_execution_mun"     = "Budget\nexecution rate",
  "debt_ratio_mun"           = "Debt ratio",
  "fpm_dependence_mun"       = "FPM\ndependence"
)

box_data <- df_main %>%
  select(names(var_labels)) %>%
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(variable = factor(variable,
                           levels = names(var_labels),
                           labels = var_labels))

p_box <- ggplot(box_data, aes(x = "", y = value)) +
  geom_boxplot(fill = "steelblue", color = "grey30",
               alpha = 0.6, outlier.size = 0.4,
               outlier.alpha = 0.25, linewidth = 0.4,
               width = 0.5) +
  facet_wrap(~ variable, scales = "free_y", nrow = 2) +
  labs(
    title   = "Distribution of key predictors",
    x       = NULL,
    y       = NULL,
    caption = "Source: FINBRA 2024; Censo Escolar 2025."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.caption       = element_text(color = "grey55", size = 7),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_blank(),
    strip.text         = element_text(size = 9, face = "bold",
                                      lineheight = 1.2)
  )

ggsave(paste0(PATH_FIGS, "fig_coef_main.pdf"),
       p_box, width = 8, height = 6)
cat("Saved fig_coef_main.pdf\n")

# ==============================================================================
# FIGURE 2: SMOOTH LINE — m2 and m3 composite models overlaid
# ==============================================================================

cat("\nBuilding Figure 2: composite smooth plot (m2 vs m3)...\n")

smooth_data <- df_main %>%
  filter(!is.na(adm_capacity_score_mun),
         !is.na(log_mean_dist)) %>%
  arrange(adm_capacity_score_mun)

lm_m2 <- lm(log_mean_dist ~ adm_capacity_score_mun +
              log_pop_mun + log_gdp_pc_mun + log_area_mun +
              urban_share_mun + log_dist_capital + bioma +
              fiscal_autonomy_est,
            data = smooth_data)

lm_m3 <- lm(log_mean_dist ~ adm_capacity_score_mun +
              log_pop_mun + log_gdp_pc_mun + log_area_mun +
              urban_share_mun + log_dist_capital +
              factor(SG_UF),
            data = smooth_data)

x_grid_comp <- data.frame(
  adm_capacity_score_mun = seq(
    quantile(smooth_data$adm_capacity_score_mun, 0.02),
    quantile(smooth_data$adm_capacity_score_mun, 0.98),
    length.out = 200
  )
) %>%
  mutate(
    log_pop_mun         = mean(smooth_data$log_pop_mun,      na.rm = TRUE),
    log_gdp_pc_mun      = mean(smooth_data$log_gdp_pc_mun,   na.rm = TRUE),
    log_area_mun        = mean(smooth_data$log_area_mun,     na.rm = TRUE),
    urban_share_mun     = mean(smooth_data$urban_share_mun,  na.rm = TRUE),
    log_dist_capital    = mean(smooth_data$log_dist_capital, na.rm = TRUE),
    fiscal_autonomy_est = mean(smooth_data$fiscal_autonomy_est, na.rm = TRUE),
    bioma               = "Cerrado",
    SG_UF               = "SP"
  )

pred_m2 <- predict(lm_m2, newdata = x_grid_comp,
                   interval = "confidence") %>%
  as.data.frame() %>%
  bind_cols(x_grid_comp %>% select(adm_capacity_score_mun)) %>%
  mutate(model = "m2: Composite (no FE)")

pred_m3 <- predict(lm_m3, newdata = x_grid_comp,
                   interval = "confidence") %>%
  as.data.frame() %>%
  bind_cols(x_grid_comp %>% select(adm_capacity_score_mun)) %>%
  mutate(model = "m3: Composite (state FE)")

pred_comp <- bind_rows(pred_m2, pred_m3) %>%
  mutate(model = factor(model,
                        levels = c("m2: Composite (no FE)",
                                   "m3: Composite (state FE)")))

p_smooth <- ggplot(pred_comp,
                   aes(x = adm_capacity_score_mun,
                       y = fit, color = model, fill = model)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = c("m2: Composite (no FE)"    = "steelblue",
               "m3: Composite (state FE)" = "#1a5276"),
    name = NULL
  ) +
  scale_fill_manual(
    values = c("m2: Composite (no FE)"    = "steelblue",
               "m3: Composite (state FE)" = "#1a5276"),
    name = NULL
  ) +
  labs(
    title   = "Administrative capacity and school accessibility",
    x       = "Administrative capacity score",
    y       = "Predicted log mean distance (km)",
    caption = "Controls held at sample means. M3 evaluated at SP reference state."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.caption     = element_text(color = "grey55", size = 7),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

ggsave(paste0(PATH_FIGS, "fig_smooth_models.pdf"),
       p_smooth, width = 7, height = 5)
cat("Saved fig_smooth_models.pdf\n")

# ==============================================================================
# FIGURE 3: SMOOTH LINE — m1 fiscal autonomy baseline
# ==============================================================================

cat("\nBuilding Figure 3: fiscal autonomy smooth plot (m1)...\n")

smooth_data_m1 <- df_main %>%
  filter(!is.na(fiscal_autonomy_mun),
         !is.na(log_mean_dist)) %>%
  arrange(fiscal_autonomy_mun)

lm_m1 <- lm(log_mean_dist ~ fiscal_autonomy_mun +
              log_pop_mun + log_gdp_pc_mun + log_area_mun +
              urban_share_mun + log_dist_capital + bioma +
              fiscal_autonomy_est,
            data = smooth_data_m1)

x_grid_m1 <- data.frame(
  fiscal_autonomy_mun = seq(
    quantile(smooth_data_m1$fiscal_autonomy_mun, 0.02),
    quantile(smooth_data_m1$fiscal_autonomy_mun, 0.98),
    length.out = 200
  )
) %>%
  mutate(
    log_pop_mun         = mean(smooth_data_m1$log_pop_mun,      na.rm = TRUE),
    log_gdp_pc_mun      = mean(smooth_data_m1$log_gdp_pc_mun,   na.rm = TRUE),
    log_area_mun        = mean(smooth_data_m1$log_area_mun,     na.rm = TRUE),
    urban_share_mun     = mean(smooth_data_m1$urban_share_mun,  na.rm = TRUE),
    log_dist_capital    = mean(smooth_data_m1$log_dist_capital, na.rm = TRUE),
    fiscal_autonomy_est = mean(smooth_data_m1$fiscal_autonomy_est, na.rm = TRUE),
    bioma               = "Cerrado"
  )

pred_m1 <- predict(lm_m1, newdata = x_grid_m1,
                   interval = "confidence") %>%
  as.data.frame() %>%
  bind_cols(x_grid_m1 %>% select(fiscal_autonomy_mun))

p_smooth_m1 <- ggplot(pred_m1,
                      aes(x = fiscal_autonomy_mun, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              fill = "steelblue", alpha = 0.2, color = NA) +
  geom_line(color = "steelblue", linewidth = 0.9) +
  labs(
    title   = "Fiscal autonomy and school accessibility",
    x       = "Fiscal autonomy (own revenue / total revenue)",
    y       = "Predicted log mean distance (km)",
    caption = "Controls held at sample means. Biome = Cerrado reference."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.caption     = element_text(color = "grey55", size = 7),
    panel.grid.minor = element_blank()
  )

ggsave(paste0(PATH_FIGS, "fig_smooth_m1.pdf"),
       p_smooth_m1, width = 7, height = 5)
cat("Saved fig_smooth_m1.pdf\n")

cat("\nScript 31 complete.\n")


