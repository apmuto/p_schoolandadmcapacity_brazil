# ==================================================
# 98_explore_dist.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Quantile regression — are fiscal capacity
#       effects different at the top (most
#       inaccessible municipalities) vs median?
#       Tests whether O'Donnell brown areas show
#       stronger effects at high quantiles.
# Input:  data/processed/master_mun_main.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(quantreg)

# ============================================================
# LOAD DATA
# ============================================================

cat("Loading master_mun_main...\n")
main <- load_br_csv("data/processed/master_mun_main.csv") %>%
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
    periphery = as.integer(regiao %in%
                             c("Norte", "Nordeste"))
  ) %>%
  filter(!is.na(log_mean_dist_school),
         !is.na(fpm_dependence_mun),
         !is.na(fiscal_autonomy_mun),
         !is.na(adm_capacity_score_mun),
         !is.na(log_pop_mun),
         !is.na(log_gdp_pc_mun),
         !is.na(bioma))

report_dims(main, "analysis sample")

# ============================================================
# QUANTILE REGRESSION
# Run at multiple quantiles to see if effects
# differ across the distribution
# ============================================================

cat("\n============================================================\n")
cat("QUANTILE REGRESSION — log_mean_dist_school\n")
cat("Quantiles: 0.10, 0.25, 0.50, 0.75, 0.90\n")
cat("============================================================\n")

quantiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# Run models at each quantile
qr_models <- lapply(quantiles, function(q) {
  rq(log_mean_dist_school ~
       fpm_dependence_mun +
       fiscal_autonomy_mun +
       adm_capacity_score_mun +
       log_pop_mun +
       log_gdp_pc_mun +
       factor(bioma),
     data = main,
     tau  = q)
})

# Extract key coefficients across quantiles
key_vars <- c("fpm_dependence_mun",
              "fiscal_autonomy_mun",
              "adm_capacity_score_mun",
              "log_pop_mun",
              "log_gdp_pc_mun")

cat("\nCoefficients by quantile:\n")
cat(sprintf("%-30s", "Variable"))
for (q in quantiles) cat(sprintf("  Q%02.0f  ", q*100))
cat("\n")
cat(paste(rep("-", 70), collapse=""), "\n")

for (v in key_vars) {
  cat(sprintf("%-30s", v))
  for (i in seq_along(quantiles)) {
    coef_val <- coef(qr_models[[i]])[v]
    cat(sprintf(" %6.3f", coef_val))
  }
  cat("\n")
}

# ============================================================
# SIGNIFICANCE TESTS AT EACH QUANTILE
# ============================================================

cat("\n--- Significance at each quantile ---\n")
cat("(using summary with bootstrap SE)\n\n")

for (i in seq_along(quantiles)) {
  q <- quantiles[i]
  cat(sprintf("Q%02.0f (tau=%.2f):\n", q*100, q))
  s <- summary(qr_models[[i]], se = "boot", R = 200)
  coefs <- s$coefficients
  for (v in key_vars) {
    if (v %in% rownames(coefs)) {
      sig <- case_when(
        coefs[v, "Pr(>|t|)"] < 0.001 ~ "***",
        coefs[v, "Pr(>|t|)"] < 0.01  ~ "**",
        coefs[v, "Pr(>|t|)"] < 0.05  ~ "*",
        coefs[v, "Pr(>|t|)"] < 0.10  ~ ".",
        TRUE ~ ""
      )
      cat(sprintf("  %-30s coef=%7.3f  p=%.3f %s\n",
                  v,
                  coefs[v, "Value"],
                  coefs[v, "Pr(>|t|)"],
                  sig))
    }
  }
  cat("\n")
}

# ============================================================
# COMPARE OLS vs QUANTILE AT KEY QUANTILES
# ============================================================

cat("\n============================================================\n")
cat("OLS vs QUANTILE REGRESSION COMPARISON\n")
cat("============================================================\n")

m_ols <- lm(log_mean_dist_school ~
              fpm_dependence_mun +
              fiscal_autonomy_mun +
              adm_capacity_score_mun +
              log_pop_mun +
              log_gdp_pc_mun +
              factor(bioma),
            data = main)

cat("\nOLS coefficients:\n")
ols_coefs <- coef(m_ols)
for (v in key_vars) {
  cat(sprintf("  %-30s coef=%7.3f\n", v, ols_coefs[v]))
}
cat(sprintf("  R²=%.3f\n", summary(m_ols)$r.squared))

# ============================================================
# QUANTILE PLOT — coefficients across quantiles
# ============================================================

cat("\nBuilding quantile coefficient plot...\n")

# Run finer grid of quantiles for plot
quantiles_fine <- seq(0.10, 0.90, by = 0.05)

qr_fine <- lapply(quantiles_fine, function(q) {
  m <- rq(log_mean_dist_school ~
            fpm_dependence_mun +
            fiscal_autonomy_mun +
            adm_capacity_score_mun +
            log_pop_mun +
            log_gdp_pc_mun +
            factor(bioma),
          data = main, tau = q)
  s <- summary(m, se = "boot", R = 100)
  tibble(
    tau     = q,
    variable = key_vars,
    coef    = coef(m)[key_vars],
    se      = s$coefficients[key_vars, "Std. Error"],
    lo      = coef(m)[key_vars] -
              1.96 * s$coefficients[key_vars, "Std. Error"],
    hi      = coef(m)[key_vars] +
              1.96 * s$coefficients[key_vars, "Std. Error"]
  )
})

qr_plot_data <- bind_rows(qr_fine) %>%
  mutate(variable = recode(variable,
    "fpm_dependence_mun"     = "FPM dependence",
    "fiscal_autonomy_mun"    = "Fiscal autonomy",
    "adm_capacity_score_mun" = "Adm. capacity score",
    "log_pop_mun"            = "Log population",
    "log_gdp_pc_mun"         = "Log GDP pc"
  ))

# OLS reference lines
ols_ref <- tibble(
  variable = c("FPM dependence", "Fiscal autonomy",
               "Adm. capacity score",
               "Log population", "Log GDP pc"),
  ols_coef = ols_coefs[key_vars]
)

p_qr <- ggplot(qr_plot_data,
               aes(x = tau, y = coef)) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              fill = "#2166ac", alpha = 0.2) +
  geom_line(color = "#2166ac", linewidth = 0.8) +
  geom_hline(data = ols_ref,
             aes(yintercept = ols_coef),
             color = "red", linetype = "dashed",
             linewidth = 0.6) +
  geom_hline(yintercept = 0,
             color = "black", linewidth = 0.3) +
  facet_wrap(~variable, scales = "free_y", nrow = 2) +
  scale_x_continuous(breaks = c(0.1,0.25,0.5,0.75,0.9),
                     labels = c("Q10","Q25","Q50",
                                "Q75","Q90")) +
  labs(
    title = "Quantile regression coefficients",
    subtitle = paste0("Blue = quantile regression (95% CI) | ",
                      "Red dashed = OLS\n",
                      "Outcome: log mean distance to school"),
    x = "Quantile",
    y = "Coefficient",
    caption = "Source: Censo Escolar 2025, FINBRA 2024, GHSL-SMOD 2020"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"))

ggsave("outputs/figures/98_quantile_regression.png",
       p_qr, width = 10, height = 7, dpi = 150)
cat("Saved quantile plot\n")

# ============================================================
# QUANTILE REGRESSION BY REGION
# Does the quantile pattern differ in periphery?
# ============================================================

cat("\n============================================================\n")
cat("QUANTILE REGRESSION BY PERIPHERY\n")
cat("============================================================\n")

for (label in c("Core", "Periphery")) {
  is_p <- label == "Periphery"
  df <- main %>% filter(periphery == as.integer(is_p))
  cat(sprintf("\n--- %s (n=%d) ---\n", label, nrow(df)))

  cat(sprintf("%-30s", "Variable"))
  for (q in c(0.25, 0.50, 0.75, 0.90)) {
    cat(sprintf("  Q%02.0f ", q*100))
  }
  cat("\n")

  for (v in c("fpm_dependence_mun",
              "fiscal_autonomy_mun")) {
    cat(sprintf("%-30s", v))
    for (q in c(0.25, 0.50, 0.75, 0.90)) {
      m <- rq(log_mean_dist_school ~
                fpm_dependence_mun +
                fiscal_autonomy_mun +
                log_pop_mun + log_gdp_pc_mun,
              data = df, tau = q)
      cat(sprintf(" %6.3f", coef(m)[v]))
    }
    cat("\n")
  }
}

cat("\nScript 98 complete.\n")