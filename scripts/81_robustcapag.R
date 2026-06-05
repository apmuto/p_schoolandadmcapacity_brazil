# ==================================================
# 81_robustcapag.R
# Project: Municipal administrative capacity and
#          secondary school accessibility in Brazil
# Goal: Part 0: Composite construction validation
#         (PCA loadings, inter-indicator correlations,
#          pop orthogonalization check, CAPAG bridge)
#       Part 1: CAPAG vs FINBRA composite — same sample
#       Part 2: CAPAG full specifications
#       Part 3: CAPAG by region
#       Part 4: Municipal vs state CAPAG
#       Part 5: AIC comparison
# Note: CAPAG missing for ~34% of municipalities
#       (n.d./n.e.) — smaller sample, interpret
#       with caution.
# Input:  data/processed/master_mun_main.csv
# Author: Ana Paula Muto
# Date: 2025
# ==================================================

source("scripts/00_functions_misc.R")
library(tidyverse)
library(data.table)
library(lmtest)
library(sandwich)

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
  )

report_dims(main, "master_mun_main")

cat("\nCAPAG sample sizes:\n")
cat("With CAPAG:", sum(!is.na(main$capag_numeric)), "\n")
cat("Missing CAPAG:", sum(is.na(main$capag_numeric)), "\n")
cat("CAPAG distribution:\n")
print(table(main$capag_numeric, useNA = "always"))

# ============================================================
# HELPER: robust coeftest
# ============================================================

robust <- function(model, cluster) {
  coeftest(model,
           vcov = vcovCL(model,
                         cluster = cluster))
}

print_key <- function(robust_model,
                      vars, r2, n, label = "") {
  cat(sprintf("\n%s (R²=%.3f, n=%d):\n", label, r2, n))
  for (v in vars) {
    if (v %in% rownames(robust_model)) {
      cat(sprintf("  %-30s coef=%7.3f p=%.4f\n",
                  v,
                  robust_model[v, "Estimate"],
                  robust_model[v, "Pr(>|t|)"]))
    }
  }
}

key_vars <- c("capag_numeric",
              "fpm_dependence_mun",
              "fiscal_autonomy_mun",
              "log_pop_mun",
              "log_gdp_pc_mun",
              "dist_capital_km")

# ============================================================
# PART 1: CAPAG vs FINBRA COMPOSITE
# Side-by-side comparison on same sample
# ============================================================

cat("\n============================================================\n")
cat("PART 1: CAPAG vs FINBRA COMPOSITE — SAME SAMPLE\n")
cat("============================================================\n")

# Restrict to municipalities with CAPAG data
main_capag <- main %>%
  filter(!is.na(capag_numeric),
         !is.na(log_mean_dist),
         !is.na(fpm_dependence_mun),
         !is.na(fiscal_autonomy_mun),
         !is.na(log_pop_mun),
         !is.na(log_gdp_pc_mun),
         !is.na(bioma))

cat("CAPAG sample:", nrow(main_capag), "municipalities\n")
cat("CAPAG distribution in sample:\n")
print(table(main_capag$capag_numeric))

# Model with FINBRA composite (CAPAG sample)
m_finbra <- lm(log_mean_dist ~
                 adm_capacity_score_mun +
                 fpm_dependence_mun +
                 fiscal_autonomy_mun +
                 log_pop_mun + log_gdp_pc_mun +
                 factor(bioma),
               data = main_capag)

r_finbra <- robust(m_finbra, ~SG_UF)
print_key(r_finbra,
          c("adm_capacity_score_mun",
            "fpm_dependence_mun",
            "fiscal_autonomy_mun",
            "log_pop_mun", "log_gdp_pc_mun"),
          summary(m_finbra)$r.squared,
          nobs(m_finbra),
          "FINBRA composite (CAPAG sample)")

# Model with CAPAG instead
m_capag <- lm(log_mean_dist ~
                capag_numeric +
                fpm_dependence_mun +
                fiscal_autonomy_mun +
                log_pop_mun + log_gdp_pc_mun +
                factor(bioma),
              data = main_capag)

r_capag <- robust(m_capag, ~SG_UF)
print_key(r_capag,
          c("capag_numeric",
            "fpm_dependence_mun",
            "fiscal_autonomy_mun",
            "log_pop_mun", "log_gdp_pc_mun"),
          summary(m_capag)$r.squared,
          nobs(m_capag),
          "CAPAG grade (A=4 to D=1)")

# --- Save Part 1 as LaTeX table ---
dir.create("results", showWarnings = FALSE)

fmt_coef <- function(r, v) {
  if (!v %in% rownames(r)) return("--")
  est <- r[v, "Estimate"]
  p   <- r[v, "Pr(>|t|)"]
  se  <- r[v, "Std. Error"]
  stars <- ifelse(p < 0.01, "$^{***}$",
                  ifelse(p < 0.05, "$^{**}$",
                         ifelse(p < 0.1,  "$^{*}$", "")))
  sprintf("%.3f%s (%.3f)", est, stars, se)
}

part1_vars <- c("adm_capacity_score_mun", "capag_numeric",
                "fpm_dependence_mun", "fiscal_autonomy_mun",
                "log_pop_mun", "log_gdp_pc_mun")

part1_labels <- c(
  "adm_capacity_score_mun" = "Adm. capacity score",
  "capag_numeric"          = "CAPAG grade (A=4 to D=1)",
  "fpm_dependence_mun"     = "FPM dependence",
  "fiscal_autonomy_mun"    = "Fiscal autonomy",
  "log_pop_mun"            = "Log population",
  "log_gdp_pc_mun"         = "Log GDP per capita"
)

p1_lines <- c(
  "\\begin{table}[h]",
  "\\centering",
  "\\caption{Robustness: FINBRA composite vs CAPAG grade (same sample)}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "& FINBRA composite & CAPAG grade \\\\",
  "\\midrule"
)
for (v in part1_vars) {
  lbl <- part1_labels[v]
  c1  <- fmt_coef(r_finbra, v)
  c2  <- fmt_coef(r_capag,  v)
  p1_lines <- c(p1_lines,
                sprintf("%s & %s & %s \\\\", lbl, c1, c2))
}
p1_lines <- c(p1_lines,
              "\\midrule",
              sprintf("$R^2$ & %.3f & %.3f \\\\",
                      summary(m_finbra)$r.squared,
                      summary(m_capag)$r.squared),
              sprintf("$N$ & %d & %d \\\\",
                      nobs(m_finbra), nobs(m_capag)),
              "\\bottomrule",
              "\\end{tabular}",
              "\\\\[4pt]",
              paste0("\\footnotesize{DV: log mean settlement-to-school distance. ",
                     "OLS, SE clustered by state (in parentheses). ",
                     "Biome FE included. ",
                     "$^{*}$p$<$0.1, $^{**}$p$<$0.05, $^{***}$p$<$0.01.}"),
              "\\end{table}"
)
writeLines(p1_lines, "results/tab_capag_vs_finbra.tex")
cat("Saved results/tab_capag_vs_finbra.tex\n")

# ============================================================
# PART 3: CAPAG BY REGION
# ============================================================

cat("\n============================================================\n")
cat("PART 3: CAPAG BY REGION\n")
cat("============================================================\n")

regions <- c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")

# Collect results for LaTeX table
region_results <- list()

for (reg in regions) {
  df_reg <- main_capag %>% filter(regiao == reg)
  if (nrow(df_reg) < 30) next
  m <- lm(log_mean_dist ~
            capag_numeric + fpm_dependence_mun +
            log_pop_mun + log_gdp_pc_mun,
          data = df_reg)
  r <- robust(m, ~SG_UF)
  cat(sprintf("\n--- %s (n=%d) ---\n", reg, nrow(df_reg)))
  for (v in c("capag_numeric", "fpm_dependence_mun")) {
    if (v %in% rownames(r)) {
      cat(sprintf("  %-25s coef=%7.3f p=%.3f\n",
                  v, r[v,"Estimate"], r[v,"Pr(>|t|)"]))
    }
  }
  cat(sprintf("  R²=%.3f\n", summary(m)$r.squared))
  region_results[[reg]] <- list(r = r, m = m, n = nrow(df_reg))
}

# --- Save Part 3 as LaTeX table ---
reg_lines <- c(
  "\\begin{table}[h]",
  "\\centering",
  "\\caption{Regional heterogeneity: CAPAG and FPM dependence by region}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  paste0("& Norte & Nordeste & Sudeste & Sul & ",
         "Centro-Oeste \\\\"),
  "\\midrule"
)

for (v in c("capag_numeric", "fpm_dependence_mun")) {
  lbl <- ifelse(v == "capag_numeric",
                "CAPAG grade",
                "FPM dependence")
  row_vals <- sapply(regions, function(reg) {
    if (is.null(region_results[[reg]])) return("--")
    fmt_coef(region_results[[reg]]$r, v)
  })
  reg_lines <- c(reg_lines,
                 sprintf("%s & %s \\\\",
                         lbl, paste(row_vals, collapse = " & ")))
}

r2_vals <- sapply(regions, function(reg) {
  if (is.null(region_results[[reg]])) return("--")
  sprintf("%.3f", summary(region_results[[reg]]$m)$r.squared)
})
n_vals <- sapply(regions, function(reg) {
  if (is.null(region_results[[reg]])) return("--")
  as.character(region_results[[reg]]$n)
})

reg_lines <- c(reg_lines,
               "\\midrule",
               sprintf("$R^2$ & %s \\\\", paste(r2_vals, collapse = " & ")),
               sprintf("$N$ & %s \\\\",   paste(n_vals,  collapse = " & ")),
               "\\bottomrule",
               "\\end{tabular}",
               "\\\\[4pt]",
               paste0("\\footnotesize{DV: log mean settlement-to-school distance. ",
                      "OLS, SE clustered by state (in parentheses). ",
                      "$^{*}$p$<$0.1, $^{**}$p$<$0.05, $^{***}$p$<$0.01.}"),
               "\\end{table}"
)
writeLines(reg_lines, "results/tab_capag_by_region.tex")
cat("Saved results/tab_capag_by_region.tex\n")

cat("\nScript 81 complete.\n")

# ============================================================
# PART 1b: M1 FISCAL AUTONOMY — CAPAG VALIDATION
# Same CAPAG subsample as Part 1; fiscal_autonomy_mun as IV
# Purpose: confirm M1 Tilly baseline survives on restricted
#          sample and that CAPAG does not displace it
# ============================================================

cat("\n============================================================\n")
cat("PART 1b: M1 FISCAL AUTONOMY — CAPAG VALIDATION\n")
cat("============================================================\n")

# M1 fiscal autonomy on CAPAG subsample (no CAPAG control)
m_fa_capag_base <- lm(log_mean_dist ~
                        fiscal_autonomy_mun +
                        fpm_dependence_mun +
                        log_pop_mun + log_gdp_pc_mun +
                        factor(bioma),
                      data = main_capag)

r_fa_capag_base <- robust(m_fa_capag_base, ~SG_UF)

# M1 fiscal autonomy + CAPAG together (displacement test)
m_fa_capag_joint <- lm(log_mean_dist ~
                         fiscal_autonomy_mun +
                         capag_numeric +
                         fpm_dependence_mun +
                         log_pop_mun + log_gdp_pc_mun +
                         factor(bioma),
                       data = main_capag)

r_fa_capag_joint <- robust(m_fa_capag_joint, ~SG_UF)

print_key(r_fa_capag_base,
          c("fiscal_autonomy_mun", "fpm_dependence_mun",
            "log_pop_mun", "log_gdp_pc_mun"),
          summary(m_fa_capag_base)$r.squared,
          nobs(m_fa_capag_base),
          "M1 fiscal autonomy (CAPAG subsample, no CAPAG control)")

print_key(r_fa_capag_joint,
          c("fiscal_autonomy_mun", "capag_numeric",
            "fpm_dependence_mun", "log_pop_mun", "log_gdp_pc_mun"),
          summary(m_fa_capag_joint)$r.squared,
          nobs(m_fa_capag_joint),
          "M1 fiscal autonomy + CAPAG jointly")

# --- Key numbers for robustness table ---
vfa <- "fiscal_autonomy_mun"
cat(sprintf("\n[TAB] M1 CAPAG subsample (no CAPAG): beta=%.3f SE=%.3f p=%.4f N=%d\n",
            r_fa_capag_base[vfa,"Estimate"],
            r_fa_capag_base[vfa,"Std. Error"],
            r_fa_capag_base[vfa,"Pr(>|t|)"],
            nobs(m_fa_capag_base)))
cat(sprintf("[TAB] M1 + CAPAG jointly:              beta=%.3f SE=%.3f p=%.4f N=%d\n",
            r_fa_capag_joint[vfa,"Estimate"],
            r_fa_capag_joint[vfa,"Std. Error"],
            r_fa_capag_joint[vfa,"Pr(>|t|)"],
            nobs(m_fa_capag_joint)))
cat(sprintf("[TAB] CAPAG in joint model:            beta=%.3f SE=%.3f p=%.4f\n",
            r_fa_capag_joint["capag_numeric","Estimate"],
            r_fa_capag_joint["capag_numeric","Std. Error"],
            r_fa_capag_joint["capag_numeric","Pr(>|t|)"]))

# --- Save Part 1b as LaTeX table ---
p1b_vars   <- c("fiscal_autonomy_mun", "capag_numeric",
                "fpm_dependence_mun", "log_pop_mun", "log_gdp_pc_mun")
p1b_labels <- c(
  "fiscal_autonomy_mun" = "Fiscal autonomy (M1)",
  "capag_numeric"       = "CAPAG grade (A=4 to D=1)",
  "fpm_dependence_mun"  = "FPM dependence",
  "log_pop_mun"         = "Log population",
  "log_gdp_pc_mun"      = "Log GDP per capita"
)

p1b_lines <- c(
  "\\begin{table}[h]",
  "\\centering",
  "\\caption{Robustness M1: Fiscal autonomy on CAPAG subsample}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "& FA only (CAPAG subsample) & FA + CAPAG jointly \\\\",
  "\\midrule"
)
for (v in p1b_vars) {
  lbl <- p1b_labels[v]
  c1  <- fmt_coef(r_fa_capag_base,  v)
  c2  <- fmt_coef(r_fa_capag_joint, v)
  p1b_lines <- c(p1b_lines,
                 sprintf("%s & %s & %s \\\\", lbl, c1, c2))
}
p1b_lines <- c(p1b_lines,
               "\\midrule",
               sprintf("$R^2$ & %.3f & %.3f \\\\",
                       summary(m_fa_capag_base)$r.squared,
                       summary(m_fa_capag_joint)$r.squared),
               sprintf("$N$ & %d & %d \\\\",
                       nobs(m_fa_capag_base), nobs(m_fa_capag_joint)),
               "\\bottomrule",
               "\\end{tabular}",
               "\\\\[4pt]",
               paste0("\\footnotesize{DV: log mean settlement-to-school distance. ",
                      "CAPAG subsample (n\\,=\\,", nrow(main_capag), "). ",
                      "OLS, SE clustered by state (in parentheses). ",
                      "Biome FE included. ",
                      "$^{*}$p$<$0.1, $^{**}$p$<$0.05, $^{***}$p$<$0.01.}"),
               "\\end{table}"
)
writeLines(p1b_lines, "results/tab_capag_fa_robustness.tex")
cat("Saved results/tab_capag_fa_robustness.tex\n")

# ============================================================
# PART 0: COMPOSITE CONSTRUCTION VALIDATION
# ============================================================

cat("\n============================================================\n")
cat("PART 0: COMPOSITE CONSTRUCTION VALIDATION\n")
cat("============================================================\n")

# --- 0a: Population orthogonalization check ---
cat("\n--- 0a: Correlation with log_pop BEFORE orthogonalization ---\n")
raw_indicators <- main %>%
  select(edu_share_mun, budget_execution_mun,
         debt_ratio_mun, log_pop_mun) %>%
  filter(complete.cases(.))

cor_before <- cor(raw_indicators)[,"log_pop_mun"]
cor_before <- cor_before[names(cor_before) != "log_pop_mun"]
print(round(cor_before, 3))

cat("\n--- Correlation with log_pop AFTER orthogonalization ---\n")
resid_edu    <- residuals(lm(edu_share_mun      ~ log_pop_mun,
                             data = main, na.action = na.exclude))
resid_budget <- residuals(lm(budget_execution_mun ~ log_pop_mun,
                             data = main, na.action = na.exclude))
resid_debt   <- residuals(lm(debt_ratio_mun     ~ log_pop_mun,
                             data = main, na.action = na.exclude))

ortho_df <- data.frame(
  r_edu    = resid_edu,
  r_budget = resid_budget,
  r_debt   = resid_debt,
  log_pop  = main$log_pop_mun
) %>% filter(complete.cases(.))

cor_after <- cor(ortho_df)[,"log_pop"]
cor_after  <- cor_after[names(cor_after) != "log_pop"]
print(round(cor_after, 3))

# --- 0b: PCA on orthogonalized residuals ---
cat("\n--- 0b: PCA loadings and variance explained ---\n")
pca_data <- ortho_df %>% select(r_edu, r_budget, r_debt)
pca_out  <- prcomp(pca_data, scale. = TRUE)

var_exp <- round(pca_out$sdev^2 / sum(pca_out$sdev^2) * 100, 1)
loadings <- round(pca_out$rotation[, 1:3], 3)
rownames(loadings) <- c("Edu. spending share",
                        "Budget execution rate",
                        "Debt ratio")
colnames(loadings) <- paste0("PC", 1:3)

cat("\nVariance explained (%):\n")
print(data.frame(PC = paste0("PC", 1:3),
                 Variance_pct = var_exp))

cat("\nPCA loadings:\n")
print(loadings)

# --- 0c: Inter-indicator correlations ---
cat("\n--- 0c: Inter-indicator correlations (orthogonalized) ---\n")
cor_inter <- cor(pca_data, use = "complete.obs")
rownames(cor_inter) <- colnames(cor_inter) <-
  c("Edu. share", "Budget exec.", "Debt ratio")
print(round(cor_inter, 3))

# --- 0d: Composite correlation with CAPAG (bridge to Part 1) ---
cat("\n--- 0d: Composite vs CAPAG correlation (bridge to Part 1) ---\n")
capag_check <- main %>%
  filter(!is.na(adm_capacity_score_mun),
         !is.na(capag_numeric))
cor_capag <- cor(capag_check$adm_capacity_score_mun,
                 capag_check$capag_numeric,
                 use = "complete.obs")
cat(sprintf("  cor(composite, CAPAG) = %.3f  (n=%d)\n",
            cor_capag, nrow(capag_check)))

# --- 0e: Save validation table as LaTeX ---
cat("\n--- Saving composite validation table ---\n")
dir.create("results", showWarnings = FALSE)

val_lines <- c(
  "\\begin{table}[h]",
  "\\centering",
  "\\caption{Composite construction: PCA validation}",
  "\\begin{tabular}{lrrr}",
  "\\toprule",
  "Indicator & PC1 & PC2 & PC3 \\\\",
  "\\midrule",
  sprintf("Education spending share & %.3f & %.3f & %.3f \\\\",
          loadings["Edu. spending share","PC1"],
          loadings["Edu. spending share","PC2"],
          loadings["Edu. spending share","PC3"]),
  sprintf("Budget execution rate & %.3f & %.3f & %.3f \\\\",
          loadings["Budget execution rate","PC1"],
          loadings["Budget execution rate","PC2"],
          loadings["Budget execution rate","PC3"]),
  sprintf("Debt ratio & %.3f & %.3f & %.3f \\\\",
          loadings["Debt ratio","PC1"],
          loadings["Debt ratio","PC2"],
          loadings["Debt ratio","PC3"]),
  "\\midrule",
  sprintf("Variance explained (\\%%) & %.1f & %.1f & %.1f \\\\",
          var_exp[1], var_exp[2], var_exp[3]),
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Correlations with log population}} \\\\",
  sprintf("Before orthogonalization & \\multicolumn{3}{r}{edu=%.3f, budget=%.3f, debt=%.3f} \\\\",
          cor_before["edu_share_mun"],
          cor_before["budget_execution_mun"],
          cor_before["debt_ratio_mun"]),
  sprintf("After orthogonalization  & \\multicolumn{3}{r}{edu=%.3f, budget=%.3f, debt=%.3f} \\\\",
          cor_after["r_edu"],
          cor_after["r_budget"],
          cor_after["r_debt"]),
  "\\midrule",
  sprintf("Correlation with CAPAG grade & \\multicolumn{3}{r}{%.3f (n=%d)} \\\\",
          cor_capag, nrow(capag_check)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\\\[4pt]",
  "\\footnotesize{PCA on size-orthogonalized residuals. PC1 used as composite score.}",
  "\\end{table}"
)
writeLines(val_lines, "results/tab_composite_validation.tex")
cat("Saved results/tab_composite_validation.tex\n")

cat("\nScript 81 complete.\n")