# 00_setup.R
# Paths, packages and helpers shared by the pipeline. Every script is run
# from the project root and sources this file first.

# Rscript does not always start in a UTF-8 locale, which R needs when the
# project path contains non-ASCII characters.
if (!isTRUE(l10n_info()$MBCS)) {
  for (locale in c(Sys.getenv("LANG"), "en_US.UTF-8", "C.UTF-8")) {
    if (grepl("utf-?8", locale, ignore.case = TRUE) &&
        nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", locale)))) {
      Sys.setenv(LC_CTYPE = locale)
      break
    }
  }
}

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readxl)
  library(writexl)
  library(survey)
})

raw_file <- file.path("data", "STRONG1Data.xlsx")
analysis_file <- file.path("data", "STRONG1_T1_analysis_data.rds")
analysis_dir <- "02_Analysis_Outputs"
main_dir <- "03_Main_Outputs"
supplement_dir <- "04_Supplement"

# Experiences of AI emotional support and topics discussed, asked only of
# emotional-support users.
q04_items <- sprintf("MH_AI_Q04_R%02d", 0:6)
q05_items <- sprintf("MH_AI_Q05_R%02d", 1:12)
item_labels <- setNames(c(
  "It helped me calm down or cope with distress", "It helped me understand my feelings or gain insight",
  "I shared things I couldn't tell people", "I used it instead of reaching out to people",
  "I found myself relying on it repeatedly for comfort",
  "The responses were inaccurate, unhelpful, or frustrating",
  "It made me feel worse, more confused, or misunderstood",
  "Anxiety / worries / panic attacks", "Stress / overload / burnout", "Low mood / depression",
  "Overwhelm or difficulty with emotional regulation", "Sleep difficulties",
  "Loneliness / need for conversation and support", "Interpersonal relationships and conflicts",
  "Trauma, mourning, or difficult memories", "Body image / eating or harmful habits",
  "Personal dilemmas / values / decision making", "Regarding professional help (before / after)", "Other"
), c(q04_items, q05_items))

# All models are logistic regressions fitted with svyglm on an
# equal-probability design: unweighted estimates with design-based
# (robust) standard errors and Wald tests.
fit_logit <- function(data, terms, outcome = "ai_support_binary") {
  svyglm(
    reformulate(terms, response = outcome),
    design = svydesign(ids = ~1, probs = ~1, data = data),
    family = quasibinomial()
  )
}

# Odds ratios with Wald 95% CIs (all non-intercept terms unless `terms`
# is given).
odds_ratios <- function(model, terms = NULL) {
  est <- coef(model)[-1]
  se <- sqrt(diag(vcov(model)))[-1]
  ci <- confint(model)[-1, , drop = FALSE]
  out <- tibble(
    term = names(est),
    OR = exp(est),
    CI_lower = exp(ci[, 1]),
    CI_upper = exp(ci[, 2]),
    p_value = 2 * pt(-abs(est / se), df = model$df.residual)
  )
  if (is.null(terms)) out else filter(out, term %in% terms)
}

# Multi-parameter Wald F test for a set of terms in a fitted model.
wald_test <- function(model, terms, label) {
  w <- regTermTest(model, reformulate(terms), method = "Wald")
  tibble(test = label, F = as.numeric(w$Ftest), df_num = w$df, df_den = w$ddf, p_value = as.numeric(w$p))
}

# Generalized VIFs from the equivalent glm.
vif_table <- function(data, terms, outcome = "ai_support_binary") {
  v <- car::vif(glm(reformulate(terms, response = outcome), data = data, family = binomial()))
  if (!is.matrix(v)) v <- cbind(GVIF = v, Df = 1)
  tibble(predictor = rownames(v), GVIF = v[, "GVIF"], Df = v[, "Df"], adjusted_GVIF = v[, "GVIF"]^(1 / (2 * v[, "Df"])))
}

# Writes an aggregate workbook to 02_Analysis_Outputs, recording the R and
# package versions of the run.
write_analysis <- function(sheets, file) {
  packages <- c("survey", "dplyr", "car")
  sheets$Run_Info <- tibble(
    item = c("R", packages),
    version = c(R.version.string, vapply(packages, function(p) as.character(packageVersion(p)), ""))
  )
  write_xlsx(sheets, file.path(analysis_dir, file))
}
