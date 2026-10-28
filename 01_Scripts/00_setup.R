# 00_setup.R
# Shared configuration: locale, paths, packages and the helpers used
# by more than one script. Sourced at the top of every other script.


# R must run in a UTF-8 locale when the project path contains
# non-ASCII characters; Rscript does not always inherit one.
if (!isTRUE(l10n_info()$MBCS)) {
  for (locale in c(Sys.getenv("LANG"), "en_US.UTF-8", "C.UTF-8")) {
    if (grepl("utf-?8", locale, ignore.case = TRUE) &&
        nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", locale)))) {
      Sys.setenv(LC_CTYPE = locale)
      break
    }
  }
}

# Work from the project root so every path below is relative (and
# therefore ASCII), whether a script is run from the root or from
# 01_Scripts.
if (!file.exists(file.path("01_Scripts", "00_setup.R"))) {
  setwd("..")
}
if (!file.exists(file.path("01_Scripts", "00_setup.R"))) {
  stop("Run the pipeline from the project root: Rscript 01_Scripts/run_all.R")
}


# ---- Paths -------------------------------------------------------

data_file <- Sys.getenv("STRONG1_DATA", unset = file.path("data", "STRONG1Data.xlsx"))

analysis_dir <- "02_Analysis_Outputs"
main_dir <- "03_Main_Outputs"
supplement_dir <- "04_Supplement"

for (d in c(analysis_dir, main_dir, supplement_dir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}


# ---- Packages ----------------------------------------------------

# Missing packages are installed from the configured repository, or from
# the CRAN cloud mirror when none is set (as under a default Rscript).
use_packages <- function(...) {
  packages <- c(...)
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  repos <- getOption("repos")
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")
  if (length(missing) > 0) install.packages(missing, repos = repos)
  invisible(lapply(packages, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
}

use_packages("dplyr", "purrr", "tibble", "readxl", "writexl", "survey")


# ---- Shared helpers ----------------------------------------------

# Sample standardization (SD divisor n - 1).
standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Proportion with a logit-transformed 95% CI (equal-probability
# design, so the standard error is the usual binomial one).
prop_ci <- function(indicator) {
  indicator <- indicator[!is.na(indicator)]
  design <- svydesign(ids = ~1, probs = ~1, data = data.frame(indicator))
  estimate <- svyciprop(~indicator, design, method = "logit")
  ci <- confint(estimate)
  tibble(
    n_valid = length(indicator),
    n_selected = sum(indicator == 1),
    pct = 100 * mean(indicator),
    CI_lower = 100 * max(0, ci[1]),
    CI_upper = 100 * min(1, ci[2])
  )
}

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

# Odds ratios with Wald 95% CIs (all non-intercept terms unless
# `terms` is given).
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

# Write one aggregate workbook to 02_Analysis_Outputs, with the R and
# package versions used.
write_analysis <- function(sheets, file) {
  packages <- c("survey", "dplyr", "car", "psych")
  sheets$Run_Info <- tibble(
    item = c("R", packages),
    version = c(R.version.string, vapply(packages, function(p) {
      if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else NA_character_
    }, character(1)))
  )
  path <- file.path(analysis_dir, file)
  write_xlsx(sheets, path)
  message("Written: ", path)
}
