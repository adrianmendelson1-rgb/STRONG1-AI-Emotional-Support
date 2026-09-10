# ============================================================
# 06_primary_models.R
# STRONG-1 Thesis: Primary Models
#
# Outcome:
# Any AI emotional-support use in the previous 3 months
#
# This script reports:
#
# All models adjust for age, gender, education, income, and
# religious affiliation.
#
# A. LINEAR SPECIFICATION
#    Used to test whether linearity is adequate for each
#    continuous predictor, and retained afterwards only for the
#    collinearity diagnostic and the predicted-probability range.
#
# B. FLEXIBLE INFERENTIAL MODEL
#    Age = linear (formally tested, no departure found)
#    Distress = natural cubic spline (3 df)
#    Loneliness = natural cubic spline (2 df)
#    Social support = natural cubic spline (3 df)
#
# H1-H3 are evaluated with global design-based Wald tests over
# each predictor's spline terms. Departure from linearity is
# tested separately. Adjusted marginal probabilities with 95%
# CIs describe the shape and magnitude of each association.
#
# No participant-level data are exported.
# ============================================================

# ------------------------------------------------------------
# Bootstrap: locate 00_paths.R
#
# Candidates are tested with file.exists() and the first that
# resolves is used, starting from the working directory. Nothing
# is normalised to an absolute path here: on macOS an absolute
# path drags in the parent directory names, which may contain
# non-ASCII characters, and those do not always survive the
# encoding round trip between R and the shell. Relative paths
# inside the project are pure ASCII and always do.
# ------------------------------------------------------------

paths_candidates <- c(
  file.path("01_Scripts", "00_paths.R"),
  "00_paths.R",
  file.path("..", "01_Scripts", "00_paths.R")
)

reported_script_path <- c(
  unlist(
    Filter(
      Negate(is.null),
      lapply(sys.frames(), `[[`, "ofile")
    )
  ),
  # Rscript encodes spaces in --file= as "~+~"; undo that.
  gsub(
    "~+~",
    " ",
    sub(
      "^--file=",
      "",
      grep(
        "^--file=",
        commandArgs(trailingOnly = FALSE),
        value = TRUE
      )
    ),
    fixed = TRUE
  )
)

if (length(reported_script_path) > 0) {
  paths_candidates <- c(
    file.path(
      dirname(
        tail(reported_script_path, 1)
      ),
      "00_paths.R"
    ),
    paths_candidates
  )
}

paths_file <- paths_candidates[file.exists(paths_candidates)]

if (length(paths_file) == 0) {
  stop(
    paste(
      "Could not locate 01_Scripts/00_paths.R.",
      "Run the pipeline from the project root, for example:",
      "cd <project root> && Rscript 01_Scripts/run_all.R"
    )
  )
}

source(paths_file[1])

rm(paths_candidates, reported_script_path, paths_file)


# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

packages <- c(
  "dplyr",
  "purrr",
  "survey",
  "broom",
  "car",
  "tibble",
  "writexl",
  "splines"
)

ensure_packages(packages)

library(dplyr)
library(purrr)
library(survey)
library(broom)
library(car)
library(tibble)
library(writexl)
library(splines)


# ------------------------------------------------------------
# Summary helpers
#
# Conventional unweighted sample statistics. sample_quantile()
# is R's default (type 7) quantile, the same definition
# splines::ns() uses internally to place its interior knots, so
# prediction grids and knots sit on one consistent scale.
# ------------------------------------------------------------

mean_na <- function(x) mean(x, na.rm = TRUE)

sample_quantile <- function(x, probs) {

  as.numeric(
    quantile(
      x,
      probs,
      na.rm = TRUE,
      names = FALSE
    )
  )
}



# ------------------------------------------------------------
# 1. ALWAYS rebuild analysis data
#
# This guarantees that when the source dataset changes, the
# models use the newest version.
# ------------------------------------------------------------

source(
  file.path(
    scripts_dir,
    "04_prepare_analysis_data.R"
  )
)


# ------------------------------------------------------------
# 2. ONE common analytic sample
#
# Same participants are used in all linear and flexible models.
# ------------------------------------------------------------

primary_variables <- c(
  "ai_support_binary",
  "age",
  "gender_model",
  "education",
  "income",
  "religion_binary",
  "distress_z",
  "loneliness_z",
  "social_support_z"
)

primary_dat <- analysis_dat %>%
  filter(
    complete.cases(
      across(
        all_of(primary_variables)
      )
    )
  )


cat(
  "\nPrimary analytic sample N =",
  nrow(primary_dat),
  "\n"
)


# ------------------------------------------------------------
# 3. Outcome summary
# ------------------------------------------------------------

outcome_summary <- primary_dat %>%
  count(
    ai_support_binary,
    name = "n"
  ) %>%
  mutate(
    pct = 100 * n / sum(n)
  )


# ------------------------------------------------------------
# 4. Build FIXED natural-cubic-spline bases
#
# df = 3 for distress and social support.
# df = 2 for loneliness because the UCLA-3 total
# has only seven possible observed scores.
#
# The basis is learned ONCE from the analytic sample.
# The same knots are reused for predictions.
# ------------------------------------------------------------

distress_ns_object <- splines::ns(
  primary_dat$distress_z,
  df = 3
)

loneliness_ns_object <- splines::ns(
  primary_dat$loneliness_z,
  df = 2
)

support_ns_object <- splines::ns(
  primary_dat$social_support_z,
  df = 3
)


distress_ns_matrix <- as.matrix(
  distress_ns_object
)

loneliness_ns_matrix <- as.matrix(
  loneliness_ns_object
)

support_ns_matrix <- as.matrix(
  support_ns_object
)


primary_dat <- primary_dat %>%
  mutate(
    
    distress_ns1 = distress_ns_matrix[, 1],
    distress_ns2 = distress_ns_matrix[, 2],
    distress_ns3 = distress_ns_matrix[, 3],
    
    loneliness_ns1 = loneliness_ns_matrix[, 1],
    loneliness_ns2 = loneliness_ns_matrix[, 2],
    
    support_ns1 = support_ns_matrix[, 1],
    support_ns2 = support_ns_matrix[, 2],
    support_ns3 = support_ns_matrix[, 3]
  )


# ------------------------------------------------------------
# 5. Survey design
# ------------------------------------------------------------

primary_design <- svydesign(
  ids = ~1,
  probs = ~1,
  data = primary_dat
)


# ============================================================
# PART A
# LINEAR SPECIFICATION AND FUNCTIONAL-FORM ASSESSMENT
# ============================================================


# ------------------------------------------------------------
# 6. Linear model formula
# ------------------------------------------------------------

formula_step3_linear <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  distress_z +
  loneliness_z +
  social_support_z


# ------------------------------------------------------------
# 7. Fit the linear specification
#
# This is the specification whose adequacy is tested below. It is
# retained for the collinearity diagnostic and for the predicted
# probability range, not reported as a competing model.
# ------------------------------------------------------------

model_step3_linear <- svyglm(
  formula_step3_linear,
  design = primary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 8. Coefficient extraction helper
# ------------------------------------------------------------

p_display_function <- function(p) {
  
  format.pval(
    p,
    digits = 3,
    eps = .001
  )
}


# ============================================================
# PART B
# FLEXIBLE INFERENTIAL MODELS
# ============================================================


# ------------------------------------------------------------
# 9. Flexible model, distress only
#
# Tests H1 without forcing distress to be linear.
# ------------------------------------------------------------

model_step2_flexible <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    distress_ns1 +
    distress_ns2 +
    distress_ns3,
  
  design = primary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 10. Flexible final model
#
# Distress, loneliness and social support are all
# represented flexibly.
# ------------------------------------------------------------

model_step3_flexible <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    distress_ns1 +
    distress_ns2 +
    distress_ns3 +
    loneliness_ns1 +
    loneliness_ns2 +
    support_ns1 +
    support_ns2 +
    support_ns3,
  
  design = primary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 11. Global hypothesis tests in flexible models
#
# These are the appropriate tests when a predictor
# is represented by multiple spline terms.
# ------------------------------------------------------------

flex_test_H1_step2 <- regTermTest(
  model_step2_flexible,
  ~ distress_ns1 + distress_ns2 + distress_ns3,
  method = "Wald"
)


flex_test_H1_final <- regTermTest(
  model_step3_flexible,
  ~ distress_ns1 + distress_ns2 + distress_ns3,
  method = "Wald"
)


flex_test_H2 <- regTermTest(
  model_step3_flexible,
  ~ loneliness_ns1 + loneliness_ns2,
  method = "Wald"
)


flex_test_H3 <- regTermTest(
  model_step3_flexible,
  ~ support_ns1 + support_ns2 + support_ns3,
  method = "Wald"
)


flex_test_interpersonal <- regTermTest(
  model_step3_flexible,
  ~ loneliness_ns1 +
    loneliness_ns2 +
    support_ns1 +
    support_ns2 +
    support_ns3,
  method = "Wald"
)


extract_regterm <- function(
    test_object,
    label
) {
  
  output_text <- paste(
    capture.output(
      print(test_object)
    ),
    collapse = " "
  )
  
  p_value <- NA_real_
  
  if (!is.null(test_object$p)) {
    p_value <- as.numeric(
      test_object$p[1]
    )
  }
  
  if (
    is.na(p_value) &&
    !is.null(test_object$p.value)
  ) {
    p_value <- as.numeric(
      test_object$p.value[1]
    )
  }
  
  tibble(
    test = label,
    p_value = p_value,
    p_display =
      ifelse(
        is.na(p_value),
        NA_character_,
        p_display_function(
          p_value
        )
      ),
    output = output_text
  )
}


flexible_global_tests <- bind_rows(
  
  extract_regterm(
    flex_test_H1_step2,
    "H1: Distress - distress-only flexible model"
  ),
  
  extract_regterm(
    flex_test_H1_final,
    "H1: Distress - flexible final model"
  ),
  
  extract_regterm(
    flex_test_H2,
    "H2: Loneliness - flexible final model"
  ),
  
  extract_regterm(
    flex_test_H3,
    "H3: Social support - flexible final model"
  ),
  
  extract_regterm(
    flex_test_interpersonal,
    "Loneliness + social support jointly"
  )
)


# ============================================================
# FUNCTIONAL-FORM CHECKS
# ============================================================


# ------------------------------------------------------------
# 12. Alternative models that force ONE focal predictor
# back to a linear form while the others remain flexible.
#
# This isolates evidence of nonlinearity for each variable.
# ------------------------------------------------------------

model_distress_linear_conditional <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    distress_z +
    loneliness_ns1 +
    loneliness_ns2 +
    support_ns1 +
    support_ns2 +
    support_ns3,
  
  design = primary_design,
  family = quasibinomial()
)


model_loneliness_linear_conditional <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    distress_ns1 +
    distress_ns2 +
    distress_ns3 +
    loneliness_z +
    support_ns1 +
    support_ns2 +
    support_ns3,
  
  design = primary_design,
  family = quasibinomial()
)


model_support_linear_conditional <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    distress_ns1 +
    distress_ns2 +
    distress_ns3 +
    loneliness_ns1 +
    loneliness_ns2 +
    social_support_z,
  
  design = primary_design,
  family = quasibinomial()
)


# Age remains linear in the preferred flexible model,
# but we formally check it once.
age_ns_object <- splines::ns(
  primary_dat$age,
  df = 3
)

age_ns_matrix <- as.matrix(
  age_ns_object
)

primary_dat <- primary_dat %>%
  mutate(
    age_ns1 = age_ns_matrix[, 1],
    age_ns2 = age_ns_matrix[, 2],
    age_ns3 = age_ns_matrix[, 3]
  )


# Rebuild design so it contains the age spline columns
primary_design_age <- svydesign(
  ids = ~1,
  probs = ~1,
  data = primary_dat
)


model_age_flexible_conditional <- svyglm(
  
  ai_support_binary ~
    age_ns1 +
    age_ns2 +
    age_ns3 +
    gender_model +
    education +
    income +
    religion_binary +
    distress_ns1 +
    distress_ns2 +
    distress_ns3 +
    loneliness_ns1 +
    loneliness_ns2 +
    support_ns1 +
    support_ns2 +
    support_ns3,
  
  design = primary_design_age,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 13. Model-comparison helper
# ------------------------------------------------------------

get_anova_p <- function(comparison) {
  
  if (!is.null(comparison$p)) {
    return(
      as.numeric(
        comparison$p[1]
      )
    )
  }
  
  if (!is.null(comparison$p.value)) {
    return(
      as.numeric(
        comparison$p.value[1]
      )
    )
  }
  
  NA_real_
}


compare_functional_form <- function(
    simpler_model,
    flexible_model,
    predictor_name,
    flexible_form
) {
  
  comparison <- tryCatch(
    
    anova(
      simpler_model,
      flexible_model,
      test = "F",
      method = "Wald",
      force = TRUE
    ),
    
    error = function(e) e
  )
  
  
  if (
    inherits(
      comparison,
      "error"
    )
  ) {
    
    return(
      tibble(
        predictor = predictor_name,
        flexible_form = flexible_form,
        p_value = NA_real_,
        p_display = NA_character_,
        diagnostic = "Comparison failed",
        test_output =
          conditionMessage(
            comparison
          )
      )
    )
  }
  
  
  p_val <- get_anova_p(
    comparison
  )
  
  
  tibble(
    predictor = predictor_name,
    flexible_form = flexible_form,
    p_value = p_val,
    
    p_display =
      ifelse(
        is.na(p_val),
        NA_character_,
        p_display_function(
          p_val
        )
      ),
    
    diagnostic =
      case_when(
        is.na(p_val) ~
          "Review output",
        
        p_val < .05 ~
          "Evidence of nonlinearity",
        
        TRUE ~
          "No evidence of nonlinearity"
      ),
    
    test_output =
      paste(
        capture.output(
          print(comparison)
        ),
        collapse = " "
      )
  )
}


functional_form_tests <- bind_rows(
  
  compare_functional_form(
    model_step3_flexible,
    model_age_flexible_conditional,
    "Age",
    "Natural cubic spline, 3 df"
  ),
  
  compare_functional_form(
    model_distress_linear_conditional,
    model_step3_flexible,
    "Psychological distress",
    "Natural cubic spline, 3 df"
  ),
  
  compare_functional_form(
    model_loneliness_linear_conditional,
    model_step3_flexible,
    "Loneliness",
    "Natural cubic spline, 2 df"
  ),
  
  compare_functional_form(
    model_support_linear_conditional,
    model_step3_flexible,
    "Perceived social support",
    "Natural cubic spline, 3 df"
  )
)


# ============================================================
# MULTICOLLINEARITY
# ============================================================


# ------------------------------------------------------------
# 14. VIF for the linear specification
#
# VIF is most interpretable for the original predictor
# representation, not individual spline basis columns.
# ------------------------------------------------------------

vif_raw <- car::vif(
  model_step3_linear
)


if (is.matrix(vif_raw)) {
  
  vif_results <- as.data.frame(
    vif_raw
  ) %>%
    rownames_to_column(
      "predictor"
    )
  
  if (
    all(
      c("GVIF", "Df") %in%
      names(vif_results)
    )
  ) {
    
    vif_results <- vif_results %>%
      mutate(
        adjusted_GVIF =
          GVIF^(1 / (2 * Df))
      )
  }
  
} else {
  
  vif_results <- tibble(
    predictor = names(vif_raw),
    VIF = as.numeric(vif_raw)
  )
}


# ============================================================
# ADJUSTED MARGINAL PREDICTIONS
# ============================================================


# ------------------------------------------------------------
# 15. Summary helpers
# ------------------------------------------------------------

# ------------------------------------------------------------
# 16. Delta-method marginal probability + 95% CI
#
# This averages predicted risks over the actual covariate
# distribution while setting one focal predictor to a
# specified value for everyone.
# ------------------------------------------------------------

marginal_probability_ci <- function(
    model,
    newdata
) {
  
  beta <- coef(
    model
  )
  
  V <- vcov(
    model
  )
  
  
  X <- model.matrix(
    delete.response(
      terms(model)
    ),
    data = newdata
  )
  
  
  # Ensure columns match coefficient order
  X <- X[
    ,
    names(beta),
    drop = FALSE
  ]
  
  
  eta <- as.numeric(
    X %*% beta
  )
  
  p <- plogis(
    eta
  )
  w <- rep(1 / nrow(X), nrow(X))
  
  
  estimate <- sum(
    w * p
  )
  
  
  derivative_weight <-
    w *
    p *
    (1 - p)
  
  
  gradient <- colSums(
    X *
      derivative_weight
  )
  
  
  variance <- as.numeric(
    t(gradient) %*%
      V %*%
      gradient
  )
  
  
  se <- sqrt(
    max(
      variance,
      0
    )
  )
  
  
  lower <- max(
    0,
    estimate -
      1.96 * se
  )
  
  upper <- min(
    1,
    estimate +
      1.96 * se
  )
  
  
  c(
    estimate = estimate,
    SE = se,
    lower = lower,
    upper = upper
  )
}


# ------------------------------------------------------------
# 17. Reuse spline basis at new values
# ------------------------------------------------------------

get_spline_basis <- function(
    spline_object,
    value
) {
  
  as.numeric(
    predict(
      spline_object,
      newx = value
    )
  )
}


# ------------------------------------------------------------
# 18. Prediction grids
# ------------------------------------------------------------

prediction_probs <- c(
  .05,
  .10,
  .25,
  .50,
  .75,
  .90,
  .95
)


# Adjacent quantiles can coincide when a measure is bounded and its upper
# tail piles up at the ceiling, which duplicates a prediction-grid point.
# Deduplicate so each grid value is reported once.
distress_grid <- unique(
  sample_quantile(
    primary_dat$distress_z,
    prediction_probs
  )
)


support_grid <- unique(
  sample_quantile(
    primary_dat$social_support_z,
    prediction_probs
  )
)


# UCLA-3 has actual integer scores 3-9.
# Convert these to the exact z metric used in analysis_dat.
loneliness_mean_full <-
  mean_na(
    analysis_dat$loneliness
  )


loneliness_sd_full <-
  sqrt(
    mean_na(
      (
        analysis_dat$loneliness -
          loneliness_mean_full
      )^2
    )
  )


loneliness_raw_grid <- 3:9

loneliness_z_grid <-
  (
    loneliness_raw_grid -
      loneliness_mean_full
  ) /
  loneliness_sd_full


# ------------------------------------------------------------
# 19. Distress adjusted probabilities
# ------------------------------------------------------------

pred_distress <- map_dfr(
  
  distress_grid,
  
  function(value) {
    
    basis <- get_spline_basis(
      distress_ns_object,
      value
    )
    
    
    nd <- primary_dat
    
    nd$distress_ns1 <- basis[1]
    nd$distress_ns2 <- basis[2]
    nd$distress_ns3 <- basis[3]
    
    
    result <- marginal_probability_ci(
      model_step3_flexible,
      nd
    )
    
    
    tibble(
      predictor =
        "Psychological distress",
      
      predictor_value =
        value,
      
      adjusted_probability =
        result["estimate"],
      
      SE =
        result["SE"],
      
      lower_95_CI =
        result["lower"],
      
      upper_95_CI =
        result["upper"]
    )
  }
)


# ------------------------------------------------------------
# 20. Loneliness adjusted probabilities
# ------------------------------------------------------------

pred_loneliness <- map2_dfr(
  
  loneliness_raw_grid,
  loneliness_z_grid,
  
  function(raw_value, z_value) {
    
    basis <- get_spline_basis(
      loneliness_ns_object,
      z_value
    )
    
    
    nd <- primary_dat
    
    nd$loneliness_ns1 <- basis[1]
    nd$loneliness_ns2 <- basis[2]
    
    
    result <- marginal_probability_ci(
      model_step3_flexible,
      nd
    )
    
    
    tibble(
      predictor = "Loneliness",
      
      predictor_value =
        raw_value,
      
      predictor_value_z =
        z_value,
      
      adjusted_probability =
        result["estimate"],
      
      SE =
        result["SE"],
      
      lower_95_CI =
        result["lower"],
      
      upper_95_CI =
        result["upper"]
    )
  }
)


# ------------------------------------------------------------
# 21. Social-support adjusted probabilities
# ------------------------------------------------------------

pred_support <- map_dfr(
  
  support_grid,
  
  function(value) {
    
    basis <- get_spline_basis(
      support_ns_object,
      value
    )
    
    
    nd <- primary_dat
    
    nd$support_ns1 <- basis[1]
    nd$support_ns2 <- basis[2]
    nd$support_ns3 <- basis[3]
    
    
    result <- marginal_probability_ci(
      model_step3_flexible,
      nd
    )
    
    
    tibble(
      predictor =
        "Perceived social support",
      
      predictor_value =
        value,
      
      adjusted_probability =
        result["estimate"],
      
      SE =
        result["SE"],
      
      lower_95_CI =
        result["lower"],
      
      upper_95_CI =
        result["upper"]
    )
  }
)


adjusted_predictions <- bind_rows(
  pred_distress,
  pred_loneliness,
  pred_support
) %>%
  mutate(
    
    adjusted_probability_pct =
      100 *
      adjusted_probability,
    
    lower_95_CI_pct =
      100 *
      lower_95_CI,
    
    upper_95_CI_pct =
      100 *
      upper_95_CI
  )


# ============================================================
# MODEL DIAGNOSTICS
# ============================================================


# ------------------------------------------------------------
# 22. Convergence / coefficient diagnostics
# ------------------------------------------------------------

linear_coefs <- coef(
  model_step3_linear
)

flex_coefs <- coef(
  model_step3_flexible
)


linear_ses <- sqrt(
  diag(
    vcov(
      model_step3_linear
    )
  )
)


flex_ses <- sqrt(
  diag(
    vcov(
      model_step3_flexible
    )
  )
)


model_diagnostics <- tibble(
  
  metric = c(
    "Primary analytic N",
    "Outcome events",
    "Outcome non-events",
    "Linear final coefficient count",
    "Flexible final coefficient count",
    "Any NA coefficients - linear",
    "Any NA coefficients - flexible",
    "Maximum SE - linear",
    "Maximum SE - flexible"
  ),
  
  value = as.character(
    c(
      
      nrow(primary_dat),
      
      sum(
        primary_dat$
          ai_support_binary == 1
      ),
      
      sum(
        primary_dat$
          ai_support_binary == 0
      ),
      
      length(
        linear_coefs
      ),
      
      length(
        flex_coefs
      ),
      
      any(
        is.na(
          linear_coefs
        )
      ),
      
      any(
        is.na(
          flex_coefs
        )
      ),
      
      max(
        linear_ses,
        na.rm = TRUE
      ),
      
      max(
        flex_ses,
        na.rm = TRUE
      )
    )
  )
)


# ------------------------------------------------------------
# 23. Predicted probability ranges
# ------------------------------------------------------------

linear_predictions <- predict(
  model_step3_linear,
  type = "response"
)

flex_predictions <- predict(
  model_step3_flexible,
  type = "response"
)


prediction_range <- tibble(
  
  model = c(
    "Linear specification",
    "Flexible inferential model"
  ),
  
  minimum = c(
    min(
      linear_predictions,
      na.rm = TRUE
    ),
    min(
      flex_predictions,
      na.rm = TRUE
    )
  ),
  
  median = c(
    median(
      linear_predictions,
      na.rm = TRUE
    ),
    median(
      flex_predictions,
      na.rm = TRUE
    )
  ),
  
  maximum = c(
    max(
      linear_predictions,
      na.rm = TRUE
    ),
    max(
      flex_predictions,
      na.rm = TRUE
    )
  )
)


# ============================================================
# KNOT / MODEL DOCUMENTATION
# ============================================================


# ------------------------------------------------------------
# 24. Save spline definitions for reproducibility
# ------------------------------------------------------------

format_vector <- function(x) {
  
  paste(
    round(
      as.numeric(x),
      6
    ),
    collapse = " | "
  )
}


spline_specification <- tibble(
  
  predictor = c(
    "Psychological distress",
    "Loneliness",
    "Perceived social support"
  ),
  
  df = c(
    3,
    2,
    3
  ),
  
  interior_knots = c(
    
    format_vector(
      attr(
        distress_ns_object,
        "knots"
      )
    ),
    
    format_vector(
      attr(
        loneliness_ns_object,
        "knots"
      )
    ),
    
    format_vector(
      attr(
        support_ns_object,
        "knots"
      )
    )
  ),
  
  boundary_knots = c(
    
    format_vector(
      attr(
        distress_ns_object,
        "Boundary.knots"
      )
    ),
    
    format_vector(
      attr(
        loneliness_ns_object,
        "Boundary.knots"
      )
    ),
    
    format_vector(
      attr(
        support_ns_object,
        "Boundary.knots"
      )
    )
  )
)


# ------------------------------------------------------------
# 25. Model specification
# ------------------------------------------------------------

model_specification <- tibble(
  
  item = c(
    "Primary outcome",
    "Analytic sample",
    "Linear specification",
    "Inferential model",
    "Age",
    "Psychological distress",
    "Loneliness",
    "Perceived social support",
    "Gender reference",
    "Education reference",
    "Income reference",
    "Religion reference"
  ),
  
  specification = c(
    
    "Any AI emotional-support use: 0=no use, 1=any use",
    
    paste0(
      "Common complete-case N = ",
      nrow(primary_dat)
    ),
    
    paste(
      deparse(
        formula_step3_linear
      ),
      collapse = " "
    ),
    
    paste(
      "Same covariates as the linear specification;",
      "distress and social support use 3-df natural cubic splines;",
      "loneliness uses a 2-df natural cubic spline."
    ),
    
    "Linear, per 1-year increase",
    
    "Standardized; natural cubic spline, 3 df",
    
    "Standardized; natural cubic spline, 2 df",
    
    "Standardized; natural cubic spline, 3 df",
    
    "Female",
    
    "Secondary education",
    
    "About average income",
    
    "Jewish"
  )
)


# ------------------------------------------------------------
# 26. Reproducibility information
# ------------------------------------------------------------

run_info <- tibble(
  
  item = c(
    "R version",
    "survey version",
    "dplyr version",
    "broom version",
    "car version"
  ),
  
  value = c(
    
    R.version.string,
    
    as.character(
      packageVersion(
        "survey"
      )
    ),
    
    as.character(
      packageVersion(
        "dplyr"
      )
    ),
    
    as.character(
      packageVersion(
        "broom"
      )
    ),
    
    as.character(
      packageVersion(
        "car"
      )
    )
  )
)


# ============================================================
# EXPORT
# ============================================================


# ------------------------------------------------------------
# 27. Export aggregate results
# ------------------------------------------------------------

output_file <- file.path(
  analysis_outputs_dir,
  "Analysis_06_primary_models.xlsx"
)


if (
  file.exists(
    output_file
  )
) {
  file.remove(
    output_file
  )
}


write_xlsx(
  
  list(
    
    Outcome_Summary =
      outcome_summary,
    
    Model_Specification =
      model_specification,
    
    Spline_Specification =
      spline_specification,
    
    Flexible_Global_Tests =
      flexible_global_tests,
    
    Functional_Form_Tests =
      functional_form_tests,
    
    Adjusted_Predictions =
      adjusted_predictions,
    
    VIF =
      vif_results,
    
    Diagnostics =
      model_diagnostics,
    
    Prediction_Range =
      prediction_range,
    
    Run_Info =
      run_info
  ),
  
  path =
    output_file
)


# ------------------------------------------------------------
# Completion
# ------------------------------------------------------------

message(
  "Primary models complete. Output: ",
  project_relative(output_file)
)
