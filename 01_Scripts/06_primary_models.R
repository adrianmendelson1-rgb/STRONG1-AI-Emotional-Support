# ============================================================
# 06_primary_models.R
# STRONG-1 Thesis: Primary Models
#
# Outcome:
# Any AI emotional-support use in the previous 3 months
#
# This script reports:
#
# A. PRE-SPECIFIED LINEAR HIERARCHICAL MODEL
#    Step 1 = demographics
#    Step 2 = + psychological distress
#    Step 3 = + loneliness + perceived social support
#
# B. ASSUMPTION-CORRECTED FLEXIBLE MODEL
#    Age = linear
#    Distress = natural cubic spline (3 df)
#    Loneliness = natural cubic spline (2 df)
#    Social support = natural cubic spline (3 df)
#
# H1-H3 in the flexible model are evaluated with
# global design-based Wald tests.
#
# Adjusted marginal probabilities and 95% CIs are produced
# for interpretation of nonlinear associations.
#
# No participant-level data are exported.
# ============================================================

source_files <- Filter(Negate(is.null), lapply(sys.frames(), `[[`, "ofile"))
file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(source_files) > 0) {
  script_file <- normalizePath(tail(source_files, 1)[[1]])
} else if (length(file_argument) > 0) {
  script_file <- normalizePath(sub("^--file=", "", tail(file_argument, 1)))
} else {
  stop("Could not determine the location of 06_primary_models.R.")
}

source(file.path(dirname(script_file), "00_paths.R"))
rm(source_files, file_argument, script_file)


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
# 1. ALWAYS rebuild analysis data
#
# This guarantees that when the source dataset or weights
# change, the models use the newest version.
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
  "social_support_z",
  "analysis_weight"
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
  weights = ~analysis_weight,
  data = primary_dat
)


# ============================================================
# PART A
# PRE-SPECIFIED LINEAR HIERARCHICAL MODELS
# ============================================================


# ------------------------------------------------------------
# 6. Linear model formulas
# ------------------------------------------------------------

formula_step1 <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary


formula_step2_linear <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  distress_z


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
# 7. Fit pre-specified models
# ------------------------------------------------------------

model_step1 <- svyglm(
  formula_step1,
  design = primary_design,
  family = quasibinomial()
)


model_step2_linear <- svyglm(
  formula_step2_linear,
  design = primary_design,
  family = quasibinomial()
)


model_step3_linear <- svyglm(
  formula_step3_linear,
  design = primary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 8. Extract linear-model ORs
# ------------------------------------------------------------

p_display_function <- function(p) {
  
  format.pval(
    p,
    digits = 3,
    eps = .001
  )
}


extract_model <- function(model, model_name) {
  
  broom::tidy(
    model,
    conf.int = TRUE
  ) %>%
    mutate(
      OR = exp(estimate),
      CI_lower = exp(conf.low),
      CI_upper = exp(conf.high),
      p_display =
        p_display_function(
          p.value
        ),
      model = model_name
    ) %>%
    select(
      model,
      term,
      estimate_logit = estimate,
      SE = std.error,
      OR,
      CI_lower,
      CI_upper,
      p_value = p.value,
      p_display
    )
}


linear_model_results <- bind_rows(
  
  extract_model(
    model_step1,
    "Step 1: Demographics"
  ),
  
  extract_model(
    model_step2_linear,
    "Step 2: + Distress"
  ),
  
  extract_model(
    model_step3_linear,
    "Step 3: + Loneliness + Social Support"
  )
)


linear_focal_results <- linear_model_results %>%
  filter(
    term %in% c(
      "distress_z",
      "loneliness_z",
      "social_support_z"
    )
  )


# ------------------------------------------------------------
# 9. Pre-specified hierarchical block tests
# ------------------------------------------------------------

linear_test_H1 <- regTermTest(
  model_step2_linear,
  ~ distress_z,
  method = "Wald"
)


linear_test_interpersonal <- regTermTest(
  model_step3_linear,
  ~ loneliness_z + social_support_z,
  method = "Wald"
)


linear_block_tests <- tibble(
  
  test = c(
    "H1 / Step 2: Psychological distress",
    "Step 3: Loneliness + social support jointly"
  ),
  
  output = c(
    
    paste(
      capture.output(
        print(linear_test_H1)
      ),
      collapse = " "
    ),
    
    paste(
      capture.output(
        print(linear_test_interpersonal)
      ),
      collapse = " "
    )
  )
)


# ============================================================
# PART B
# ASSUMPTION-CORRECTED FLEXIBLE MODELS
# ============================================================


# ------------------------------------------------------------
# 10. Flexible Step 2
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
# 11. Flexible Step 3
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
# 12. Global hypothesis tests in flexible models
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
    "H1: Distress - flexible Step 2"
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
# 13. Alternative models that force ONE focal predictor
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
  weights = ~analysis_weight,
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
# 14. Model-comparison helper
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
# 15. VIF for pre-specified linear model
#
# VIF is most interpretable for the original predictor
# representation, not individual spline basis columns.
# ------------------------------------------------------------

vif_model <- glm(
  formula_step3_linear,
  data = primary_dat,
  family = binomial()
)


vif_raw <- car::vif(
  vif_model
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
# 16. Weighted helper functions
# ------------------------------------------------------------

weighted_mean_safe <- function(x, w) {
  
  ok <- !is.na(x) & !is.na(w)
  
  sum(
    x[ok] * w[ok]
  ) /
    sum(
      w[ok]
    )
}


weighted_quantile_safe <- function(
    x,
    w,
    probs
) {
  
  ok <- !is.na(x) & !is.na(w)
  
  x <- x[ok]
  w <- w[ok]
  
  ord <- order(x)
  
  x <- x[ord]
  w <- w[ord]
  
  cw <- cumsum(w) /
    sum(w)
  
  sapply(
    probs,
    function(p) {
      
      x[
        which(
          cw >= p
        )[1]
      ]
    }
  )
}


# ------------------------------------------------------------
# 17. Delta-method marginal probability + 95% CI
#
# This averages predicted risks over the actual covariate
# distribution while setting one focal predictor to a
# specified value for everyone.
# ------------------------------------------------------------

marginal_probability_ci <- function(
    model,
    newdata,
    weights
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
  
  
  w <- weights /
    sum(weights)
  
  
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
# 18. Reuse spline basis at new values
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
# 19. Prediction grids
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
  weighted_quantile_safe(
    primary_dat$distress_z,
    primary_dat$analysis_weight,
    prediction_probs
  )
)


support_grid <- unique(
  weighted_quantile_safe(
    primary_dat$social_support_z,
    primary_dat$analysis_weight,
    prediction_probs
  )
)


# UCLA-3 has actual integer scores 3-9.
# Convert these to the exact z metric used in analysis_dat.
loneliness_mean_full <-
  weighted_mean_safe(
    analysis_dat$loneliness,
    analysis_dat$analysis_weight
  )


loneliness_sd_full <-
  sqrt(
    weighted_mean_safe(
      (
        analysis_dat$loneliness -
          loneliness_mean_full
      )^2,
      analysis_dat$analysis_weight
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
# 20. Distress adjusted probabilities
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
      nd,
      primary_dat$analysis_weight
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
# 21. Loneliness adjusted probabilities
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
      nd,
      primary_dat$analysis_weight
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
# 22. Social-support adjusted probabilities
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
      nd,
      primary_dat$analysis_weight
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
# 23. Convergence / coefficient diagnostics
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
    "Maximum SE - flexible",
    "Unique weight values"
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
      ),
      
      n_distinct(
        primary_dat$
          analysis_weight
      )
    )
  )
)


# ------------------------------------------------------------
# 24. Predicted probability ranges
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
    "Pre-specified linear",
    "Assumption-corrected flexible"
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
# 25. Save spline definitions for reproducibility
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
# 26. Model specification
# ------------------------------------------------------------

model_specification <- tibble(
  
  item = c(
    "Primary outcome",
    "Analytic sample",
    "Pre-specified Step 1",
    "Pre-specified Step 2",
    "Pre-specified Step 3",
    "Assumption-corrected model",
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
        formula_step1
      ),
      collapse = " "
    ),
    
    paste(
      deparse(
        formula_step2_linear
      ),
      collapse = " "
    ),
    
    paste(
      deparse(
        formula_step3_linear
      ),
      collapse = " "
    ),
    
    paste(
      "Same covariates as the pre-specified final model;",
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
# 27. Weight status
# ------------------------------------------------------------

weight_status <- tibble(
  
  item = c(
    "Current analysis N",
    "Unique weights",
    "Minimum weight",
    "Maximum weight",
    "Status"
  ),
  
  value = as.character(
    c(
      
      nrow(primary_dat),
      
      n_distinct(
        primary_dat$
          analysis_weight
      ),
      
      min(
        primary_dat$
          analysis_weight
      ),
      
      max(
        primary_dat$
          analysis_weight
      ),
      
      ifelse(
        
        n_distinct(
          primary_dat$
            analysis_weight
        ) == 1 &&
          
          unique(
            primary_dat$
              analysis_weight
          ) == 1,
        
        "PLACEHOLDER: all weights = 1",
        
        "Non-constant/final weights detected"
      )
    )
  )
)


# ------------------------------------------------------------
# 28. Reproducibility information
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
# 29. Export SAFE aggregate results
# ------------------------------------------------------------

output_file <- file.path(
  safe_outputs_dir,
  "SAFE_06_primary_models.xlsx"
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
    
    Linear_Model_Results =
      linear_model_results,
    
    Linear_Focal_Results =
      linear_focal_results,
    
    Linear_Block_Tests =
      linear_block_tests,
    
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
    
    Weight_Status =
      weight_status,
    
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
  "Primary models complete. SAFE output: ",
  project_relative(output_file)
)
