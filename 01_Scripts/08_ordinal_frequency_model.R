# ============================================================
# 08_ordinal_frequency_model.R
# STRONG-1 Thesis
#
# Additional analysis of the ORIGINAL 5-category
# AI emotional-support frequency outcome.
#
# Outcome:
# 0 = Not at all
# 1 = A few times
# 2 = Several times a month
# 3 = Several times a week
# 4 = Almost every day / every day
#
# Analysis:
#
# A. Proportional-odds ordinal logistic model
# B. Survey-weighted parallel-slopes diagnostic
# C. Threshold-specific cumulative logistic models
#    if proportional odds is violated
#
# Thresholds:
# >0 = any emotional-support AI use
# >1 = at least several times a month
# >2 = at least several times a week
# >3 = almost every day / every day
#
# Functional forms carried forward from the primary analysis:
# Age            = linear
# Distress       = natural cubic spline, 3 df
# Loneliness     = natural cubic spline, 2 df
# Social support = natural cubic spline, 3 df
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
  stop("Could not determine the location of 08_ordinal_frequency_model.R.")
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
  "tibble",
  "writexl",
  "splines"
)

ensure_packages(packages)

library(dplyr)
library(purrr)
library(survey)
library(broom)
library(tibble)
library(writexl)
library(splines)


# ------------------------------------------------------------
# 1. ALWAYS rebuild prepared analysis data
# ------------------------------------------------------------

source(
  file.path(
    scripts_dir,
    "04_prepare_analysis_data.R"
  )
)


# ------------------------------------------------------------
# 2. Define one common analytic sample
# ------------------------------------------------------------

required_variables <- c(
  "ai_support_raw",
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

ordinal_dat <- analysis_dat %>%
  filter(
    complete.cases(
      across(
        all_of(required_variables)
      )
    )
  ) %>%
  mutate(
    ordinal_id = row_number(),
    
    ai_support_ordinal = ordered(
      ai_support_raw,
      levels = 0:4,
      labels = c(
        "Not at all",
        "A few times",
        "Several times a month",
        "Several times a week",
        "Almost every day / every day"
      )
    )
  )

cat(
  "\nOrdinal analytic sample N =",
  nrow(ordinal_dat),
  "\n"
)


# ------------------------------------------------------------
# 3. Outcome distribution
# ------------------------------------------------------------

outcome_distribution <- ordinal_dat %>%
  group_by(
    ai_support_ordinal
  ) %>%
  summarise(
    unweighted_n = n(),
    
    weighted_n = sum(
      analysis_weight
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    weighted_pct =
      100 *
      weighted_n /
      sum(weighted_n)
  )


# ------------------------------------------------------------
# 4. Fixed spline bases
#
# Same flexible forms selected in Analysis 06.
# ------------------------------------------------------------

distress_ns_object <- splines::ns(
  ordinal_dat$distress_z,
  df = 3
)

loneliness_ns_object <- splines::ns(
  ordinal_dat$loneliness_z,
  df = 2
)

support_ns_object <- splines::ns(
  ordinal_dat$social_support_z,
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

ordinal_dat <- ordinal_dat %>%
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

ordinal_design <- svydesign(
  ids = ~1,
  weights = ~analysis_weight,
  data = ordinal_dat
)


# ============================================================
# HELPERS
# ============================================================


# ------------------------------------------------------------
# 6. P-value display
# ------------------------------------------------------------

p_display_function <- function(p) {
  
  if (length(p) == 0 || is.na(p)) {
    return(NA_character_)
  }
  
  format.pval(
    p,
    digits = 3,
    eps = .001
  )
}


# ------------------------------------------------------------
# 7. Extract regTermTest
# ------------------------------------------------------------

extract_regterm <- function(
    test_object,
    label
) {
  
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
      p_display_function(
        p_value
      ),
    
    output = paste(
      capture.output(
        print(test_object)
      ),
      collapse = " "
    )
  )
}


# ------------------------------------------------------------
# 8. Extract svyolr slopes
# ------------------------------------------------------------

extract_svyolr <- function(
    model,
    model_name
) {
  
  # svyolr::coef() includes category cutpoints by default.
  # Only predictor slopes are odds ratios and belong here.
  beta <- coef(model, intercepts = FALSE)
  
  variance_matrix <- vcov(model)
  
  all_se <- sqrt(
    diag(variance_matrix)
  )
  
  se <- all_se[
    names(beta)
  ]
  
  z_value <- beta / se
  
  p_value <- 2 * pnorm(
    abs(z_value),
    lower.tail = FALSE
  )
  
  tibble(
    model = model_name,
    
    term = names(beta),
    
    estimate_log_odds =
      as.numeric(beta),
    
    SE =
      as.numeric(se),
    
    OR =
      exp(
        as.numeric(beta)
      ),
    
    CI_lower =
      exp(
        as.numeric(beta) -
          1.96 * as.numeric(se)
      ),
    
    CI_upper =
      exp(
        as.numeric(beta) +
          1.96 * as.numeric(se)
      ),
    
    p_value =
      as.numeric(p_value),
    
    p_display =
      vapply(
        p_value,
        p_display_function,
        character(1)
      )
  )
}


# ------------------------------------------------------------
# 9. Extract svyglm
# ------------------------------------------------------------

extract_svyglm <- function(
    model,
    threshold_label,
    comparison_label
) {
  
  broom::tidy(
    model,
    conf.int = TRUE
  ) %>%
    mutate(
      threshold = threshold_label,
      comparison = comparison_label,
      
      OR = exp(estimate),
      
      CI_lower = exp(conf.low),
      
      CI_upper = exp(conf.high),
      
      p_display =
        vapply(
          p.value,
          p_display_function,
          character(1)
        )
    ) %>%
    select(
      threshold,
      comparison,
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


# ------------------------------------------------------------
# 10. Weighted mean
# ------------------------------------------------------------

weighted_mean_safe <- function(
    x,
    w
) {
  
  ok <-
    !is.na(x) &
    !is.na(w)
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(
    x[ok] * w[ok]
  ) /
    sum(
      w[ok]
    )
}


# ------------------------------------------------------------
# 11. Weighted quantiles
# ------------------------------------------------------------

weighted_quantile_safe <- function(
    x,
    w,
    probs
) {
  
  ok <-
    !is.na(x) &
    !is.na(w)
  
  x <- x[ok]
  w <- w[ok]
  
  ord <- order(x)
  
  x <- x[ord]
  w <- w[ord]
  
  cumulative_weight <-
    cumsum(w) /
    sum(w)
  
  sapply(
    probs,
    
    function(p) {
      
      index <- which(
        cumulative_weight >= p
      )[1]
      
      x[index]
    }
  )
}


# ------------------------------------------------------------
# 12. Reuse stored spline basis at new value
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


# ============================================================
# PART A
# PROPORTIONAL-ODDS ORDINAL MODEL
# ============================================================


# ------------------------------------------------------------
# 13. Linear hierarchical formulas
# ------------------------------------------------------------

formula_step1 <-
  ai_support_ordinal ~
  age +
  gender_model +
  education +
  income +
  religion_binary

formula_step2 <-
  ai_support_ordinal ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  distress_z

formula_step3 <-
  ai_support_ordinal ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  distress_z +
  loneliness_z +
  social_support_z


# ------------------------------------------------------------
# 14. Fit linear proportional-odds models
# ------------------------------------------------------------

ordinal_step1 <- svyolr(
  formula_step1,
  design = ordinal_design,
  method = "logistic"
)

ordinal_step2 <- svyolr(
  formula_step2,
  design = ordinal_design,
  method = "logistic"
)

ordinal_step3 <- svyolr(
  formula_step3,
  design = ordinal_design,
  method = "logistic"
)


# ------------------------------------------------------------
# 15. Extract linear ordinal results
# ------------------------------------------------------------

linear_ordinal_results <- bind_rows(
  
  extract_svyolr(
    ordinal_step1,
    "Step 1: Demographics"
  ),
  
  extract_svyolr(
    ordinal_step2,
    "Step 2: + Distress"
  ),
  
  extract_svyolr(
    ordinal_step3,
    "Step 3: + Loneliness + Social Support"
  )
)

linear_focal_results <- linear_ordinal_results %>%
  filter(
    term %in% c(
      "distress_z",
      "loneliness_z",
      "social_support_z"
    )
  )


# ------------------------------------------------------------
# 16. Linear hierarchical block tests
# ------------------------------------------------------------

linear_test_distress <- regTermTest(
  ordinal_step2,
  ~ distress_z,
  method = "LRT"
)

linear_test_interpersonal <- regTermTest(
  ordinal_step3,
  ~ loneliness_z + social_support_z,
  method = "LRT"
)

linear_block_tests <- bind_rows(
  
  extract_regterm(
    linear_test_distress,
    "Step 2: Psychological distress"
  ),
  
  extract_regterm(
    linear_test_interpersonal,
    "Step 3: Loneliness + social support jointly"
  )
)


# ------------------------------------------------------------
# 17. Flexible proportional-odds model
# ------------------------------------------------------------

ordinal_flexible <- svyolr(
  
  ai_support_ordinal ~
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
  
  design = ordinal_design,
  method = "logistic"
)


# ------------------------------------------------------------
# 18. Flexible global tests
# ------------------------------------------------------------

flexible_global_tests <- bind_rows(
  
  extract_regterm(
    
    regTermTest(
      ordinal_flexible,
      ~ distress_ns1 +
        distress_ns2 +
        distress_ns3,
      method = "LRT"
    ),
    
    "Psychological distress"
  ),
  
  extract_regterm(
    
    regTermTest(
      ordinal_flexible,
      ~ loneliness_ns1 +
        loneliness_ns2,
      method = "LRT"
    ),
    
    "Loneliness"
  ),
  
  extract_regterm(
    
    regTermTest(
      ordinal_flexible,
      ~ support_ns1 +
        support_ns2 +
        support_ns3,
      method = "LRT"
    ),
    
    "Perceived social support"
  ),
  
  extract_regterm(
    
    regTermTest(
      ordinal_flexible,
      ~ loneliness_ns1 +
        loneliness_ns2 +
        support_ns1 +
        support_ns2 +
        support_ns3,
      method = "LRT"
    ),
    
    "Loneliness + social support jointly"
  )
)


# ============================================================
# PART B
# PROPORTIONAL-ODDS / PARALLEL-SLOPES DIAGNOSTIC
# ============================================================


# ------------------------------------------------------------
# 19. Build four stacked cumulative outcomes
#
# >0
# >1
# >2
# >3
# ------------------------------------------------------------

thresholds <- 0:3

stacked_dat <- map_dfr(
  
  thresholds,
  
  function(k) {
    
    ordinal_dat %>%
      mutate(
        threshold = factor(
          k,
          levels = thresholds,
          labels = c(
            ">0",
            ">1",
            ">2",
            ">3"
          )
        ),
        
        cumulative_outcome =
          as.integer(
            ai_support_raw > k
          ),
        
        stacked_weight =
          analysis_weight /
          length(thresholds)
      )
  }
)


# ------------------------------------------------------------
# 20. Survey design for repeated threshold observations
# ------------------------------------------------------------

stacked_design <- svydesign(
  ids = ~ordinal_id,
  weights = ~stacked_weight,
  data = stacked_dat
)


# ------------------------------------------------------------
# 21. Common-slopes cumulative model
# ------------------------------------------------------------

po_base_model <- svyglm(
  
  cumulative_outcome ~
    threshold +
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
  
  design = stacked_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 22. Fully non-parallel model
# ------------------------------------------------------------

po_nonparallel_model <- svyglm(
  
  cumulative_outcome ~
    threshold +
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
    support_ns3 +
    
    threshold:age +
    threshold:gender_model +
    threshold:education +
    threshold:income +
    threshold:religion_binary +
    
    threshold:distress_ns1 +
    threshold:distress_ns2 +
    threshold:distress_ns3 +
    
    threshold:loneliness_ns1 +
    threshold:loneliness_ns2 +
    
    threshold:support_ns1 +
    threshold:support_ns2 +
    threshold:support_ns3,
  
  design = stacked_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 23. Overall proportional-odds diagnostic
# ------------------------------------------------------------

po_overall_test <- regTermTest(
  
  po_nonparallel_model,
  
  ~ threshold:age +
    threshold:gender_model +
    threshold:education +
    threshold:income +
    threshold:religion_binary +
    threshold:distress_ns1 +
    threshold:distress_ns2 +
    threshold:distress_ns3 +
    threshold:loneliness_ns1 +
    threshold:loneliness_ns2 +
    threshold:support_ns1 +
    threshold:support_ns2 +
    threshold:support_ns3,
  
  method = "Wald"
)

po_overall <- extract_regterm(
  po_overall_test,
  "Overall proportional-odds / parallel-slopes diagnostic"
) %>%
  mutate(
    interpretation =
      case_when(
        is.na(p_value) ~
          "Review diagnostic output",
        
        p_value < .05 ~
          "Evidence that proportional odds is violated",
        
        TRUE ~
          "No evidence that proportional odds is violated"
      )
  )


# ------------------------------------------------------------
# 24. Parallel-slopes tests by predictor
# ------------------------------------------------------------

po_by_predictor <- bind_rows(
  
  extract_regterm(
    regTermTest(
      po_nonparallel_model,
      ~ threshold:age,
      method = "Wald"
    ),
    "Age"
  ),
  
  extract_regterm(
    regTermTest(
      po_nonparallel_model,
      ~ threshold:gender_model,
      method = "Wald"
    ),
    "Gender"
  ),
  
  extract_regterm(
    regTermTest(
      po_nonparallel_model,
      ~ threshold:education,
      method = "Wald"
    ),
    "Education"
  ),
  
  extract_regterm(
    regTermTest(
      po_nonparallel_model,
      ~ threshold:income,
      method = "Wald"
    ),
    "Income"
  ),
  
  extract_regterm(
    regTermTest(
      po_nonparallel_model,
      ~ threshold:religion_binary,
      method = "Wald"
    ),
    "Religion"
  ),
  
  extract_regterm(
    regTermTest(
      po_nonparallel_model,
      ~ threshold:distress_ns1 +
        threshold:distress_ns2 +
        threshold:distress_ns3,
      method = "Wald"
    ),
    "Psychological distress"
  ),
  
  extract_regterm(
    regTermTest(
      po_nonparallel_model,
      ~ threshold:loneliness_ns1 +
        threshold:loneliness_ns2,
      method = "Wald"
    ),
    "Loneliness"
  ),
  
  extract_regterm(
    regTermTest(
      po_nonparallel_model,
      ~ threshold:support_ns1 +
        threshold:support_ns2 +
        threshold:support_ns3,
      method = "Wald"
    ),
    "Perceived social support"
  )
) %>%
  mutate(
    interpretation =
      case_when(
        is.na(p_value) ~
          "Review output",
        
        p_value < .05 ~
          "Possible non-parallel effect",
        
        TRUE ~
          "No evidence of non-parallel effect"
      )
  )


# ------------------------------------------------------------
# 25. Analysis decision
# ------------------------------------------------------------

po_p <- po_overall$p_value[1]

analysis_decision <- tibble(
  item = c(
    "Overall PO p-value",
    "Preferred interpretation"
  ),
  
  value = c(
    p_display_function(
      po_p
    ),
    
    if (
      !is.na(po_p) &&
      po_p < .05
    ) {
      paste(
        "Proportional-odds assumption violated:",
        "interpret threshold-specific cumulative",
        "binary models rather than a single ordinal OR."
      )
    } else {
      paste(
        "No evidence of proportional-odds violation:",
        "the proportional-odds ordinal model may be interpreted."
      )
    }
  )
)


# ============================================================
# PART C
# THRESHOLD-SPECIFIC CUMULATIVE LOGISTIC MODELS
# ============================================================


# ------------------------------------------------------------
# 26. Threshold definitions
# ------------------------------------------------------------

threshold_information <- tibble(
  threshold_k = 0:3,
  
  threshold = c(
    ">0",
    ">1",
    ">2",
    ">3"
  ),
  
  comparison = c(
    "Any emotional-support AI use vs none",
    "At least several times a month vs less frequent",
    "At least several times a week vs less frequent",
    "Almost every day / every day vs less frequent"
  )
)


# ------------------------------------------------------------
# 27. Output containers
# ------------------------------------------------------------

threshold_linear_all <- list()
threshold_linear_focal <- list()
threshold_flexible_tests <- list()
threshold_diagnostics <- list()
threshold_model_objects <- list()


# ------------------------------------------------------------
# 28. Fit all four threshold-specific models
# ------------------------------------------------------------

for (
  i in seq_len(
    nrow(threshold_information)
  )
) {
  
  k <-
    threshold_information$threshold_k[i]
  
  threshold_label <-
    threshold_information$threshold[i]
  
  comparison_label <-
    threshold_information$comparison[i]
  
  
  threshold_dat <- ordinal_dat %>%
    mutate(
      threshold_outcome =
        as.integer(
          ai_support_raw > k
        )
    )
  
  
  threshold_design <- svydesign(
    ids = ~1,
    weights = ~analysis_weight,
    data = threshold_dat
  )
  
  
  # ----------------------------------------------------------
  # Linear model
  # ----------------------------------------------------------
  
  linear_model <- svyglm(
    
    threshold_outcome ~
      age +
      gender_model +
      education +
      income +
      religion_binary +
      distress_z +
      loneliness_z +
      social_support_z,
    
    design = threshold_design,
    family = quasibinomial()
  )
  
  
  # ----------------------------------------------------------
  # Flexible model
  # ----------------------------------------------------------
  
  flexible_model <- svyglm(
    
    threshold_outcome ~
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
    
    design = threshold_design,
    family = quasibinomial()
  )
  
  
  # ----------------------------------------------------------
  # Store models
  # ----------------------------------------------------------
  
  threshold_model_objects[[threshold_label]] <- list(
    linear = linear_model,
    flexible = flexible_model,
    data = threshold_dat
  )
  
  
  # ----------------------------------------------------------
  # Linear results
  # ----------------------------------------------------------
  
  linear_results <- extract_svyglm(
    linear_model,
    threshold_label,
    comparison_label
  )
  
  threshold_linear_all[[threshold_label]] <-
    linear_results
  
  threshold_linear_focal[[threshold_label]] <-
    linear_results %>%
    filter(
      term %in% c(
        "distress_z",
        "loneliness_z",
        "social_support_z"
      )
    )
  
  
  # ----------------------------------------------------------
  # Flexible global tests
  # ----------------------------------------------------------
  
  threshold_flexible_tests[[threshold_label]] <- bind_rows(
    
    extract_regterm(
      
      regTermTest(
        flexible_model,
        ~ distress_ns1 +
          distress_ns2 +
          distress_ns3,
        method = "Wald"
      ),
      
      "Psychological distress"
    ),
    
    extract_regterm(
      
      regTermTest(
        flexible_model,
        ~ loneliness_ns1 +
          loneliness_ns2,
        method = "Wald"
      ),
      
      "Loneliness"
    ),
    
    extract_regterm(
      
      regTermTest(
        flexible_model,
        ~ support_ns1 +
          support_ns2 +
          support_ns3,
        method = "Wald"
      ),
      
      "Perceived social support"
    ),
    
    extract_regterm(
      
      regTermTest(
        flexible_model,
        ~ loneliness_ns1 +
          loneliness_ns2 +
          support_ns1 +
          support_ns2 +
          support_ns3,
        method = "Wald"
      ),
      
      "Loneliness + social support jointly"
    )
  ) %>%
    mutate(
      threshold = threshold_label,
      comparison = comparison_label
    )
  
  
  # ----------------------------------------------------------
  # Diagnostics
  # ----------------------------------------------------------
  
  linear_coefs <- coef(
    linear_model
  )
  
  flex_coefs <- coef(
    flexible_model
  )
  
  linear_se <- sqrt(
    diag(
      vcov(
        linear_model
      )
    )
  )
  
  flex_se <- sqrt(
    diag(
      vcov(
        flexible_model
      )
    )
  )
  
  threshold_diagnostics[[threshold_label]] <- tibble(
    threshold = threshold_label,
    comparison = comparison_label,
    
    N = nrow(
      threshold_dat
    ),
    
    events = sum(
      threshold_dat$threshold_outcome == 1
    ),
    
    non_events = sum(
      threshold_dat$threshold_outcome == 0
    ),
    
    event_pct =
      100 *
      mean(
        threshold_dat$threshold_outcome
      ),
    
    linear_coefficient_count =
      length(
        linear_coefs
      ),
    
    flexible_coefficient_count =
      length(
        flex_coefs
      ),
    
    any_NA_linear =
      any(
        is.na(linear_coefs)
      ),
    
    any_NA_flexible =
      any(
        is.na(flex_coefs)
      ),
    
    max_SE_linear =
      max(
        linear_se,
        na.rm = TRUE
      ),
    
    max_SE_flexible =
      max(
        flex_se,
        na.rm = TRUE
      )
  )
}


# ------------------------------------------------------------
# 29. Combine threshold results
# ------------------------------------------------------------

threshold_linear_all <- bind_rows(
  threshold_linear_all
)

threshold_linear_focal <- bind_rows(
  threshold_linear_focal
)

threshold_flexible_tests <- bind_rows(
  threshold_flexible_tests
)

threshold_diagnostics <- bind_rows(
  threshold_diagnostics
)


# ============================================================
# PART D
# ADJUSTED MARGINAL PROBABILITIES
# ============================================================


# ------------------------------------------------------------
# 30. Delta-method marginal probability and 95% CI
# ------------------------------------------------------------

marginal_probability_ci <- function(
    model,
    newdata,
    weights
) {
  
  beta <- coef(model)
  
  variance_matrix <- vcov(model)
  
  X <- model.matrix(
    delete.response(
      terms(model)
    ),
    data = newdata
  )
  
  X <- X[
    ,
    names(beta),
    drop = FALSE
  ]
  
  eta <- as.numeric(
    X %*% beta
  )
  
  probability <- plogis(
    eta
  )
  
  normalized_weights <-
    weights /
    sum(weights)
  
  estimate <- sum(
    normalized_weights *
      probability
  )
  
  derivative_weights <-
    normalized_weights *
    probability *
    (1 - probability)
  
  gradient <- colSums(
    X *
      derivative_weights
  )
  
  variance <- as.numeric(
    t(gradient) %*%
      variance_matrix %*%
      gradient
  )
  
  SE <- sqrt(
    max(
      variance,
      0
    )
  )
  
  lower <- max(
    0,
    estimate -
      1.96 * SE
  )
  
  upper <- min(
    1,
    estimate +
      1.96 * SE
  )
  
  c(
    estimate = estimate,
    SE = SE,
    lower = lower,
    upper = upper
  )
}


# ------------------------------------------------------------
# 31. Prediction grids
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

distress_grid <- weighted_quantile_safe(
  ordinal_dat$distress_z,
  ordinal_dat$analysis_weight,
  prediction_probs
)

support_grid <- weighted_quantile_safe(
  ordinal_dat$social_support_z,
  ordinal_dat$analysis_weight,
  prediction_probs
)


# ------------------------------------------------------------
# 32. UCLA loneliness grid
#
# Raw score = 3 to 9
# Convert back to the z-scale used in analysis.
# ------------------------------------------------------------

loneliness_mean_full <- weighted_mean_safe(
  analysis_dat$loneliness,
  analysis_dat$analysis_weight
)

loneliness_sd_full <- sqrt(
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
# 33. Generate predictions for each threshold
# ------------------------------------------------------------

threshold_predictions <- list()

for (
  threshold_label in
  names(threshold_model_objects)
) {
  
  flexible_model <-
    threshold_model_objects[[threshold_label]]$flexible
  
  base_dat <-
    threshold_model_objects[[threshold_label]]$data
  
  
  # ----------------------------------------------------------
  # Distress predictions
  # ----------------------------------------------------------
  
  distress_predictions <- map_dfr(
    
    distress_grid,
    
    function(value) {
      
      basis <- get_spline_basis(
        distress_ns_object,
        value
      )
      
      newdata <- base_dat
      
      newdata$distress_ns1 <- basis[1]
      newdata$distress_ns2 <- basis[2]
      newdata$distress_ns3 <- basis[3]
      
      result <- marginal_probability_ci(
        flexible_model,
        newdata,
        base_dat$analysis_weight
      )
      
      tibble(
        threshold = threshold_label,
        
        predictor =
          "Psychological distress",
        
        predictor_value =
          value,
        
        predictor_value_z =
          value,
        
        predictor_value_scale =
          "z-score",
        
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
  
  
  # ----------------------------------------------------------
  # Loneliness predictions
  # ----------------------------------------------------------
  
  loneliness_predictions <- map2_dfr(
    
    loneliness_raw_grid,
    loneliness_z_grid,
    
    function(
    raw_value,
    z_value
    ) {
      
      basis <- get_spline_basis(
        loneliness_ns_object,
        z_value
      )
      
      newdata <- base_dat
      
      newdata$loneliness_ns1 <- basis[1]
      newdata$loneliness_ns2 <- basis[2]
      
      result <- marginal_probability_ci(
        flexible_model,
        newdata,
        base_dat$analysis_weight
      )
      
      tibble(
        threshold = threshold_label,
        
        predictor =
          "Loneliness",
        
        predictor_value =
          raw_value,
        
        predictor_value_z =
          z_value,
        
        predictor_value_scale =
          "UCLA-3 raw score",
        
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
  
  
  # ----------------------------------------------------------
  # Social-support predictions
  # ----------------------------------------------------------
  
  support_predictions <- map_dfr(
    
    support_grid,
    
    function(value) {
      
      basis <- get_spline_basis(
        support_ns_object,
        value
      )
      
      newdata <- base_dat
      
      newdata$support_ns1 <- basis[1]
      newdata$support_ns2 <- basis[2]
      newdata$support_ns3 <- basis[3]
      
      result <- marginal_probability_ci(
        flexible_model,
        newdata,
        base_dat$analysis_weight
      )
      
      tibble(
        threshold = threshold_label,
        
        predictor =
          "Perceived social support",
        
        predictor_value =
          value,
        
        predictor_value_z =
          value,
        
        predictor_value_scale =
          "z-score",
        
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
  
  
  threshold_predictions[[threshold_label]] <- bind_rows(
    distress_predictions,
    loneliness_predictions,
    support_predictions
  )
}


# ------------------------------------------------------------
# 34. Combine prediction output
# ------------------------------------------------------------

threshold_predictions <- bind_rows(
  threshold_predictions
) %>%
  left_join(
    threshold_information %>%
      select(
        threshold,
        comparison
      ),
    by = "threshold"
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
# DOCUMENTATION
# ============================================================


# ------------------------------------------------------------
# 35. Spline specification
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
# 36. Model specification
# ------------------------------------------------------------

model_specification <- tibble(
  item = c(
    "Analysis role",
    "Outcome",
    "Initial ordinal model",
    "Functional forms",
    "PO diagnostic",
    "Fallback model",
    "Interpretation rule"
  ),
  
  specification = c(
    paste(
      "Additional analysis of the original",
      "5-category AI emotional-support frequency"
    ),
    
    paste(
      "Ordered frequency from Not at all",
      "to Almost every day / every day"
    ),
    
    paste(
      "Survey-weighted proportional-odds",
      "ordinal logistic regression"
    ),
    
    paste(
      "Age linear; distress 3-df natural spline;",
      "loneliness 2-df natural spline;",
      "social support 3-df natural spline"
    ),
    
    paste(
      "Survey-weighted stacked cumulative logits",
      "with threshold-by-predictor interactions"
    ),
    
    paste(
      "Four cumulative binary logistic models:",
      ">0, >1, >2, and >3"
    ),
    
    paste(
      "If proportional odds is violated,",
      "substantive interpretation is based on",
      "threshold-specific cumulative models."
    )
  )
)


# ------------------------------------------------------------
# 37. Overall diagnostics
# ------------------------------------------------------------

diagnostics <- tibble(
  metric = c(
    "Ordinal analytic N",
    "Number of ordinal categories",
    "Unique weights",
    "Minimum weight",
    "Maximum weight",
    "Any NA coefficient in linear ordinal model",
    "Any NA coefficient in flexible ordinal model"
  ),
  
  value = as.character(
    c(
      nrow(ordinal_dat),
      
      nlevels(
        ordinal_dat$ai_support_ordinal
      ),
      
      n_distinct(
        ordinal_dat$analysis_weight
      ),
      
      min(
        ordinal_dat$analysis_weight
      ),
      
      max(
        ordinal_dat$analysis_weight
      ),
      
      any(
        is.na(
          coef(
            ordinal_step3
          )
        )
      ),
      
      any(
        is.na(
          coef(
            ordinal_flexible
          )
        )
      )
    )
  )
)


# ------------------------------------------------------------
# 38. Weight status
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
      nrow(
        ordinal_dat
      ),
      
      n_distinct(
        ordinal_dat$analysis_weight
      ),
      
      min(
        ordinal_dat$analysis_weight
      ),
      
      max(
        ordinal_dat$analysis_weight
      ),
      
      if (
        n_distinct(
          ordinal_dat$analysis_weight
        ) == 1 &&
        unique(
          ordinal_dat$analysis_weight
        ) == 1
      ) {
        "PLACEHOLDER: all weights = 1"
      } else {
        "Non-constant/final weights detected"
      }
    )
  )
)


# ------------------------------------------------------------
# 39. Reproducibility
# ------------------------------------------------------------

run_info <- tibble(
  item = c(
    "R version",
    "survey version",
    "dplyr version",
    "broom version"
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
    )
  )
)


# ============================================================
# EXPORT
# ============================================================


# ------------------------------------------------------------
# 40. Export SAFE aggregate results
# ------------------------------------------------------------

output_file <- file.path(
  safe_outputs_dir,
  "SAFE_08_ordinal_frequency_model.xlsx"
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
    Outcome_Distribution =
      outcome_distribution,
    
    Model_Specification =
      model_specification,
    
    Analysis_Decision =
      analysis_decision,
    
    Spline_Specification =
      spline_specification,
    
    Linear_Ordinal_Results =
      linear_ordinal_results,
    
    Linear_Focal_Results =
      linear_focal_results,
    
    Linear_Block_Tests =
      linear_block_tests,
    
    Flexible_Ordinal_Tests =
      flexible_global_tests,
    
    PO_Overall =
      po_overall,
    
    PO_By_Predictor =
      po_by_predictor,
    
    Threshold_Definitions =
      threshold_information,
    
    Threshold_Linear_All =
      threshold_linear_all,
    
    Threshold_Linear_Focal =
      threshold_linear_focal,
    
    Threshold_Flexible_Tests =
      threshold_flexible_tests,
    
    Threshold_Predictions =
      threshold_predictions,
    
    Threshold_Diagnostics =
      threshold_diagnostics,
    
    Diagnostics =
      diagnostics,
    
    Weight_Status =
      weight_status,
    
    Run_Info =
      run_info
  ),
  
  path = output_file
)


# ============================================================
# COMPLETION
# ============================================================

message(
  "Ordinal frequency models complete. SAFE output: ",
  project_relative(output_file)
)
