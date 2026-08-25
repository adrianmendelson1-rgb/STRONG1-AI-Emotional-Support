# ============================================================
# 10_robustness_sensitivity.R
# STRONG-1 Thesis
#
# ROBUSTNESS / SENSITIVITY ANALYSES
#
# A. Distress-component robustness:
#    Replace the distress composite with standardized
#    PHQ, GAD, and PSS entered simultaneously.
#
#    All OTHER continuous predictors retain the preferred
#    functional forms established in Analysis 06:
#       Age            = linear
#       Loneliness     = natural spline, 2 df
#       Social support = natural spline, 3 df
#
# B. Logical-inconsistency sensitivity:
#    Exclude participants reporting Q01 = 0 but Q02 > 0.
#
# C. Weighting sensitivity:
#    Compare survey-weighted versus equal-weight versions
#    of the same primary model.
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
  stop("Could not determine the location of 10_robustness_sensitivity.R.")
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
# 1. ALWAYS rebuild prepared analysis data
# ------------------------------------------------------------

source(
  file.path(
    scripts_dir,
    "04_prepare_analysis_data.R"
  )
)


# ------------------------------------------------------------
# 2. Validate required variables
# ------------------------------------------------------------

required_variables <- c(
  "ai_support_binary",
  "ai_support_raw",
  "ai_general_raw",
  "age",
  "gender_model",
  "education",
  "income",
  "religion_binary",
  "phq",
  "gad",
  "pss",
  "distress_z",
  "loneliness",
  "loneliness_z",
  "social_support_z",
  "analysis_weight"
)

missing_variables <- setdiff(
  required_variables,
  names(analysis_dat)
)

if (length(missing_variables) > 0) {
  stop(
    paste(
      "Required variables missing:",
      paste(
        missing_variables,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# HELPERS
# ============================================================


p_display_function <- function(p) {
  
  if (
    length(p) == 0 ||
    is.na(p)
  ) {
    return(NA_character_)
  }
  
  format.pval(
    p,
    digits = 3,
    eps = .001
  )
}


weighted_mean_safe <- function(x, w) {
  
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


weighted_sd_safe <- function(x, w) {
  
  ok <-
    !is.na(x) &
    !is.na(w)
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  x <- x[ok]
  w <- w[ok]
  
  mean_x <-
    weighted_mean_safe(
      x,
      w
    )
  
  sd_x <- sqrt(
    sum(
      w *
        (
          x -
            mean_x
        )^2
    ) /
      sum(w)
  )
  
  if (
    is.na(sd_x) ||
    sd_x <= 0
  ) {
    stop(
      "Weighted SD is zero or invalid."
    )
  }
  
  sd_x
}


weighted_z <- function(x, w) {
  
  (
    x -
      weighted_mean_safe(
        x,
        w
      )
  ) /
    weighted_sd_safe(
      x,
      w
    )
}


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
  
  result <- sapply(
    probs,
    function(p) {
      
      index <- which(
        cumulative_weight >= p
      )[1]
      
      x[index]
    }
  )
  
  as.numeric(
    result
  )
}


unique_grid <- function(x) {
  
  sort(
    unique(
      as.numeric(x)
    )
  )
}


extract_svyglm <- function(
    model,
    model_name
) {
  
  broom::tidy(
    model,
    conf.int = TRUE
  ) %>%
    mutate(
      model =
        model_name,
      
      OR =
        exp(estimate),
      
      CI_lower =
        exp(conf.low),
      
      CI_upper =
        exp(conf.high),
      
      p_display =
        vapply(
          p.value,
          p_display_function,
          character(1)
        )
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


extract_regterm <- function(
    test_object,
    label
) {
  
  p_value <- NA_real_
  
  if (!is.null(test_object$p)) {
    p_value <-
      as.numeric(
        test_object$p[1]
      )
  }
  
  if (
    is.na(p_value) &&
    !is.null(test_object$p.value)
  ) {
    p_value <-
      as.numeric(
        test_object$p.value[1]
      )
  }
  
  tibble(
    test =
      label,
    
    p_value =
      p_value,
    
    p_display =
      p_display_function(
        p_value
      ),
    
    output =
      paste(
        capture.output(
          print(test_object)
        ),
        collapse = " "
      )
  )
}


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


marginal_probability <- function(
    model,
    newdata,
    weights
) {
  
  predictions <- predict(
    model,
    newdata = newdata,
    type = "response"
  )
  
  weighted_mean_safe(
    as.numeric(predictions),
    weights
  )
}


assert_unique_curve <- function(
    data,
    label
) {
  
  duplicates <- data %>%
    count(
      predictor,
      predictor_value
    ) %>%
    filter(
      n > 1
    )
  
  if (
    nrow(duplicates) > 0
  ) {
    stop(
      paste(
        "Duplicate prediction-grid rows detected in",
        label
      )
    )
  }
  
  data
}


# ============================================================
# PREPARATION
# ============================================================


# ------------------------------------------------------------
# 3. Standardize PHQ / GAD / PSS separately
# ------------------------------------------------------------

analysis_dat <- analysis_dat %>%
  mutate(
    phq_component_z =
      weighted_z(
        phq,
        analysis_weight
      ),
    
    gad_component_z =
      weighted_z(
        gad,
        analysis_weight
      ),
    
    pss_component_z =
      weighted_z(
        pss,
        analysis_weight
      )
  )


# ------------------------------------------------------------
# 4. Primary analytic sample
# ------------------------------------------------------------

primary_variables <- c(
  "ai_support_binary",
  "age",
  "gender_model",
  "education",
  "income",
  "religion_binary",
  "distress_z",
  "loneliness",
  "loneliness_z",
  "social_support_z",
  "analysis_weight"
)

primary_dat <- analysis_dat %>%
  filter(
    complete.cases(
      across(
        all_of(
          primary_variables
        )
      )
    )
  ) %>%
  mutate(
    ai_logical_inconsistency =
      ai_general_raw == 0 &
      ai_support_raw > 0
  )


# ------------------------------------------------------------
# 5. Fixed spline bases
#
# Same definitions as Analysis 06.
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
    distress_ns1 =
      distress_ns_matrix[, 1],
    
    distress_ns2 =
      distress_ns_matrix[, 2],
    
    distress_ns3 =
      distress_ns_matrix[, 3],
    
    loneliness_ns1 =
      loneliness_ns_matrix[, 1],
    
    loneliness_ns2 =
      loneliness_ns_matrix[, 2],
    
    support_ns1 =
      support_ns_matrix[, 1],
    
    support_ns2 =
      support_ns_matrix[, 2],
    
    support_ns3 =
      support_ns_matrix[, 3]
  )


# ------------------------------------------------------------
# 6. Survey designs
# ------------------------------------------------------------

primary_design_weighted <- svydesign(
  ids = ~1,
  weights = ~analysis_weight,
  data = primary_dat
)

primary_design_unweighted <- svydesign(
  ids = ~1,
  weights = ~1,
  data = primary_dat
)


# ------------------------------------------------------------
# 7. Primary formulas
# ------------------------------------------------------------

primary_linear_formula <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  distress_z +
  loneliness_z +
  social_support_z


primary_flexible_formula <-
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
  support_ns3


# ============================================================
# PART A
# DISTRESS-COMPONENT ROBUSTNESS
# ============================================================


# ------------------------------------------------------------
# 8. Component sample
# ------------------------------------------------------------

component_dat <- primary_dat %>%
  filter(
    complete.cases(
      phq_component_z,
      gad_component_z,
      pss_component_z
    )
  )


component_design <- svydesign(
  ids = ~1,
  weights = ~analysis_weight,
  data = component_dat
)


# ------------------------------------------------------------
# 9. Component model
#
# IMPORTANT:
# Only the distress representation changes.
# Loneliness and social support retain the flexible forms
# selected in Analysis 06.
# ------------------------------------------------------------

component_formula <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  phq_component_z +
  gad_component_z +
  pss_component_z +
  loneliness_ns1 +
  loneliness_ns2 +
  support_ns1 +
  support_ns2 +
  support_ns3


component_model <- svyglm(
  component_formula,
  design = component_design,
  family = quasibinomial()
)


component_results <- extract_svyglm(
  component_model,
  "PHQ, GAD and PSS entered simultaneously"
)


component_focal_results <- component_results %>%
  filter(
    term %in% c(
      "phq_component_z",
      "gad_component_z",
      "pss_component_z"
    )
  )


component_joint_test <- extract_regterm(
  
  regTermTest(
    component_model,
    ~ phq_component_z +
      gad_component_z +
      pss_component_z,
    method = "Wald"
  ),
  
  "PHQ + GAD + PSS jointly"
)


# ------------------------------------------------------------
# 10. Component correlations
# ------------------------------------------------------------

weighted_correlation <- function(
    x,
    y,
    w
) {
  
  ok <-
    !is.na(x) &
    !is.na(y) &
    !is.na(w)
  
  result <- cov.wt(
    cbind(
      x[ok],
      y[ok]
    ),
    wt = w[ok],
    cor = TRUE
  )
  
  as.numeric(
    result$cor[
      1,
      2
    ]
  )
}


component_correlations <- tibble(
  pair = c(
    "PHQ-GAD",
    "PHQ-PSS",
    "GAD-PSS"
  ),
  
  weighted_correlation = c(
    weighted_correlation(
      component_dat$phq,
      component_dat$gad,
      component_dat$analysis_weight
    ),
    
    weighted_correlation(
      component_dat$phq,
      component_dat$pss,
      component_dat$analysis_weight
    ),
    
    weighted_correlation(
      component_dat$gad,
      component_dat$pss,
      component_dat$analysis_weight
    )
  )
)


# ------------------------------------------------------------
# 11. Multicollinearity diagnostic
#
# VIF is calculated on the linear predictor representation
# because individual spline-basis VIFs are not substantively
# interpretable.
# ------------------------------------------------------------

component_vif_formula <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  phq_component_z +
  gad_component_z +
  pss_component_z +
  loneliness_z +
  social_support_z


component_vif_model <- glm(
  component_vif_formula,
  data = component_dat,
  family = binomial()
)


component_vif_raw <- car::vif(
  component_vif_model
)


if (is.matrix(component_vif_raw)) {
  
  component_vif <- as.data.frame(
    component_vif_raw
  ) %>%
    rownames_to_column(
      "predictor"
    )
  
  if (
    all(
      c(
        "GVIF",
        "Df"
      ) %in%
      names(component_vif)
    )
  ) {
    
    component_vif <- component_vif %>%
      mutate(
        adjusted_GVIF =
          GVIF^(
            1 /
              (
                2 * Df
              )
          )
      )
  }
  
} else {
  
  component_vif <- tibble(
    predictor =
      names(
        component_vif_raw
      ),
    
    VIF =
      as.numeric(
        component_vif_raw
      )
  )
}


# ============================================================
# PART B
# LOGICAL-INCONSISTENCY SENSITIVITY
# ============================================================


# ------------------------------------------------------------
# 12. Full primary models
# ------------------------------------------------------------

full_linear_model <- svyglm(
  primary_linear_formula,
  design = primary_design_weighted,
  family = quasibinomial()
)

full_flexible_model <- svyglm(
  primary_flexible_formula,
  design = primary_design_weighted,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 13. Exclude inconsistent cases
# ------------------------------------------------------------

sensitivity_dat <- primary_dat %>%
  filter(
    !ai_logical_inconsistency
  )


sensitivity_design <- svydesign(
  ids = ~1,
  weights = ~analysis_weight,
  data = sensitivity_dat
)


sensitivity_linear_model <- svyglm(
  primary_linear_formula,
  design = sensitivity_design,
  family = quasibinomial()
)

sensitivity_flexible_model <- svyglm(
  primary_flexible_formula,
  design = sensitivity_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 14. Linear comparison
# ------------------------------------------------------------

full_linear_focal <- extract_svyglm(
  full_linear_model,
  "Primary full sample"
) %>%
  filter(
    term %in% c(
      "distress_z",
      "loneliness_z",
      "social_support_z"
    )
  ) %>%
  select(
    term,
    full_OR = OR,
    full_CI_lower = CI_lower,
    full_CI_upper = CI_upper,
    full_p = p_value
  )


sensitivity_linear_focal <- extract_svyglm(
  sensitivity_linear_model,
  "Exclude logical inconsistencies"
) %>%
  filter(
    term %in% c(
      "distress_z",
      "loneliness_z",
      "social_support_z"
    )
  ) %>%
  select(
    term,
    sensitivity_OR = OR,
    sensitivity_CI_lower = CI_lower,
    sensitivity_CI_upper = CI_upper,
    sensitivity_p = p_value
  )


inconsistency_linear_comparison <- full_linear_focal %>%
  left_join(
    sensitivity_linear_focal,
    by = "term"
  ) %>%
  mutate(
    OR_absolute_difference =
      abs(
        sensitivity_OR -
          full_OR
      ),
    
    log_OR_absolute_difference =
      abs(
        log(
          sensitivity_OR
        ) -
          log(
            full_OR
          )
      )
  )


# ------------------------------------------------------------
# 15. Flexible global-test helper
# ------------------------------------------------------------

get_flexible_tests <- function(
    model,
    model_label
) {
  
  bind_rows(
    
    extract_regterm(
      regTermTest(
        model,
        ~ distress_ns1 +
          distress_ns2 +
          distress_ns3,
        method = "Wald"
      ),
      "Psychological distress"
    ),
    
    extract_regterm(
      regTermTest(
        model,
        ~ loneliness_ns1 +
          loneliness_ns2,
        method = "Wald"
      ),
      "Loneliness"
    ),
    
    extract_regterm(
      regTermTest(
        model,
        ~ support_ns1 +
          support_ns2 +
          support_ns3,
        method = "Wald"
      ),
      "Perceived social support"
    ),
    
    extract_regterm(
      regTermTest(
        model,
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
      model =
        model_label
    )
}


inconsistency_flexible_tests <- bind_rows(
  get_flexible_tests(
    full_flexible_model,
    "Primary full sample"
  ),
  
  get_flexible_tests(
    sensitivity_flexible_model,
    "Exclude logical inconsistencies"
  )
)


# ============================================================
# PART C
# FLEXIBLE PREDICTION CURVES
# ============================================================


# ------------------------------------------------------------
# 16. UNIQUE common prediction grids
#
# unique() is essential because scale ceilings can make
# multiple weighted percentiles identical.
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


distress_grid <- unique_grid(
  weighted_quantile_safe(
    primary_dat$distress_z,
    primary_dat$analysis_weight,
    prediction_probs
  )
)


support_grid <- unique_grid(
  weighted_quantile_safe(
    primary_dat$social_support_z,
    primary_dat$analysis_weight,
    prediction_probs
  )
)


loneliness_mapping <- primary_dat %>%
  group_by(
    loneliness
  ) %>%
  summarise(
    loneliness_z_value =
      mean(
        loneliness_z
      ),
    .groups = "drop"
  ) %>%
  arrange(
    loneliness
  )


prediction_grid_summary <- bind_rows(
  
  tibble(
    predictor =
      "Psychological distress",
    predictor_value =
      distress_grid
  ),
  
  tibble(
    predictor =
      "Loneliness",
    predictor_value =
      loneliness_mapping$loneliness
  ),
  
  tibble(
    predictor =
      "Perceived social support",
    predictor_value =
      support_grid
  )
)


# ------------------------------------------------------------
# 17. Curve generator
# ------------------------------------------------------------

make_flexible_curve <- function(
    model,
    base_dat,
    predictor,
    weighting_vector
) {
  
  if (
    predictor ==
    "Psychological distress"
  ) {
    
    output <- map_dfr(
      distress_grid,
      
      function(value) {
        
        basis <- get_spline_basis(
          distress_ns_object,
          value
        )
        
        newdata <- base_dat
        
        newdata$distress_ns1 <-
          basis[1]
        
        newdata$distress_ns2 <-
          basis[2]
        
        newdata$distress_ns3 <-
          basis[3]
        
        tibble(
          predictor =
            predictor,
          
          predictor_value =
            value,
          
          adjusted_probability =
            marginal_probability(
              model,
              newdata,
              weighting_vector
            )
        )
      }
    )
  }
  
  
  if (
    predictor ==
    "Loneliness"
  ) {
    
    output <- map_dfr(
      seq_len(
        nrow(
          loneliness_mapping
        )
      ),
      
      function(i) {
        
        raw_value <-
          loneliness_mapping$
          loneliness[i]
        
        z_value <-
          loneliness_mapping$
          loneliness_z_value[i]
        
        basis <- get_spline_basis(
          loneliness_ns_object,
          z_value
        )
        
        newdata <- base_dat
        
        newdata$loneliness_ns1 <-
          basis[1]
        
        newdata$loneliness_ns2 <-
          basis[2]
        
        tibble(
          predictor =
            predictor,
          
          predictor_value =
            raw_value,
          
          adjusted_probability =
            marginal_probability(
              model,
              newdata,
              weighting_vector
            )
        )
      }
    )
  }
  
  
  if (
    predictor ==
    "Perceived social support"
  ) {
    
    output <- map_dfr(
      support_grid,
      
      function(value) {
        
        basis <- get_spline_basis(
          support_ns_object,
          value
        )
        
        newdata <- base_dat
        
        newdata$support_ns1 <-
          basis[1]
        
        newdata$support_ns2 <-
          basis[2]
        
        newdata$support_ns3 <-
          basis[3]
        
        tibble(
          predictor =
            predictor,
          
          predictor_value =
            value,
          
          adjusted_probability =
            marginal_probability(
              model,
              newdata,
              weighting_vector
            )
        )
      }
    )
  }
  
  
  assert_unique_curve(
    output,
    predictor
  )
}


# ------------------------------------------------------------
# 18. Full versus inconsistent-case-excluded curves
# ------------------------------------------------------------

full_curves <- bind_rows(
  make_flexible_curve(
    full_flexible_model,
    primary_dat,
    "Psychological distress",
    primary_dat$analysis_weight
  ),
  
  make_flexible_curve(
    full_flexible_model,
    primary_dat,
    "Loneliness",
    primary_dat$analysis_weight
  ),
  
  make_flexible_curve(
    full_flexible_model,
    primary_dat,
    "Perceived social support",
    primary_dat$analysis_weight
  )
) %>%
  rename(
    full_probability =
      adjusted_probability
  )


sensitivity_curves <- bind_rows(
  make_flexible_curve(
    sensitivity_flexible_model,
    sensitivity_dat,
    "Psychological distress",
    sensitivity_dat$analysis_weight
  ),
  
  make_flexible_curve(
    sensitivity_flexible_model,
    sensitivity_dat,
    "Loneliness",
    sensitivity_dat$analysis_weight
  ),
  
  make_flexible_curve(
    sensitivity_flexible_model,
    sensitivity_dat,
    "Perceived social support",
    sensitivity_dat$analysis_weight
  )
) %>%
  rename(
    sensitivity_probability =
      adjusted_probability
  )


inconsistency_prediction_comparison <- full_curves %>%
  left_join(
    sensitivity_curves,
    by = c(
      "predictor",
      "predictor_value"
    )
  ) %>%
  mutate(
    full_probability_pct =
      100 *
      full_probability,
    
    sensitivity_probability_pct =
      100 *
      sensitivity_probability,
    
    absolute_difference_percentage_points =
      100 *
      abs(
        sensitivity_probability -
          full_probability
      )
  )


inconsistency_prediction_magnitude <-
  inconsistency_prediction_comparison %>%
  group_by(
    predictor
  ) %>%
  summarise(
    maximum_absolute_difference_percentage_points =
      max(
        absolute_difference_percentage_points,
        na.rm = TRUE
      ),
    
    mean_absolute_difference_percentage_points =
      mean(
        absolute_difference_percentage_points,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


# ============================================================
# PART D
# WEIGHTING SENSITIVITY
# ============================================================


# ------------------------------------------------------------
# 19. Unweighted versions of same primary models
#
# Same predictor coding and spline basis are intentionally used
# so this isolates the impact of survey weighting itself.
# ------------------------------------------------------------

unweighted_linear_model <- svyglm(
  primary_linear_formula,
  design = primary_design_unweighted,
  family = quasibinomial()
)


unweighted_flexible_model <- svyglm(
  primary_flexible_formula,
  design = primary_design_unweighted,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 20. Linear weighted vs unweighted
# ------------------------------------------------------------

weighted_linear_results <- extract_svyglm(
  full_linear_model,
  "Weighted"
) %>%
  filter(
    term %in% c(
      "distress_z",
      "loneliness_z",
      "social_support_z"
    )
  ) %>%
  select(
    term,
    weighted_OR = OR,
    weighted_CI_lower = CI_lower,
    weighted_CI_upper = CI_upper,
    weighted_p = p_value
  )


unweighted_linear_results <- extract_svyglm(
  unweighted_linear_model,
  "Unweighted"
) %>%
  filter(
    term %in% c(
      "distress_z",
      "loneliness_z",
      "social_support_z"
    )
  ) %>%
  select(
    term,
    unweighted_OR = OR,
    unweighted_CI_lower = CI_lower,
    unweighted_CI_upper = CI_upper,
    unweighted_p = p_value
  )


weight_linear_comparison <- weighted_linear_results %>%
  left_join(
    unweighted_linear_results,
    by = "term"
  ) %>%
  mutate(
    OR_absolute_difference =
      abs(
        weighted_OR -
          unweighted_OR
      ),
    
    log_OR_absolute_difference =
      abs(
        log(
          weighted_OR
        ) -
          log(
            unweighted_OR
          )
      )
  )


weight_flexible_tests <- bind_rows(
  get_flexible_tests(
    full_flexible_model,
    "Weighted"
  ),
  
  get_flexible_tests(
    unweighted_flexible_model,
    "Unweighted"
  )
)


# ------------------------------------------------------------
# 21. Weighted vs unweighted prediction curves
# ------------------------------------------------------------

weighted_curves <- full_curves %>%
  rename(
    weighted_probability =
      full_probability
  )


unweighted_curves <- bind_rows(
  make_flexible_curve(
    unweighted_flexible_model,
    primary_dat,
    "Psychological distress",
    rep(
      1,
      nrow(primary_dat)
    )
  ),
  
  make_flexible_curve(
    unweighted_flexible_model,
    primary_dat,
    "Loneliness",
    rep(
      1,
      nrow(primary_dat)
    )
  ),
  
  make_flexible_curve(
    unweighted_flexible_model,
    primary_dat,
    "Perceived social support",
    rep(
      1,
      nrow(primary_dat)
    )
  )
) %>%
  rename(
    unweighted_probability =
      adjusted_probability
  )


weight_prediction_comparison <- weighted_curves %>%
  left_join(
    unweighted_curves,
    by = c(
      "predictor",
      "predictor_value"
    )
  ) %>%
  mutate(
    weighted_probability_pct =
      100 *
      weighted_probability,
    
    unweighted_probability_pct =
      100 *
      unweighted_probability,
    
    absolute_difference_percentage_points =
      100 *
      abs(
        weighted_probability -
          unweighted_probability
      )
  )


weight_prediction_magnitude <-
  weight_prediction_comparison %>%
  group_by(
    predictor
  ) %>%
  summarise(
    maximum_absolute_difference_percentage_points =
      max(
        absolute_difference_percentage_points,
        na.rm = TRUE
      ),
    
    mean_absolute_difference_percentage_points =
      mean(
        absolute_difference_percentage_points,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


# ============================================================
# DOCUMENTATION
# ============================================================


sample_counts <- tibble(
  sample = c(
    "Prepared analysis dataset",
    "Primary analytic sample",
    "Distress-component robustness sample",
    "Logical inconsistencies in primary sample",
    "Sensitivity sample after excluding inconsistencies"
  ),
  
  N = c(
    nrow(
      analysis_dat
    ),
    
    nrow(
      primary_dat
    ),
    
    nrow(
      component_dat
    ),
    
    sum(
      primary_dat$
        ai_logical_inconsistency
    ),
    
    nrow(
      sensitivity_dat
    )
  )
)


weight_status <- tibble(
  item = c(
    "Primary analytic N",
    "Unique weight values",
    "Minimum weight",
    "Maximum weight",
    "Weight comparison currently informative?"
  ),
  
  value = as.character(
    c(
      nrow(
        primary_dat
      ),
      
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
      
      if (
        n_distinct(
          primary_dat$
          analysis_weight
        ) == 1 &&
        unique(
          primary_dat$
          analysis_weight
        ) == 1
      ) {
        paste(
          "No - current weights are all 1;",
          "weighted and unweighted estimates should coincide."
        )
      } else {
        "Yes - non-constant/final weights detected."
      }
    )
  )
)


analysis_specification <- tibble(
  analysis = c(
    "Distress-component robustness",
    "Logical-inconsistency sensitivity",
    "Weighting sensitivity"
  ),
  
  specification = c(
    paste(
      "PHQ, GAD, and PSS standardized and entered",
      "simultaneously; age remains linear and the",
      "preferred spline forms for loneliness and social",
      "support from Analysis 06 are retained."
    ),
    
    paste(
      "Primary linear and flexible models repeated after",
      "excluding participants with Q01=0 and Q02>0."
    ),
    
    paste(
      "Weighted and equal-weight versions of the same",
      "primary model are compared on the same sample,",
      "using identical predictor coding and spline bases."
    )
  )
)


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

output_file <- file.path(
  safe_outputs_dir,
  "SAFE_10_robustness_sensitivity.xlsx"
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
    Analysis_Specification =
      analysis_specification,
    
    Sample_Counts =
      sample_counts,
    
    Prediction_Grid =
      prediction_grid_summary,
    
    Components_Results =
      component_results,
    
    Components_Focal =
      component_focal_results,
    
    Components_Joint_Test =
      component_joint_test,
    
    Components_Correlations =
      component_correlations,
    
    Components_VIF =
      component_vif,
    
    Inconsistency_Linear =
      inconsistency_linear_comparison,
    
    Inconsistency_Flexible =
      inconsistency_flexible_tests,
    
    Inconsistency_Predictions =
      inconsistency_prediction_comparison,
    
    Inconsistency_Magnitude =
      inconsistency_prediction_magnitude,
    
    Weight_Linear =
      weight_linear_comparison,
    
    Weight_Flexible =
      weight_flexible_tests,
    
    Weight_Predictions =
      weight_prediction_comparison,
    
    Weight_Magnitude =
      weight_prediction_magnitude,
    
    Weight_Status =
      weight_status,
    
    Run_Info =
      run_info
  ),
  
  path =
    output_file
)


# ============================================================
# COMPLETION
# ============================================================

message(
  "Robustness and sensitivity analyses complete. SAFE output: ",
  project_relative(output_file)
)
