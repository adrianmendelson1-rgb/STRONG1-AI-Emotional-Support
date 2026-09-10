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
#    Refit the flexible primary model after excluding
#    participants reporting Q01 = 0 but Q02 > 0.
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
  "social_support_z"
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


standardize <- function(x) {

  (x - mean(x, na.rm = TRUE)) /
    sd(x, na.rm = TRUE)
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
    newdata
) {

  predictions <- predict(
    model,
    newdata = newdata,
    type = "response"
  )

  mean_na(
    as.numeric(predictions)
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
      standardize(phq),
    
    gad_component_z =
      standardize(gad),
    
    pss_component_z =
      standardize(pss)
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
  "social_support_z"
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
# 6. Survey design
#
# ids = ~1 with equal probabilities: an unweighted analysis with
# design-based (robust) standard errors.
# ------------------------------------------------------------

primary_design <- svydesign(
  ids = ~1,
  probs = ~1,
  data = primary_dat
)


# ------------------------------------------------------------
# 7. Primary model formula
#
# Functional form was settled in Analysis 06; this script reuses
# the flexible specification rather than re-testing it.
# ------------------------------------------------------------

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
  probs = ~1,
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

pairwise_cor <- function(x, y) {

  ok <-
    !is.na(x) &
    !is.na(y)

  cor(
    x[ok],
    y[ok]
  )
}


component_correlations <- tibble(
  pair = c(
    "PHQ-GAD",
    "PHQ-PSS",
    "GAD-PSS"
  ),
  
  correlation = c(
    pairwise_cor(
      component_dat$phq,
      component_dat$gad
    ),
    
    pairwise_cor(
      component_dat$phq,
      component_dat$pss
    ),
    
    pairwise_cor(
      component_dat$gad,
      component_dat$pss
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


# Collinearity is diagnosed on the linear representation of every
# predictor: a VIF computed on individual spline basis columns has
# no interpretation.
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
# 12. Full-sample flexible model
# ------------------------------------------------------------

full_flexible_model <- svyglm(
  primary_flexible_formula,
  design = primary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 13. Refit after excluding inconsistent cases
# ------------------------------------------------------------

sensitivity_dat <- primary_dat %>%
  filter(
    !ai_logical_inconsistency
  )


sensitivity_design <- svydesign(
  ids = ~1,
  probs = ~1,
  data = sensitivity_dat
)


sensitivity_flexible_model <- svyglm(
  primary_flexible_formula,
  design = sensitivity_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 14. Flexible global-test helper
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
# 15. UNIQUE common prediction grids
#
# unique() is essential because scale ceilings can make
# adjacent percentiles identical.
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
  sample_quantile(
    primary_dat$distress_z,
    prediction_probs
  )
)


support_grid <- unique_grid(
  sample_quantile(
    primary_dat$social_support_z,
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
# 16. Curve generator
# ------------------------------------------------------------

make_flexible_curve <- function(
    model,
    base_dat,
    predictor
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
              newdata
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
              newdata
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
              newdata
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
# 17. Full versus inconsistent-case-excluded curves
# ------------------------------------------------------------

full_curves <- bind_rows(
  make_flexible_curve(
    full_flexible_model,
    primary_dat,
    "Psychological distress"
  ),
  
  make_flexible_curve(
    full_flexible_model,
    primary_dat,
    "Loneliness"
  ),
  
  make_flexible_curve(
    full_flexible_model,
    primary_dat,
    "Perceived social support"
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
    "Psychological distress"
  ),
  
  make_flexible_curve(
    sensitivity_flexible_model,
    sensitivity_dat,
    "Loneliness"
  ),
  
  make_flexible_curve(
    sensitivity_flexible_model,
    sensitivity_dat,
    "Perceived social support"
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


analysis_specification <- tibble(
  analysis = c(
    "Distress-component robustness",
    "Logical-inconsistency sensitivity"
  ),
  
  specification = c(
    paste(
      "PHQ, GAD, and PSS standardized and entered",
      "simultaneously; age remains linear and the",
      "preferred spline forms for loneliness and social",
      "support from Analysis 06 are retained."
    ),
    
    paste(
      "The flexible primary model repeated after excluding",
      "participants with Q01=0 and Q02>0."
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
  analysis_outputs_dir,
  "Analysis_10_robustness_sensitivity.xlsx"
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
    
    Inconsistency_Flexible =
      inconsistency_flexible_tests,
    
    Inconsistency_Predictions =
      inconsistency_prediction_comparison,
    
    Inconsistency_Magnitude =
      inconsistency_prediction_magnitude,
    
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
  "Robustness and sensitivity analyses complete. Output: ",
  project_relative(output_file)
)
