# ============================================================
# 07_general_AI_users_model.R
# STRONG-1 Thesis
#
# Secondary model among GENERAL AI USERS ONLY
#
# Question:
# Among people who use AI at all, what is associated with
# also using AI for emotional support?
#
# Step 1 = demographics
# Step 2 = + general AI-use frequency
# Step 3 = + psychological distress
# Step 4 = + loneliness + perceived social support
#
# Both pre-specified linear and flexible versions are reported.
# No participant-level data are exported.
# ============================================================

source_files <- Filter(Negate(is.null), lapply(sys.frames(), `[[`, "ofile"))
file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(source_files) > 0) {
  script_file <- normalizePath(tail(source_files, 1)[[1]])
} else if (length(file_argument) > 0) {
  script_file <- normalizePath(sub("^--file=", "", tail(file_argument, 1)))
} else {
  stop("Could not determine the location of 07_general_AI_users_model.R.")
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
# 1. ALWAYS rebuild prepared data
# ------------------------------------------------------------

source(
  file.path(
    scripts_dir,
    "04_prepare_analysis_data.R"
  )
)


# ------------------------------------------------------------
# 2. Define general-AI-user analytic sample
#
# General AI users = MH_AI_Q01 > 0
#
# Same complete-case sample is used for all model steps.
# ------------------------------------------------------------

secondary_variables <- c(
  "ai_support_binary",
  "age",
  "gender_model",
  "education",
  "income",
  "religion_binary",
  "ai_general_raw",
  "distress_z",
  "loneliness_z",
  "social_support_z",
  "analysis_weight"
)


secondary_dat <- analysis_dat %>%
  filter(
    general_ai_user == 1
  ) %>%
  filter(
    complete.cases(
      across(
        all_of(secondary_variables)
      )
    )
  )


cat(
  "\nGeneral-AI-user analytic sample N =",
  nrow(secondary_dat),
  "\n"
)


# ------------------------------------------------------------
# 3. General AI frequency factor
#
# Only categories 1-4 exist in this subset.
# Reference = "A few times"
# ------------------------------------------------------------

secondary_dat <- secondary_dat %>%
  mutate(
    
    general_ai_frequency_model =
      factor(
        ai_general_raw,
        levels = 1:4,
        labels = c(
          "A few times",
          "Several times a month",
          "Several times a week",
          "Almost every day / every day"
        )
      ),
    
    general_ai_frequency_model =
      relevel(
        general_ai_frequency_model,
        ref = "A few times"
      )
  )


# ------------------------------------------------------------
# 4. Outcome summary
# ------------------------------------------------------------

outcome_summary <- secondary_dat %>%
  count(
    ai_support_binary,
    name = "n"
  ) %>%
  mutate(
    pct =
      100 * n / sum(n)
  )


# ------------------------------------------------------------
# 5. General AI frequency distribution
# ------------------------------------------------------------

general_AI_distribution <- secondary_dat %>%
  group_by(
    general_ai_frequency_model
  ) %>%
  summarise(
    
    n = n(),
    
    weighted_n =
      sum(
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
# 6. Flexible spline bases
#
# Same conservative functional forms as in primary analysis:
# distress = 3 df
# loneliness = 2 df
# social support = 3 df
#
# Bases are learned within this analytic subset.
# ------------------------------------------------------------

distress_ns_object <- splines::ns(
  secondary_dat$distress_z,
  df = 3
)

loneliness_ns_object <- splines::ns(
  secondary_dat$loneliness_z,
  df = 2
)

support_ns_object <- splines::ns(
  secondary_dat$social_support_z,
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


secondary_dat <- secondary_dat %>%
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
# 7. Survey design
# ------------------------------------------------------------

secondary_design <- svydesign(
  ids = ~1,
  weights = ~analysis_weight,
  data = secondary_dat
)


# ============================================================
# PART A
# LINEAR HIERARCHICAL MODEL
# ============================================================


# ------------------------------------------------------------
# 8. Model formulas
# ------------------------------------------------------------

formula_step1 <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary


formula_step2 <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  general_ai_frequency_model


formula_step3_linear <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  general_ai_frequency_model +
  distress_z


formula_step4_linear <-
  ai_support_binary ~
  age +
  gender_model +
  education +
  income +
  religion_binary +
  general_ai_frequency_model +
  distress_z +
  loneliness_z +
  social_support_z


# ------------------------------------------------------------
# 9. Fit linear models
# ------------------------------------------------------------

model_step1 <- svyglm(
  formula_step1,
  design = secondary_design,
  family = quasibinomial()
)


model_step2 <- svyglm(
  formula_step2,
  design = secondary_design,
  family = quasibinomial()
)


model_step3_linear <- svyglm(
  formula_step3_linear,
  design = secondary_design,
  family = quasibinomial()
)


model_step4_linear <- svyglm(
  formula_step4_linear,
  design = secondary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 10. Extract OR / CI / p
# ------------------------------------------------------------

p_display_function <- function(p) {
  
  format.pval(
    p,
    digits = 3,
    eps = .001
  )
}


extract_model <- function(
    model,
    model_name
) {
  
  broom::tidy(
    model,
    conf.int = TRUE
  ) %>%
    mutate(
      
      OR =
        exp(estimate),
      
      CI_lower =
        exp(conf.low),
      
      CI_upper =
        exp(conf.high),
      
      p_display =
        p_display_function(
          p.value
        ),
      
      model =
        model_name
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
    model_step2,
    "Step 2: + General AI frequency"
  ),
  
  extract_model(
    model_step3_linear,
    "Step 3: + Distress"
  ),
  
  extract_model(
    model_step4_linear,
    "Step 4: + Loneliness + Social Support"
  )
)


# ------------------------------------------------------------
# 11. Focal linear results
# ------------------------------------------------------------

linear_focal_results <- linear_model_results %>%
  filter(
    
    grepl(
      "^general_ai_frequency_model",
      term
    ) |
      
      term %in% c(
        "distress_z",
        "loneliness_z",
        "social_support_z"
      )
  )


# ------------------------------------------------------------
# 12. Hierarchical block tests
# ------------------------------------------------------------

test_general_AI <- regTermTest(
  model_step2,
  ~ general_ai_frequency_model,
  method = "Wald"
)


test_distress_linear <- regTermTest(
  model_step3_linear,
  ~ distress_z,
  method = "Wald"
)


test_interpersonal_linear <- regTermTest(
  model_step4_linear,
  ~ loneliness_z + social_support_z,
  method = "Wald"
)


linear_block_tests <- tibble(
  
  test = c(
    "Step 2: General AI-use frequency",
    "Step 3: Psychological distress",
    "Step 4: Loneliness + social support jointly"
  ),
  
  output = c(
    
    paste(
      capture.output(
        print(test_general_AI)
      ),
      collapse = " "
    ),
    
    paste(
      capture.output(
        print(test_distress_linear)
      ),
      collapse = " "
    ),
    
    paste(
      capture.output(
        print(test_interpersonal_linear)
      ),
      collapse = " "
    )
  )
)


# ============================================================
# PART B
# FLEXIBLE MODEL
# ============================================================


# ------------------------------------------------------------
# 13. Flexible Step 3
# ------------------------------------------------------------

model_step3_flexible <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    general_ai_frequency_model +
    distress_ns1 +
    distress_ns2 +
    distress_ns3,
  
  design = secondary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 14. Flexible Step 4
# ------------------------------------------------------------

model_step4_flexible <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    general_ai_frequency_model +
    distress_ns1 +
    distress_ns2 +
    distress_ns3 +
    loneliness_ns1 +
    loneliness_ns2 +
    support_ns1 +
    support_ns2 +
    support_ns3,
  
  design = secondary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 15. Extract global test p-values
# ------------------------------------------------------------

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
    !is.null(
      test_object$p.value
    )
  ) {
    
    p_value <-
      as.numeric(
        test_object$p.value[1]
      )
  }
  
  
  tibble(
    
    test = label,
    
    p_value =
      p_value,
    
    p_display =
      ifelse(
        is.na(p_value),
        NA_character_,
        p_display_function(
          p_value
        )
      ),
    
    output =
      paste(
        capture.output(
          print(
            test_object
          )
        ),
        collapse = " "
      )
  )
}


# ------------------------------------------------------------
# 16. Flexible global hypothesis tests
# ------------------------------------------------------------

flex_test_general_AI <- regTermTest(
  
  model_step4_flexible,
  
  ~ general_ai_frequency_model,
  
  method = "Wald"
)


flex_test_distress <- regTermTest(
  
  model_step4_flexible,
  
  ~ distress_ns1 +
    distress_ns2 +
    distress_ns3,
  
  method = "Wald"
)


flex_test_loneliness <- regTermTest(
  
  model_step4_flexible,
  
  ~ loneliness_ns1 +
    loneliness_ns2,
  
  method = "Wald"
)


flex_test_support <- regTermTest(
  
  model_step4_flexible,
  
  ~ support_ns1 +
    support_ns2 +
    support_ns3,
  
  method = "Wald"
)


flex_test_interpersonal <- regTermTest(
  
  model_step4_flexible,
  
  ~ loneliness_ns1 +
    loneliness_ns2 +
    support_ns1 +
    support_ns2 +
    support_ns3,
  
  method = "Wald"
)


flexible_global_tests <- bind_rows(
  
  extract_regterm(
    flex_test_general_AI,
    "General AI-use frequency"
  ),
  
  extract_regterm(
    flex_test_distress,
    "Psychological distress"
  ),
  
  extract_regterm(
    flex_test_loneliness,
    "Loneliness"
  ),
  
  extract_regterm(
    flex_test_support,
    "Perceived social support"
  ),
  
  extract_regterm(
    flex_test_interpersonal,
    "Loneliness + social support jointly"
  )
)


# ============================================================
# FUNCTIONAL-FORM CHECKS WITHIN GENERAL AI USERS
# ============================================================


# ------------------------------------------------------------
# 17. Alternative models
# ------------------------------------------------------------

model_distress_linear_conditional <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    general_ai_frequency_model +
    distress_z +
    loneliness_ns1 +
    loneliness_ns2 +
    support_ns1 +
    support_ns2 +
    support_ns3,
  
  design = secondary_design,
  family = quasibinomial()
)


model_loneliness_linear_conditional <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    general_ai_frequency_model +
    distress_ns1 +
    distress_ns2 +
    distress_ns3 +
    loneliness_z +
    support_ns1 +
    support_ns2 +
    support_ns3,
  
  design = secondary_design,
  family = quasibinomial()
)


model_support_linear_conditional <- svyglm(
  
  ai_support_binary ~
    age +
    gender_model +
    education +
    income +
    religion_binary +
    general_ai_frequency_model +
    distress_ns1 +
    distress_ns2 +
    distress_ns3 +
    loneliness_ns1 +
    loneliness_ns2 +
    social_support_z,
  
  design = secondary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 18. Age spline check
# ------------------------------------------------------------

age_ns_object <- splines::ns(
  secondary_dat$age,
  df = 3
)

age_ns_matrix <- as.matrix(
  age_ns_object
)


secondary_dat <- secondary_dat %>%
  mutate(
    
    age_ns1 =
      age_ns_matrix[, 1],
    
    age_ns2 =
      age_ns_matrix[, 2],
    
    age_ns3 =
      age_ns_matrix[, 3]
  )


secondary_design_age <- svydesign(
  ids = ~1,
  weights = ~analysis_weight,
  data = secondary_dat
)


model_age_flexible <- svyglm(
  
  ai_support_binary ~
    age_ns1 +
    age_ns2 +
    age_ns3 +
    gender_model +
    education +
    income +
    religion_binary +
    general_ai_frequency_model +
    distress_ns1 +
    distress_ns2 +
    distress_ns3 +
    loneliness_ns1 +
    loneliness_ns2 +
    support_ns1 +
    support_ns2 +
    support_ns3,
  
  design = secondary_design_age,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 19. Functional-form comparison helper
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
        predictor =
          predictor_name,
        
        flexible_form =
          flexible_form,
        
        p_value =
          NA_real_,
        
        p_display =
          NA_character_,
        
        diagnostic =
          "Comparison failed",
        
        test_output =
          conditionMessage(
            comparison
          )
      )
    )
  }
  
  
  p_val <-
    get_anova_p(
      comparison
    )
  
  
  tibble(
    
    predictor =
      predictor_name,
    
    flexible_form =
      flexible_form,
    
    p_value =
      p_val,
    
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
          print(
            comparison
          )
        ),
        collapse = " "
      )
  )
}


functional_form_tests <- bind_rows(
  
  compare_functional_form(
    model_step4_flexible,
    model_age_flexible,
    "Age",
    "Natural cubic spline, 3 df"
  ),
  
  compare_functional_form(
    model_distress_linear_conditional,
    model_step4_flexible,
    "Psychological distress",
    "Natural cubic spline, 3 df"
  ),
  
  compare_functional_form(
    model_loneliness_linear_conditional,
    model_step4_flexible,
    "Loneliness",
    "Natural cubic spline, 2 df"
  ),
  
  compare_functional_form(
    model_support_linear_conditional,
    model_step4_flexible,
    "Perceived social support",
    "Natural cubic spline, 3 df"
  )
)


# ============================================================
# MULTICOLLINEARITY
# ============================================================


# ------------------------------------------------------------
# 20. VIF for final linear model
# ------------------------------------------------------------

vif_model <- glm(
  formula_step4_linear,
  data = secondary_dat,
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
      c(
        "GVIF",
        "Df"
      ) %in%
      names(
        vif_results
      )
    )
  ) {
    
    vif_results <- vif_results %>%
      mutate(
        
        adjusted_GVIF =
          GVIF^(
            1 /
              (
                2 *
                  Df
              )
          )
      )
  }
  
} else {
  
  vif_results <- tibble(
    
    predictor =
      names(
        vif_raw
      ),
    
    VIF =
      as.numeric(
        vif_raw
      )
  )
}


# ============================================================
# DIAGNOSTICS
# ============================================================


# ------------------------------------------------------------
# 21. Model diagnostics
# ------------------------------------------------------------

linear_coefs <- coef(
  model_step4_linear
)

flex_coefs <- coef(
  model_step4_flexible
)


linear_ses <- sqrt(
  diag(
    vcov(
      model_step4_linear
    )
  )
)

flex_ses <- sqrt(
  diag(
    vcov(
      model_step4_flexible
    )
  )
)


diagnostics <- tibble(
  
  metric = c(
    "General-AI-user analytic N",
    "AI emotional-support users",
    "Non-emotional-support AI users",
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
      
      nrow(
        secondary_dat
      ),
      
      sum(
        secondary_dat$
          ai_support_binary == 1
      ),
      
      sum(
        secondary_dat$
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
        secondary_dat$
          analysis_weight
      )
    )
  )
)


# ------------------------------------------------------------
# 22. Predicted probability ranges
# ------------------------------------------------------------

linear_predictions <- predict(
  model_step4_linear,
  type = "response"
)


flex_predictions <- predict(
  model_step4_flexible,
  type = "response"
)


prediction_range <- tibble(
  
  model = c(
    "Linear secondary model",
    "Flexible secondary model"
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
# DOCUMENTATION
# ============================================================


# ------------------------------------------------------------
# 23. Model specification
# ------------------------------------------------------------

model_specification <- tibble(
  
  item = c(
    "Analytic population",
    "Outcome",
    "Step 1",
    "Step 2",
    "Step 3",
    "Step 4",
    "General AI frequency treatment",
    "Flexible distress",
    "Flexible loneliness",
    "Flexible social support",
    "Gender reference",
    "Education reference",
    "Income reference",
    "Religion reference"
  ),
  
  specification = c(
    
    "Participants reporting any general AI use",
    
    "Any AI emotional-support use: 0=no, 1=yes",
    
    paste(
      deparse(
        formula_step1
      ),
      collapse = " "
    ),
    
    paste(
      deparse(
        formula_step2
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
      deparse(
        formula_step4_linear
      ),
      collapse = " "
    ),
    
    paste(
      "Categorical factor;",
      "reference = A few times"
    ),
    
    "Natural cubic spline, 3 df",
    
    "Natural cubic spline, 2 df",
    
    "Natural cubic spline, 3 df",
    
    "Female",
    
    "Secondary education",
    
    "About average income",
    
    "Jewish"
  )
)


# ------------------------------------------------------------
# 24. Spline documentation
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
# 25. Weight status
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
        secondary_dat
      ),
      
      n_distinct(
        secondary_dat$
          analysis_weight
      ),
      
      min(
        secondary_dat$
          analysis_weight
      ),
      
      max(
        secondary_dat$
          analysis_weight
      ),
      
      ifelse(
        
        n_distinct(
          secondary_dat$
            analysis_weight
        ) == 1 &&
          
          unique(
            secondary_dat$
              analysis_weight
          ) == 1,
        
        "PLACEHOLDER: all weights = 1",
        
        "Non-constant/final weights detected"
      )
    )
  )
)


# ------------------------------------------------------------
# 26. Reproducibility
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
# 27. Export SAFE results
# ------------------------------------------------------------

output_file <- file.path(
  safe_outputs_dir,
  "SAFE_07_general_AI_users_model.xlsx"
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
    
    General_AI_Distribution =
      general_AI_distribution,
    
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
    
    VIF =
      vif_results,
    
    Diagnostics =
      diagnostics,
    
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
  "General-AI-user model complete. SAFE output: ",
  project_relative(output_file)
)
