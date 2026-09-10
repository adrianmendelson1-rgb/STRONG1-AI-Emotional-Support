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
# The model adjusts for demographics and, additionally, for
# general AI-use frequency, so the focal predictors are assessed
# net of how much a participant uses AI at all.
#
# Focal continuous predictors follow the same functional-form
# strategy as the primary analysis: assessed for linearity, then
# modelled as natural cubic splines. General AI-use frequency is
# categorical and keeps interpretable category contrasts.
#
# This is a restricted subset of the same cohort, not a separate
# sample.
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
# splines::ns() uses to place its interior knots.
# ------------------------------------------------------------

mean_na <- function(x) mean(x, na.rm = TRUE)

sd_na <- function(x) sd(x, na.rm = TRUE)

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
  "social_support_z"
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
    
    .groups = "drop"
  ) %>%
  mutate(
    
    pct =
      100 *
      n /
      sum(n)
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
  probs = ~1,
  data = secondary_dat
)


# ============================================================
# PART A
# LINEAR SPECIFICATION AND FUNCTIONAL-FORM ASSESSMENT
# ============================================================


# ------------------------------------------------------------
# 8. Linear model formula
# ------------------------------------------------------------

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
# 9. Fit the linear specification
#
# This is the specification whose adequacy is tested below. It is
# retained for the collinearity diagnostic and for the predicted
# probability range, not reported as a competing model.
# ------------------------------------------------------------

model_step4_linear <- svyglm(
  formula_step4_linear,
  design = secondary_design,
  family = quasibinomial()
)


# ------------------------------------------------------------
# 10. Coefficient extraction helper
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


# ============================================================
# PART B
# FLEXIBLE MODEL
# ============================================================



# ------------------------------------------------------------
# 11. Flexible final model
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
# 12. Extract global test p-values
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
# 13. Flexible global hypothesis tests
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
# 14. Alternative models
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
# 15. Age spline check
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
  probs = ~1,
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
# 16. Functional-form comparison helper
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



# ------------------------------------------------------------
# Category-specific ORs for general AI-use frequency
#
# General AI-use frequency is categorical, so its category
# contrasts stay directly interpretable and are taken from the
# flexible final model. The three focal predictors are summarised
# by global Wald tests instead, because their spline coefficients
# carry no direct interpretation.
# ------------------------------------------------------------

flexible_general_AI_ORs <- extract_model(
  model_step4_flexible,
  "Flexible final model"
) %>%
  filter(
    grepl(
      "^general_ai_frequency_model",
      term
    )
  ) %>%
  mutate(
    category =
      sub(
        "^general_ai_frequency_model",
        "",
        term
      ),
    reference = "A few times"
  ) %>%
  select(
    category,
    reference,
    OR,
    CI_lower,
    CI_upper,
    p_value,
    p_display
  )


# ------------------------------------------------------------
# Adjusted marginal probabilities for the focal predictors
#
# Same delta-method construction as the primary analysis:
# predicted risk averaged over the observed covariate
# distribution with one focal predictor fixed for everyone.
# ------------------------------------------------------------

marginal_probability_ci <- function(model, newdata) {

  beta <- coef(model)

  V <- vcov(model)

  X <- model.matrix(
    delete.response(
      terms(model)
    ),
    data = newdata
  )

  X <- X[, names(beta), drop = FALSE]

  p <- plogis(
    as.numeric(X %*% beta)
  )

  w <- rep(1 / nrow(X), nrow(X))

  estimate <- sum(w * p)

  gradient <- colSums(
    X * (w * p * (1 - p))
  )

  se <- sqrt(
    max(
      as.numeric(t(gradient) %*% V %*% gradient),
      0
    )
  )

  c(
    estimate = estimate,
    SE = se,
    lower = max(0, estimate - 1.96 * se),
    upper = min(1, estimate + 1.96 * se)
  )
}


get_spline_basis <- function(spline_object, value) {

  as.numeric(
    predict(
      spline_object,
      newx = value
    )
  )
}


prediction_probs <- c(.05, .10, .25, .50, .75, .90, .95)

# A bounded measure whose tail piles up at its ceiling can return the
# same value for adjacent percentiles; keep each grid point once.
distress_grid <- unique(
  sample_quantile(
    secondary_dat$distress_z,
    prediction_probs
  )
)

support_grid <- unique(
  sample_quantile(
    secondary_dat$social_support_z,
    prediction_probs
  )
)

# UCLA-3 has seven observed integer scores; predict at each one and
# carry the corresponding standardized value.
loneliness_raw_grid <- 3:9

loneliness_z_grid <-
  (
    loneliness_raw_grid -
      mean_na(analysis_dat$loneliness)
  ) /
  sd_na(analysis_dat$loneliness)


predict_over_grid <- function(
    label,
    values,
    z_values,
    spline_object,
    column_names
) {

  map2_dfr(
    values,
    z_values,
    function(value, z_value) {

      basis <- get_spline_basis(
        spline_object,
        z_value
      )

      nd <- secondary_dat

      for (i in seq_along(column_names)) {
        nd[[column_names[i]]] <- basis[i]
      }

      result <- marginal_probability_ci(
        model_step4_flexible,
        nd
      )

      tibble(
        predictor = label,
        predictor_value = value,
        predictor_value_z = z_value,
        adjusted_probability = result[["estimate"]],
        SE = result[["SE"]],
        lower_95_CI = result[["lower"]],
        upper_95_CI = result[["upper"]]
      )
    }
  )
}


adjusted_predictions <- bind_rows(

  predict_over_grid(
    "Psychological distress",
    distress_grid,
    distress_grid,
    distress_ns_object,
    c("distress_ns1", "distress_ns2", "distress_ns3")
  ),

  predict_over_grid(
    "Loneliness",
    loneliness_raw_grid,
    loneliness_z_grid,
    loneliness_ns_object,
    c("loneliness_ns1", "loneliness_ns2")
  ),

  predict_over_grid(
    "Perceived social support",
    support_grid,
    support_grid,
    support_ns_object,
    c("support_ns1", "support_ns2", "support_ns3")
  )
) %>%
  mutate(
    adjusted_probability_pct = 100 * adjusted_probability,
    lower_95_CI_pct = 100 * lower_95_CI,
    upper_95_CI_pct = 100 * upper_95_CI
  )


# ============================================================
# MULTICOLLINEARITY
# ============================================================


# ------------------------------------------------------------
# 17. VIF for final linear model
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
# 18. Model diagnostics
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
    "Maximum SE - flexible"
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
      )
    )
  )
)


# ------------------------------------------------------------
# 19. Predicted probability ranges
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
# 20. Model specification
# ------------------------------------------------------------

model_specification <- tibble(
  
  item = c(
    "Analytic population",
    "Outcome",
    "Linear specification",
    "Inferential model",
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
        formula_step4_linear
      ),
      collapse = " "
    ),
    
    paste(
      "Same covariates as the linear specification;",
      "distress and social support use 3-df natural cubic splines;",
      "loneliness uses a 2-df natural cubic spline."
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
# 21. Spline documentation
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
# 22. Reproducibility
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
# 23. Export aggregate results
# ------------------------------------------------------------

output_file <- file.path(
  analysis_outputs_dir,
  "Analysis_07_general_AI_users_model.xlsx"
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
    
    Flexible_General_AI_ORs =
      flexible_general_AI_ORs,
    
    Adjusted_Predictions =
      adjusted_predictions,
    
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
  "General-AI-user model complete. Output: ",
  project_relative(output_file)
)
