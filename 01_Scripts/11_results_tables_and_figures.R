# ============================================================
# 11_results_tables_and_figures.R
# STRONG-1 Thesis
#
# FINAL PUBLICATION OUTPUTS
#
# MAIN MANUSCRIPT
# ----------------
# Table 1:
#   Sample characteristics by AI emotional-support use
#
# Table 2:
#   Key associations with AI emotional-support use
#
# Figure 1:
#   Adjusted probability curves
#
# Figure 2:
#   AI emotional-support experiences and psychotherapy context
#
# SUPPLEMENT
# ----------
# S1 = Detailed AI experience descriptives
# S2 = Frequency-threshold analysis
# S3 = Robustness / sensitivity summary
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
  stop("Could not determine the location of 11_results_tables_and_figures.R.")
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
  "tibble",
  "readxl",
  "writexl",
  "ggplot2",
  "patchwork"
)

ensure_packages(packages)

library(dplyr)
library(purrr)
library(survey)
library(tibble)
library(readxl)
library(writexl)
library(ggplot2)
library(patchwork)


# ------------------------------------------------------------
# 1. Re-run primary model
#
# This rebuilds analysis_dat and the final flexible primary
# model so Figure 1 can be generated directly from the model
# on a dense prediction grid.
# ------------------------------------------------------------

source(
  file.path(
    scripts_dir,
    "06_primary_models.R"
  )
)

# Every workbook read below is a SAFE aggregate output produced
# by Scripts 06-10. Define this only after Script 06 finishes so
# its source-data import continues to use readxl::read_excel.
read_safe_excel <- function(path, ...) {
  readxl::read_excel(
    file.path(safe_outputs_dir, path),
    ...
  )
}


# ------------------------------------------------------------
# 2. Required SAFE files
# ------------------------------------------------------------

required_files <- file.path(
  safe_outputs_dir,
  c(
    "SAFE_06_primary_models.xlsx",
    "SAFE_07_general_AI_users_model.xlsx",
    "SAFE_08_ordinal_frequency_model.xlsx",
    "SAFE_09_AI_experience_descriptives.xlsx",
    "SAFE_10_robustness_sensitivity.xlsx"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  
  stop(
    paste(
      "Missing SAFE result file(s):",
      paste(
        project_relative(missing_files),
        collapse = ", "
      )
    )
  )
}


# ============================================================
# HELPERS
# ============================================================


weighted_mean_safe_11 <- function(
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


weighted_sd_safe_11 <- function(
    x,
    w
) {
  
  ok <-
    !is.na(x) &
    !is.na(w)
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  x <- x[ok]
  w <- w[ok]
  
  mean_x <-
    weighted_mean_safe_11(
      x,
      w
    )
  
  sqrt(
    sum(
      w *
        (
          x -
            mean_x
        )^2
    ) /
      sum(w)
  )
}


weighted_prop_safe_11 <- function(
    indicator,
    w
) {
  
  ok <-
    !is.na(indicator) &
    !is.na(w)
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(
    indicator[ok] *
      w[ok]
  ) /
    sum(
      w[ok]
    )
}


weighted_quantile_safe_11 <- function(
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
      
      x[
        which(
          cumulative_weight >= p
        )[1]
      ]
    }
  )
}


format_mean_sd <- function(
    mean_value,
    sd_value,
    digits = 2
) {
  
  paste0(
    formatC(
      mean_value,
      digits = digits,
      format = "f"
    ),
    " (",
    formatC(
      sd_value,
      digits = digits,
      format = "f"
    ),
    ")"
  )
}


format_n_pct <- function(
    n,
    pct,
    digits = 1
) {
  
  paste0(
    n,
    " (",
    formatC(
      pct,
      digits = digits,
      format = "f"
    ),
    "%)"
  )
}


format_or_ci <- function(
    OR,
    lower,
    upper
) {
  
  paste0(
    formatC(
      OR,
      digits = 2,
      format = "f"
    ),
    " [",
    formatC(
      lower,
      digits = 2,
      format = "f"
    ),
    ", ",
    formatC(
      upper,
      digits = 2,
      format = "f"
    ),
    "]"
  )
}


format_p <- function(p) {
  
  if (is.na(p)) {
    return(NA_character_)
  }
  
  format.pval(
    p,
    digits = 3,
    eps = .001
  )
}


# ------------------------------------------------------------
# Delta-method marginal probability and 95% CI
# ------------------------------------------------------------

marginal_probability_ci_11 <- function(
    model,
    newdata,
    weights
) {
  
  beta <- coef(
    model
  )
  
  variance_matrix <- vcov(
    model
  )
  
  
  X <- model.matrix(
    delete.response(
      terms(
        model
      )
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
    (
      1 -
        probability
    )
  
  
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


get_spline_basis_11 <- function(
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
# TABLE 1
# SAMPLE CHARACTERISTICS
# ============================================================


# ------------------------------------------------------------
# 3. Table 1 data
# ------------------------------------------------------------

table1_dat <- analysis_dat %>%
  filter(
    !is.na(
      ai_support_binary
    ),
    !is.na(
      analysis_weight
    )
  )


overall_dat <-
  table1_dat

nonuser_dat <- table1_dat %>%
  filter(
    ai_support_binary == 0
  )

user_dat <- table1_dat %>%
  filter(
    ai_support_binary == 1
  )


N_total <-
  nrow(
    overall_dat
  )

N_nonusers <-
  nrow(
    nonuser_dat
  )

N_users <-
  nrow(
    user_dat
  )


# ------------------------------------------------------------
# 4. Continuous rows
# ------------------------------------------------------------

continuous_row <- function(
    variable,
    label,
    digits = 2
) {
  
  tibble(
    Characteristic =
      label,
    
    Overall =
      format_mean_sd(
        weighted_mean_safe_11(
          overall_dat[[variable]],
          overall_dat$analysis_weight
        ),
        weighted_sd_safe_11(
          overall_dat[[variable]],
          overall_dat$analysis_weight
        ),
        digits
      ),
    
    No_AI_emotional_support =
      format_mean_sd(
        weighted_mean_safe_11(
          nonuser_dat[[variable]],
          nonuser_dat$analysis_weight
        ),
        weighted_sd_safe_11(
          nonuser_dat[[variable]],
          nonuser_dat$analysis_weight
        ),
        digits
      ),
    
    AI_emotional_support =
      format_mean_sd(
        weighted_mean_safe_11(
          user_dat[[variable]],
          user_dat$analysis_weight
        ),
        weighted_sd_safe_11(
          user_dat[[variable]],
          user_dat$analysis_weight
        ),
        digits
      )
  )
}


# ------------------------------------------------------------
# 5. Categorical rows
# ------------------------------------------------------------

categorical_rows <- function(
    variable,
    variable_label
) {
  
  x <-
    table1_dat[[variable]]
  
  
  if (is.factor(x)) {
    
    levels_present <-
      levels(x)
    
  } else {
    
    levels_present <-
      sort(
        unique(
          x[
            !is.na(x)
          ]
        )
      )
  }
  
  
  map_dfr(
    
    levels_present,
    
    function(level_value) {
      
      overall_indicator <-
        as.integer(
          overall_dat[[variable]] ==
            level_value
        )
      
      nonuser_indicator <-
        as.integer(
          nonuser_dat[[variable]] ==
            level_value
        )
      
      user_indicator <-
        as.integer(
          user_dat[[variable]] ==
            level_value
        )
      
      
      tibble(
        Characteristic =
          paste0(
            variable_label,
            ": ",
            level_value
          ),
        
        Overall =
          format_n_pct(
            sum(
              overall_indicator,
              na.rm = TRUE
            ),
            100 *
              weighted_prop_safe_11(
                overall_indicator,
                overall_dat$analysis_weight
              )
          ),
        
        No_AI_emotional_support =
          format_n_pct(
            sum(
              nonuser_indicator,
              na.rm = TRUE
            ),
            100 *
              weighted_prop_safe_11(
                nonuser_indicator,
                nonuser_dat$analysis_weight
              )
          ),
        
        AI_emotional_support =
          format_n_pct(
            sum(
              user_indicator,
              na.rm = TRUE
            ),
            100 *
              weighted_prop_safe_11(
                user_indicator,
                user_dat$analysis_weight
              )
          )
      )
    }
  )
}


# ------------------------------------------------------------
# 6. Binary row
# ------------------------------------------------------------

binary_row <- function(
    variable,
    value,
    label
) {
  
  overall_indicator <-
    as.integer(
      overall_dat[[variable]] ==
        value
    )
  
  nonuser_indicator <-
    as.integer(
      nonuser_dat[[variable]] ==
        value
    )
  
  user_indicator <-
    as.integer(
      user_dat[[variable]] ==
        value
    )
  
  
  tibble(
    Characteristic =
      label,
    
    Overall =
      format_n_pct(
        sum(
          overall_indicator,
          na.rm = TRUE
        ),
        100 *
          weighted_prop_safe_11(
            overall_indicator,
            overall_dat$analysis_weight
          )
      ),
    
    No_AI_emotional_support =
      format_n_pct(
        sum(
          nonuser_indicator,
          na.rm = TRUE
        ),
        100 *
          weighted_prop_safe_11(
            nonuser_indicator,
            nonuser_dat$analysis_weight
          )
      ),
    
    AI_emotional_support =
      format_n_pct(
        sum(
          user_indicator,
          na.rm = TRUE
        ),
        100 *
          weighted_prop_safe_11(
            user_indicator,
            user_dat$analysis_weight
          )
      )
  )
}


# ------------------------------------------------------------
# 7. Construct Table 1
#
# IMPORTANT:
# religion_full is used descriptively.
# Jewish/non-Jewish is used only in regression models.
# ------------------------------------------------------------

table1 <- bind_rows(
  
  tibble(
    Characteristic =
      "N",
    
    Overall =
      as.character(
        N_total
      ),
    
    No_AI_emotional_support =
      as.character(
        N_nonusers
      ),
    
    AI_emotional_support =
      as.character(
        N_users
      )
  ),
  
  continuous_row(
    "age",
    "Age, years",
    digits = 1
  ),
  
  categorical_rows(
    "gender_full",
    "Gender"
  ),
  
  categorical_rows(
    "education",
    "Education"
  ),
  
  categorical_rows(
    "income",
    "Income"
  ),
  
  categorical_rows(
    "religion_full",
    "Religious affiliation"
  ),
  
  continuous_row(
    "distress_z",
    "Psychological distress, z-score",
    digits = 2
  ),
  
  continuous_row(
    "loneliness",
    "Loneliness (UCLA-3)",
    digits = 2
  ),
  
  continuous_row(
    "social_support",
    "Perceived social support",
    digits = 2
  ),
  
  binary_row(
    "general_ai_user",
    1,
    "Any general AI use"
  )
)


# ============================================================
# TABLE 2
# KEY MODEL RESULTS
# ============================================================


# ------------------------------------------------------------
# 7b. Analytic sample sizes for Table 2
#
# Derived from the fitted primary model and from the SAFE
# outputs, so the Table 2 labels can never drift away from the
# analysis that actually ran.
# ------------------------------------------------------------

read_safe_analysis_n <- function(file) {

  read_safe_excel(
    file,
    sheet = "Weight_Status"
  ) %>%
    filter(item == "Current analysis N") %>%
    pull(value) %>%
    as.numeric()
}


primary_n <- nrow(primary_dat)

primary_n_reported <- read_safe_analysis_n(
  "SAFE_06_primary_models.xlsx"
)

general_n <- read_safe_analysis_n(
  "SAFE_07_general_AI_users_model.xlsx"
)

if (
  !is.finite(primary_n) ||
  !is.finite(primary_n_reported) ||
  !is.finite(general_n) ||
  primary_n < 1 ||
  general_n < 1
) {
  stop("Table 2 analytic sample sizes could not be determined.")
}

if (as.integer(primary_n) != as.integer(primary_n_reported)) {
  stop(
    paste0(
      "Primary analytic N disagrees between the fitted model (",
      primary_n,
      ") and SAFE_06 (",
      primary_n_reported,
      ")."
    )
  )
}


format_analysis_n <- function(n) {

  formatC(
    as.integer(n),
    format = "d",
    big.mark = ","
  )
}


# ------------------------------------------------------------
# 8. Primary model
# ------------------------------------------------------------

primary_linear <- read_safe_excel(
  "SAFE_06_primary_models.xlsx",
  sheet = "Linear_Focal_Results"
)

primary_flexible <- read_safe_excel(
  "SAFE_06_primary_models.xlsx",
  sheet = "Flexible_Global_Tests"
)


primary_linear_final <- primary_linear %>%
  filter(
    model ==
      "Step 3: + Loneliness + Social Support"
  ) %>%
  filter(
    term %in% c(
      "distress_z",
      "loneliness_z",
      "social_support_z"
    )
  ) %>%
  mutate(
    Predictor =
      dplyr::recode(
        term,
        distress_z =
          "Psychological distress",
        loneliness_z =
          "Loneliness",
        social_support_z =
          "Perceived social support"
      )
  ) %>%
  select(
    Predictor,
    OR,
    CI_lower,
    CI_upper,
    linear_p = p_value
  )


primary_flexible_final <- primary_flexible %>%
  filter(
    test %in% c(
      "H1: Distress - flexible final model",
      "H2: Loneliness - flexible final model",
      "H3: Social support - flexible final model"
    )
  ) %>%
  mutate(
    Predictor =
      dplyr::recode(
        test,
        
        `H1: Distress - flexible final model` =
          "Psychological distress",
        
        `H2: Loneliness - flexible final model` =
          "Loneliness",
        
        `H3: Social support - flexible final model` =
          "Perceived social support"
      )
  ) %>%
  select(
    Predictor,
    flexible_p = p_value
  )


primary_table2 <- primary_linear_final %>%
  left_join(
    primary_flexible_final,
    by = "Predictor"
  ) %>%
  mutate(
    Analysis =
      paste0(
        "Full sample (N = ",
        format_analysis_n(primary_n),
        ")"
      ),
    
    `Pre-specified linear OR (95% CI)` =
      pmap_chr(
        list(
          OR,
          CI_lower,
          CI_upper
        ),
        format_or_ci
      ),
    
    `Linear p` =
      vapply(
        linear_p,
        format_p,
        character(1)
      ),
    
    `Flexible-model global p` =
      vapply(
        flexible_p,
        format_p,
        character(1)
      )
  )


# ------------------------------------------------------------
# 9. General-AI-user secondary model
# ------------------------------------------------------------

general_linear <- read_safe_excel(
  "SAFE_07_general_AI_users_model.xlsx",
  sheet = "Linear_Focal_Results"
)

general_flexible <- read_safe_excel(
  "SAFE_07_general_AI_users_model.xlsx",
  sheet = "Flexible_Global_Tests"
)


general_linear_final <- general_linear %>%
  filter(
    model ==
      "Step 4: + Loneliness + Social Support"
  ) %>%
  filter(
    term %in% c(
      "distress_z",
      "loneliness_z",
      "social_support_z"
    )
  ) %>%
  mutate(
    Predictor =
      dplyr::recode(
        term,
        distress_z =
          "Psychological distress",
        loneliness_z =
          "Loneliness",
        social_support_z =
          "Perceived social support"
      )
  ) %>%
  select(
    Predictor,
    OR,
    CI_lower,
    CI_upper,
    linear_p = p_value
  )


general_flexible_final <- general_flexible %>%
  filter(
    test %in% c(
      "Psychological distress",
      "Loneliness",
      "Perceived social support"
    )
  ) %>%
  transmute(
    Predictor =
      test,
    
    flexible_p =
      p_value
  )


general_table2 <- general_linear_final %>%
  left_join(
    general_flexible_final,
    by = "Predictor"
  ) %>%
  mutate(
    Analysis =
      paste0(
        "General AI users (N = ",
        format_analysis_n(general_n),
        ")"
      ),
    
    `Pre-specified linear OR (95% CI)` =
      pmap_chr(
        list(
          OR,
          CI_lower,
          CI_upper
        ),
        format_or_ci
      ),
    
    `Linear p` =
      vapply(
        linear_p,
        format_p,
        character(1)
      ),
    
    `Flexible-model global p` =
      vapply(
        flexible_p,
        format_p,
        character(1)
      )
  )


table2 <- bind_rows(
  primary_table2,
  general_table2
) %>%
  select(
    Analysis,
    Predictor,
    `Pre-specified linear OR (95% CI)`,
    `Linear p`,
    `Flexible-model global p`
  )


# ------------------------------------------------------------
# 10. Publication notes
# ------------------------------------------------------------

table_notes <- tibble(
  
  item = c(
    "Table 1",
    "Table 2 - full sample",
    "Table 2 - secondary analysis",
    "Table 2 - interpretation",
    "Current weight status"
  ),
  
  note = c(
    
    paste(
      "Continuous variables are shown as weighted mean (SD);",
      "categorical variables as unweighted n (weighted %).",
      "No significance tests are presented in Table 1."
    ),
    
    paste(
      "The full-sample model adjusts for age, gender, education,",
      "income, and religious affiliation."
    ),
    
    paste(
      "The general-AI-user model additionally adjusts for",
      "general AI-use frequency."
    ),
    
    paste(
      "Linear ORs are retained as the pre-specified analysis.",
      "Because functional-form diagnostics identified meaningful",
      "nonlinearity, flexible-model global tests and adjusted",
      "probability curves are used to characterize nonlinear associations."
    ),
    
    if (
      n_distinct(
        analysis_dat$analysis_weight
      ) == 1 &&
      unique(
        analysis_dat$analysis_weight
      ) == 1
    ) {
      "All current analyses use placeholder weights equal to 1."
    } else {
      "Final/non-constant survey weights detected."
    }
  )
)


# ============================================================
# FIGURE 1
# ADJUSTED PROBABILITY CURVES
# ============================================================


# ------------------------------------------------------------
# 11. Dense prediction grids
#
# Distress and social support:
# dense grids between weighted 5th and 95th percentiles.
#
# Loneliness:
# actual UCLA-3 raw scores 3-9 only.
# ------------------------------------------------------------

distress_bounds <- weighted_quantile_safe_11(
  primary_dat$distress_z,
  primary_dat$analysis_weight,
  c(
    .05,
    .95
  )
)


support_bounds <- weighted_quantile_safe_11(
  primary_dat$social_support_z,
  primary_dat$analysis_weight,
  c(
    .05,
    .95
  )
)


distress_dense_grid <- seq(
  distress_bounds[1],
  distress_bounds[2],
  length.out = 120
)


support_dense_grid <- seq(
  support_bounds[1],
  support_bounds[2],
  length.out = 120
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


# ------------------------------------------------------------
# 12. Distress predictions
# ------------------------------------------------------------

figure1_distress <- map_dfr(
  
  distress_dense_grid,
  
  function(value) {
    
    basis <- get_spline_basis_11(
      distress_ns_object,
      value
    )
    
    newdata <-
      primary_dat
    
    newdata$distress_ns1 <-
      basis[1]
    
    newdata$distress_ns2 <-
      basis[2]
    
    newdata$distress_ns3 <-
      basis[3]
    
    
    result <- marginal_probability_ci_11(
      model_step3_flexible,
      newdata,
      primary_dat$analysis_weight
    )
    
    
    tibble(
      predictor =
        "Psychological distress",
      
      predictor_value =
        value,
      
      adjusted_probability =
        result["estimate"],
      
      lower_95_CI =
        result["lower"],
      
      upper_95_CI =
        result["upper"]
    )
  }
)


# ------------------------------------------------------------
# 13. Loneliness predictions
# ------------------------------------------------------------

figure1_loneliness <- map_dfr(
  
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
    
    
    basis <- get_spline_basis_11(
      loneliness_ns_object,
      z_value
    )
    
    
    newdata <-
      primary_dat
    
    newdata$loneliness_ns1 <-
      basis[1]
    
    newdata$loneliness_ns2 <-
      basis[2]
    
    
    result <- marginal_probability_ci_11(
      model_step3_flexible,
      newdata,
      primary_dat$analysis_weight
    )
    
    
    tibble(
      predictor =
        "Loneliness",
      
      predictor_value =
        raw_value,
      
      adjusted_probability =
        result["estimate"],
      
      lower_95_CI =
        result["lower"],
      
      upper_95_CI =
        result["upper"]
    )
  }
)


# ------------------------------------------------------------
# 14. Social-support predictions
# ------------------------------------------------------------

figure1_support <- map_dfr(
  
  support_dense_grid,
  
  function(value) {
    
    basis <- get_spline_basis_11(
      support_ns_object,
      value
    )
    
    
    newdata <-
      primary_dat
    
    newdata$support_ns1 <-
      basis[1]
    
    newdata$support_ns2 <-
      basis[2]
    
    newdata$support_ns3 <-
      basis[3]
    
    
    result <- marginal_probability_ci_11(
      model_step3_flexible,
      newdata,
      primary_dat$analysis_weight
    )
    
    
    tibble(
      predictor =
        "Perceived social support",
      
      predictor_value =
        value,
      
      adjusted_probability =
        result["estimate"],
      
      lower_95_CI =
        result["lower"],
      
      upper_95_CI =
        result["upper"]
    )
  }
)


# ------------------------------------------------------------
# 15. Combine Figure 1 data
# ------------------------------------------------------------

figure1_data <- bind_rows(
  figure1_distress,
  figure1_loneliness,
  figure1_support
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
      upper_95_CI,
    
    Predictor =
      dplyr::recode(
        predictor,
        
        `Psychological distress` =
          "Psychological distress (z-score)",
        
        `Loneliness` =
          "Loneliness (UCLA-3 score)",
        
        `Perceived social support` =
          "Perceived social support (z-score)"
      ),
    
    Predictor =
      factor(
        Predictor,
        levels = c(
          "Psychological distress (z-score)",
          "Loneliness (UCLA-3 score)",
          "Perceived social support (z-score)"
        )
      )
  )


# ------------------------------------------------------------
# 16. Figure 1
# ------------------------------------------------------------

figure1 <- ggplot(
  figure1_data,
  aes(
    x = predictor_value,
    y = adjusted_probability_pct
  )
) +
  
  geom_ribbon(
    aes(
      ymin = lower_95_CI_pct,
      ymax = upper_95_CI_pct
    ),
    alpha = 0.18
  ) +
  
  geom_line(
    linewidth = 0.9
  ) +
  
  facet_wrap(
    ~ Predictor,
    scales = "free_x",
    nrow = 1
  ) +
  
  labs(
    x = NULL,
    y =
      "Adjusted probability of AI emotional-support use (%)"
  ) +
  
  theme_classic(
    base_size = 11
  ) +
  
  theme(
    strip.background =
      element_blank(),
    
    strip.text =
      element_text(
        face = "bold"
      ),
    
    axis.title.y =
      element_text(
        margin =
          margin(
            r = 8
          )
      ),
    
    panel.spacing =
      grid::unit(
        1.2,
        "lines"
      )
  )


ggsave(
  filename =
    file.path(
      main_outputs_dir,
      "Figure_1_Adjusted_Probability_Curves.png"
    ),
  
  plot =
    figure1,
  
  width =
    9.5,
  
  height =
    3.4,
  
  dpi =
    600
)


ggsave(
  filename =
    file.path(
      main_outputs_dir,
      "Figure_1_Adjusted_Probability_Curves.pdf"
    ),
  
  plot =
    figure1,
  
  width =
    9.5,
  
  height =
    3.4
)


# ============================================================
# FIGURE 2
# AI EXPERIENCES AND PSYCHOTHERAPY CONTEXT
# ============================================================


# ------------------------------------------------------------
# 16b. Subgroup sizes for the Figure 2 panel titles
#
# Read from the n_valid column of the SAFE_09 outputs, so the
# panel titles can never drift from the analysis that produced
# the estimates they label.
# ------------------------------------------------------------

figure2_subgroup_n <- function(data) {

  n_valid <- unique(data$n_valid)

  if (length(n_valid) != 1 || !is.finite(n_valid)) {
    stop("Figure 2 subgroup size could not be determined.")
  }

  formatC(
    as.integer(n_valid),
    format = "d",
    big.mark = ","
  )
}


# ------------------------------------------------------------
# 17. Panel A — AI experiences
# ------------------------------------------------------------

figure2_experiences <- read_safe_excel(
  "SAFE_09_AI_experience_descriptives.xlsx",
  sheet = "Experiences_Q04"
) %>%
  mutate(
    display_label =
      case_when(
        
        grepl(
          "understand my feelings",
          experience,
          ignore.case = TRUE
        ) ~
          "Helped understand feelings or gain insight",
        
        grepl(
          "calm down",
          experience,
          ignore.case = TRUE
        ) ~
          "Helped calm down or cope with distress",
        
        grepl(
          "couldn't tell people",
          experience,
          ignore.case = TRUE
        ) ~
          "Shared things I couldn't tell people",
        
        grepl(
          "instead of reaching out",
          experience,
          ignore.case = TRUE
        ) ~
          "Used AI instead of reaching out to people",
        
        grepl(
          "relying on it repeatedly",
          experience,
          ignore.case = TRUE
        ) ~
          "Repeatedly relied on AI for comfort",
        
        grepl(
          "inaccurate",
          experience,
          ignore.case = TRUE
        ) ~
          "Responses were inaccurate or unhelpful",
        
        grepl(
          "feel worse",
          experience,
          ignore.case = TRUE
        ) ~
          "Felt worse, confused, or misunderstood",
        
        TRUE ~
          experience
      )
  ) %>%
  arrange(
    weighted_pct
  ) %>%
  mutate(
    display_label =
      factor(
        display_label,
        levels =
          display_label
      )
  )


figure2_A <- ggplot(
  figure2_experiences,
  aes(
    x = weighted_pct,
    y = display_label
  )
) +
  
  geom_errorbar(
    aes(
      xmin = lower_95_CI_pct,
      xmax = upper_95_CI_pct
    ),
    width = 0.18,
    orientation = "y"
  ) +
  
  geom_point(
    size = 2.3
  ) +
  
  scale_x_continuous(
    limits = c(
      0,
      65
    ),
    breaks = c(
      0,
      20,
      40,
      60
    )
  ) +
  
  labs(
    x =
      "Participants reporting experience (%)",
    
    y =
      NULL,
    
    title =
      paste0(
        "Experiences with AI emotional support (n = ",
        figure2_subgroup_n(figure2_experiences),
        ")"
      )
  ) +
  
  theme_classic(
    base_size = 10.5
  ) +
  
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 10.5
      ),
    
    axis.text.y =
      element_text(
        size = 9
      )
  )


# ------------------------------------------------------------
# 18. Panel B — psychotherapy versus AI
# ------------------------------------------------------------

figure2_therapy <- read_safe_excel(
  "SAFE_09_AI_experience_descriptives.xlsx",
  sheet = "Therapy_vs_AI_3cat"
) %>%
  mutate(
    display_label =
      case_when(
        
        grepl(
          "psychological therapy helped more",
          category,
          ignore.case = TRUE
        ) ~
          "Therapy helped more",
        
        grepl(
          "similarly",
          category,
          ignore.case = TRUE
        ) ~
          "Helped similarly",
        
        grepl(
          "AI helped more",
          category,
          ignore.case = TRUE
        ) ~
          "AI helped more",
        
        TRUE ~
          category
      ),
    
    display_label =
      factor(
        display_label,
        levels = rev(
          c(
            "Therapy helped more",
            "Helped similarly",
            "AI helped more"
          )
        )
      )
  )


figure2_B <- ggplot(
  figure2_therapy,
  aes(
    x = weighted_pct,
    y = display_label
  )
) +
  
  geom_errorbar(
    aes(
      xmin = lower_95_CI_pct,
      xmax = upper_95_CI_pct
    ),
    width = 0.18,
    orientation = "y"
  ) +
  
  geom_point(
    size = 2.4
  ) +
  
  scale_x_continuous(
    limits = c(
      0,
      85
    ),
    breaks = c(
      0,
      20,
      40,
      60,
      80
    )
  ) +
  
  labs(
    x =
      "Participants (%)",
    
    y =
      NULL,
    
    title =
      paste0(
        "Perceived helpfulness:\npsychotherapy versus AI (n = ",
        figure2_subgroup_n(figure2_therapy),
        ")"
      )
  ) +
  
  theme_classic(
    base_size = 10.5
  ) +
  
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 10
      )
  )


# ------------------------------------------------------------
# 19. Panel C — sharing therapy-related content
# ------------------------------------------------------------

figure2_sharing <- read_safe_excel(
  "SAFE_09_AI_experience_descriptives.xlsx",
  sheet = "Therapy_Sharing_4cat"
) %>%
  mutate(
    display_label =
      case_when(
        
        category ==
          "No, not at all" ~
          "Not at all",
        
        category ==
          "Yes, infrequently" ~
          "Infrequently",
        
        category ==
          "Yes, frequently" ~
          "Frequently",
        
        grepl(
          "almost after every session",
          category,
          ignore.case = TRUE
        ) ~
          "Regularly / almost every session",
        
        TRUE ~
          category
      ),
    
    display_label =
      factor(
        display_label,
        levels = rev(
          c(
            "Not at all",
            "Infrequently",
            "Frequently",
            "Regularly / almost every session"
          )
        )
      )
  )


figure2_C <- ggplot(
  figure2_sharing,
  aes(
    x = weighted_pct,
    y = display_label
  )
) +
  
  geom_errorbar(
    aes(
      xmin = lower_95_CI_pct,
      xmax = upper_95_CI_pct
    ),
    width = 0.18,
    orientation = "y"
  ) +
  
  geom_point(
    size = 2.4
  ) +
  
  scale_x_continuous(
    limits = c(
      0,
      85
    ),
    breaks = c(
      0,
      20,
      40,
      60,
      80
    )
  ) +
  
  labs(
    x =
      "Participants (%)",
    
    y =
      NULL,
    
    title =
      paste0(
        "Sharing therapy-related content\nwith AI (n = ",
        figure2_subgroup_n(figure2_sharing),
        ")"
      )
  ) +
  
  theme_classic(
    base_size = 10.5
  ) +
  
  theme(
    plot.title =
      element_text(
        face = "bold",
        size = 10
      )
  )


# ------------------------------------------------------------
# 20. Combine Figure 2
# ------------------------------------------------------------

figure2 <- (
  figure2_A /
    (
      figure2_B |
        figure2_C
    )
) +
  
  plot_layout(
    heights = c(
      1.35,
      1
    )
  ) +
  
  plot_annotation(
    tag_levels = "A"
  )


ggsave(
  filename =
    file.path(
      main_outputs_dir,
      "Figure_2_AI_Experiences_and_Psychotherapy.png"
    ),
  
  plot =
    figure2,
  
  width =
    9.5,
  
  height =
    7.2,
  
  dpi =
    600
)


ggsave(
  filename =
    file.path(
      main_outputs_dir,
      "Figure_2_AI_Experiences_and_Psychotherapy.pdf"
    ),
  
  plot =
    figure2,
  
  width =
    9.5,
  
  height =
    7.2
)


# ============================================================
# SUPPLEMENT S1
# DETAILED AI EXPERIENCE DESCRIPTIVES
# ============================================================


s1_helpfulness <- read_safe_excel(
  "SAFE_09_AI_experience_descriptives.xlsx",
  sheet = "Helpfulness_Collapsed"
) %>%
  transmute(
    Section =
      "Perceived helpfulness",
    
    Item =
      summary,
    
    N =
      n_valid,
    
    n =
      n_selected,
    
    Weighted_percent =
      weighted_pct,
    
    Lower_95_CI =
      lower_95_CI_pct,
    
    Upper_95_CI =
      upper_95_CI_pct
  )


s1_experiences <- read_safe_excel(
  "SAFE_09_AI_experience_descriptives.xlsx",
  sheet = "Experiences_Q04"
) %>%
  transmute(
    Section =
      "Experiences",
    
    Item =
      experience,
    
    N =
      n_valid,
    
    n =
      n_selected,
    
    Weighted_percent =
      weighted_pct,
    
    Lower_95_CI =
      lower_95_CI_pct,
    
    Upper_95_CI =
      upper_95_CI_pct
  )


s1_topics <- read_safe_excel(
  "SAFE_09_AI_experience_descriptives.xlsx",
  sheet = "Topics_Q05"
) %>%
  transmute(
    Section =
      "Topics discussed",
    
    Item =
      topic,
    
    N =
      n_valid,
    
    n =
      n_selected,
    
    Weighted_percent =
      weighted_pct,
    
    Lower_95_CI =
      lower_95_CI_pct,
    
    Upper_95_CI =
      upper_95_CI_pct
  )


s1_therapy <- read_safe_excel(
  "SAFE_09_AI_experience_descriptives.xlsx",
  sheet = "Therapy_vs_AI_3cat"
) %>%
  transmute(
    Section =
      "AI compared with psychotherapy",
    
    Item =
      category,
    
    N =
      n_valid,
    
    n =
      n_selected,
    
    Weighted_percent =
      weighted_pct,
    
    Lower_95_CI =
      lower_95_CI_pct,
    
    Upper_95_CI =
      upper_95_CI_pct
  )


s1_sharing <- read_safe_excel(
  "SAFE_09_AI_experience_descriptives.xlsx",
  sheet = "Therapy_Sharing_4cat"
) %>%
  transmute(
    Section =
      "Sharing therapy content with AI",
    
    Item =
      category,
    
    N =
      n_valid,
    
    n =
      n_selected,
    
    Weighted_percent =
      weighted_pct,
    
    Lower_95_CI =
      lower_95_CI_pct,
    
    Upper_95_CI =
      upper_95_CI_pct
  )


supplement_S1_AI_experiences <- bind_rows(
  s1_helpfulness,
  s1_experiences,
  s1_topics,
  s1_therapy,
  s1_sharing
)


# ============================================================
# SUPPLEMENT S2
# FREQUENCY-THRESHOLD ANALYSIS
# ============================================================


threshold_linear <- read_safe_excel(
  "SAFE_08_ordinal_frequency_model.xlsx",
  sheet = "Threshold_Linear_Focal"
)

threshold_flexible <- read_safe_excel(
  "SAFE_08_ordinal_frequency_model.xlsx",
  sheet = "Threshold_Flexible_Tests"
)


s2_linear <- threshold_linear %>%
  mutate(
    Predictor =
      dplyr::recode(
        term,
        distress_z =
          "Psychological distress",
        loneliness_z =
          "Loneliness",
        social_support_z =
          "Perceived social support"
      ),
    
    `Pre-specified linear OR (95% CI)` =
      pmap_chr(
        list(
          OR,
          CI_lower,
          CI_upper
        ),
        format_or_ci
      ),
    
    `Linear p` =
      vapply(
        p_value,
        format_p,
        character(1)
      )
  ) %>%
  select(
    threshold,
    comparison,
    Predictor,
    `Pre-specified linear OR (95% CI)`,
    `Linear p`
  )


s2_flexible <- threshold_flexible %>%
  filter(
    test %in% c(
      "Psychological distress",
      "Loneliness",
      "Perceived social support"
    )
  ) %>%
  transmute(
    threshold,
    
    Predictor =
      test,
    
    `Flexible-model global p` =
      vapply(
        p_value,
        format_p,
        character(1)
      )
  )


supplement_S2_thresholds <- s2_linear %>%
  left_join(
    s2_flexible,
    by = c(
      "threshold",
      "Predictor"
    )
  )


# ============================================================
# SUPPLEMENT S3
# ROBUSTNESS / SENSITIVITY
# ============================================================


component_focal <- read_safe_excel(
  "SAFE_10_robustness_sensitivity.xlsx",
  sheet = "Components_Focal"
)

component_joint <- read_safe_excel(
  "SAFE_10_robustness_sensitivity.xlsx",
  sheet = "Components_Joint_Test"
)

inconsistency_linear <- read_safe_excel(
  "SAFE_10_robustness_sensitivity.xlsx",
  sheet = "Inconsistency_Linear"
)

inconsistency_magnitude <- read_safe_excel(
  "SAFE_10_robustness_sensitivity.xlsx",
  sheet = "Inconsistency_Magnitude"
)

weight_magnitude <- read_safe_excel(
  "SAFE_10_robustness_sensitivity.xlsx",
  sheet = "Weight_Magnitude"
)


s3_components <- component_focal %>%
  transmute(
    Analysis =
      "Distress components",
    
    Result =
      term,
    
    Estimate =
      pmap_chr(
        list(
          OR,
          CI_lower,
          CI_upper
        ),
        format_or_ci
      ),
    
    p =
      vapply(
        p_value,
        format_p,
        character(1)
      )
  )


s3_joint <- component_joint %>%
  transmute(
    Analysis =
      "Distress components",
    
    Result =
      test,
    
    Estimate =
      NA_character_,
    
    p =
      vapply(
        p_value,
        format_p,
        character(1)
      )
  )


s3_inconsistency <- inconsistency_linear %>%
  transmute(
    Analysis =
      "Exclude Q01=0 / Q02>0 inconsistencies",
    
    Result =
      term,
    
    Estimate =
      paste0(
        "Full OR ",
        formatC(
          full_OR,
          digits = 2,
          format = "f"
        ),
        "; sensitivity OR ",
        formatC(
          sensitivity_OR,
          digits = 2,
          format = "f"
        )
      ),
    
    p =
      NA_character_
  )


s3_curve_difference <- inconsistency_magnitude %>%
  transmute(
    Analysis =
      "Exclude Q01=0 / Q02>0 inconsistencies",
    
    Result =
      paste0(
        predictor,
        ": maximum adjusted-probability difference"
      ),
    
    Estimate =
      paste0(
        formatC(
          maximum_absolute_difference_percentage_points,
          digits = 2,
          format = "f"
        ),
        " percentage points"
      ),
    
    p =
      NA_character_
  )


s3_weights <- weight_magnitude %>%
  transmute(
    Analysis =
      "Weighted vs unweighted",
    
    Result =
      paste0(
        predictor,
        ": maximum adjusted-probability difference"
      ),
    
    Estimate =
      paste0(
        formatC(
          maximum_absolute_difference_percentage_points,
          digits = 2,
          format = "f"
        ),
        " percentage points"
      ),
    
    p =
      NA_character_
  )


supplement_S3_robustness <- bind_rows(
  s3_components,
  s3_joint,
  s3_inconsistency,
  s3_curve_difference,
  s3_weights
)


# ============================================================
# EXPORT MAIN RESULTS
# ============================================================


main_output_file <- file.path(
  main_outputs_dir,
  "MAIN_Results_Tables.xlsx"
)


if (
  file.exists(
    main_output_file
  )
) {
  file.remove(
    main_output_file
  )
}


write_xlsx(
  list(
    Table_1 =
      table1,
    
    Table_2 =
      table2,
    
    Notes =
      table_notes,
    
    Figure_1_Data =
      figure1_data,
    
    Figure_2_Experiences =
      figure2_experiences,
    
    Figure_2_Therapy =
      figure2_therapy,
    
    Figure_2_Sharing =
      figure2_sharing
  ),
  
  path =
    main_output_file
)


# ============================================================
# EXPORT SUPPLEMENT
# ============================================================


supplement_output_file <- file.path(
  supplement_outputs_dir,
  "SUPPLEMENT_Results_Tables.xlsx"
)


if (
  file.exists(
    supplement_output_file
  )
) {
  file.remove(
    supplement_output_file
  )
}


write_xlsx(
  list(
    S1_AI_Experiences =
      supplement_S1_AI_experiences,
    
    S2_Frequency_Thresholds =
      supplement_S2_thresholds,
    
    S3_Robustness =
      supplement_S3_robustness
  ),
  
  path =
    supplement_output_file
)


# ============================================================
# FINAL QA
# ============================================================


weight_status <- if (
  n_distinct(
    analysis_dat$analysis_weight
  ) == 1 &&
  unique(
    analysis_dat$analysis_weight
  ) == 1
) {
  
  "PLACEHOLDER: all weights = 1"
  
} else {
  
  "FINAL / NON-CONSTANT WEIGHTS DETECTED"
}


message(
  "Publication outputs complete.\n",
  "  Weight status: ", weight_status, "\n",
  "  Main tables:   ", project_relative(main_output_file), "\n",
  "  Supplement:    ", project_relative(supplement_output_file), "\n",
  "  Figures:       ", project_relative(main_outputs_dir),
  " (Figure_1 and Figure_2, PNG and PDF)"
)
