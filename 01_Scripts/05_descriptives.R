# ============================================================
# 05_descriptives.R
# STRONG-1 Thesis: Descriptive Analyses
#
# Outputs ONLY aggregate, safe-to-share results.
# ============================================================

source_files <- Filter(Negate(is.null), lapply(sys.frames(), `[[`, "ofile"))
file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(source_files) > 0) {
  script_file <- normalizePath(tail(source_files, 1)[[1]])
} else if (length(file_argument) > 0) {
  script_file <- normalizePath(sub("^--file=", "", tail(file_argument, 1)))
} else {
  stop("Could not determine the location of 05_descriptives.R.")
}

source(file.path(dirname(script_file), "00_paths.R"))
rm(source_files, file_argument, script_file)


# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

packages <- c(
  "dplyr",
  "purrr",
  "tibble",
  "survey",
  "writexl"
)

ensure_packages(packages)

library(dplyr)
library(purrr)
library(tibble)
library(survey)
library(writexl)


# ------------------------------------------------------------
# 1. Make sure prepared analysis data exist
# ------------------------------------------------------------

if (!exists("analysis_dat")) {
  
  cat("analysis_dat not found. Running preparation script...\n")
  
  source(
    file.path(
      scripts_dir,
      "04_prepare_analysis_data.R"
    )
  )
}


# ------------------------------------------------------------
# 2. Survey design
# ------------------------------------------------------------

# Currently all weights = 1.
# When final weights arrive, this exact code will use them.

survey_design <- svydesign(
  ids = ~1,
  weights = ~analysis_weight,
  data = analysis_dat
)


# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------

weighted_mean_safe <- function(x, w) {
  
  ok <- !is.na(x) & !is.na(w)
  
  sum(x[ok] * w[ok]) /
    sum(w[ok])
}


weighted_sd_safe <- function(x, w) {
  
  ok <- !is.na(x) & !is.na(w)
  
  x <- x[ok]
  w <- w[ok]
  
  m <- weighted_mean_safe(x, w)
  
  sqrt(
    sum(
      w * (x - m)^2
    ) /
      sum(w)
  )
}


continuous_summary <- function(data, var_name, label, group_name) {
  
  x <- data[[var_name]]
  w <- data$analysis_weight
  
  tibble(
    variable = label,
    group = group_name,
    n = sum(!is.na(x)),
    mean = round(
      weighted_mean_safe(x, w),
      3
    ),
    sd = round(
      weighted_sd_safe(x, w),
      3
    ),
    minimum = if_else(
      any(!is.na(x)),
      min(x, na.rm = TRUE),
      NA_real_
    ),
    maximum = if_else(
      any(!is.na(x)),
      max(x, na.rm = TRUE),
      NA_real_
    )
  )
}


# Weighted prevalence + 95% CI
weighted_prop_ci <- function(indicator, weights) {
  
  temp <- data.frame(
    indicator = indicator,
    weights = weights
  )
  
  temp <- temp[
    !is.na(temp$indicator) &
      !is.na(temp$weights),
  ]
  
  temp_design <- svydesign(
    ids = ~1,
    weights = ~weights,
    data = temp
  )
  
  estimate <- tryCatch(
    
    svyciprop(
      ~indicator,
      temp_design,
      method = "logit"
    ),
    
    error = function(e) {
      
      svyciprop(
        ~indicator,
        temp_design,
        method = "mean"
      )
    }
  )
  
  ci <- confint(estimate)
  
  c(
    estimate = as.numeric(coef(estimate)),
    lower = max(0, as.numeric(ci[1])),
    upper = min(1, as.numeric(ci[2]))
  )
}


# ------------------------------------------------------------
# 4. Sample/group sizes
# ------------------------------------------------------------

group_sizes <- analysis_dat %>%
  group_by(
    ai_support_binary_factor
  ) %>%
  summarise(
    unweighted_n = n(),
    weighted_sum = sum(
      analysis_weight
    ),
    .groups = "drop"
  ) %>%
  rename(
    group =
      ai_support_binary_factor
  )

group_sizes <- bind_rows(
  
  tibble(
    group = "Overall",
    unweighted_n = nrow(
      analysis_dat
    ),
    weighted_sum = sum(
      analysis_dat$analysis_weight
    )
  ),
  
  group_sizes
)


# ------------------------------------------------------------
# 5. Continuous descriptives
# ------------------------------------------------------------

continuous_variables <- c(
  age = "Age",
  phq = "PHQ-9",
  gad = "GAD-7",
  pss = "PSS-10",
  distress_z = "Psychological distress composite (z)",
  loneliness = "UCLA-3 loneliness",
  social_support = "mMOS-SS social support"
)


continuous_overall <- map_dfr(
  names(continuous_variables),
  
  function(v) {
    
    continuous_summary(
      data = analysis_dat,
      var_name = v,
      label = continuous_variables[[v]],
      group_name = "Overall"
    )
  }
)


continuous_no_use <- map_dfr(
  names(continuous_variables),
  
  function(v) {
    
    continuous_summary(
      data =
        analysis_dat %>%
        filter(
          ai_support_binary == 0
        ),
      
      var_name = v,
      label = continuous_variables[[v]],
      group_name = "No AI emotional-support use"
    )
  }
)


continuous_any_use <- map_dfr(
  names(continuous_variables),
  
  function(v) {
    
    continuous_summary(
      data =
        analysis_dat %>%
        filter(
          ai_support_binary == 1
        ),
      
      var_name = v,
      label = continuous_variables[[v]],
      group_name = "Any AI emotional-support use"
    )
  }
)


continuous_descriptives <- bind_rows(
  continuous_overall,
  continuous_no_use,
  continuous_any_use
)


# ------------------------------------------------------------
# 6. Continuous standardized mean differences
#    Users vs non-users
#
# Descriptive only — no p-values.
# ------------------------------------------------------------

continuous_smd <- map_dfr(
  names(continuous_variables),
  
  function(v) {
    
    d0 <- analysis_dat %>%
      filter(
        ai_support_binary == 0
      )
    
    d1 <- analysis_dat %>%
      filter(
        ai_support_binary == 1
      )
    
    m0 <- weighted_mean_safe(
      d0[[v]],
      d0$analysis_weight
    )
    
    m1 <- weighted_mean_safe(
      d1[[v]],
      d1$analysis_weight
    )
    
    sd0 <- weighted_sd_safe(
      d0[[v]],
      d0$analysis_weight
    )
    
    sd1 <- weighted_sd_safe(
      d1[[v]],
      d1$analysis_weight
    )
    
    pooled_sd <- sqrt(
      (
        sd0^2 +
          sd1^2
      ) / 2
    )
    
    tibble(
      variable =
        continuous_variables[[v]],
      
      smd =
        round(
          (m1 - m0) /
            pooled_sd,
          3
        ),
      
      absolute_smd =
        round(
          abs(
            (m1 - m0) /
              pooled_sd
          ),
          3
        )
    )
  }
)


# ------------------------------------------------------------
# 7. Categorical descriptives
# ------------------------------------------------------------

categorical_variables <- c(
  gender_full = "Gender",
  education = "Education",
  income = "Income",
  religion_full = "Religion",
  survey_language = "Survey language"
)


categorical_summary_one <- function(
    data,
    var_name,
    label,
    group_name
) {
  
  data %>%
    mutate(
      category =
        as.character(
          .data[[var_name]]
        )
    ) %>%
    group_by(
      category
    ) %>%
    summarise(
      unweighted_n = n(),
      weighted_n =
        sum(
          analysis_weight
        ),
      .groups = "drop"
    ) %>%
    mutate(
      weighted_pct =
        round(
          100 *
            weighted_n /
            sum(weighted_n),
          2
        ),
      
      variable = label,
      group = group_name
    ) %>%
    select(
      variable,
      category,
      group,
      unweighted_n,
      weighted_pct
    )
}


categorical_descriptives <- map_dfr(
  names(categorical_variables),
  
  function(v) {
    
    bind_rows(
      
      categorical_summary_one(
        data = analysis_dat,
        var_name = v,
        label =
          categorical_variables[[v]],
        group_name = "Overall"
      ),
      
      categorical_summary_one(
        data =
          analysis_dat %>%
          filter(
            ai_support_binary == 0
          ),
        var_name = v,
        label =
          categorical_variables[[v]],
        group_name =
          "No AI emotional-support use"
      ),
      
      categorical_summary_one(
        data =
          analysis_dat %>%
          filter(
            ai_support_binary == 1
          ),
        var_name = v,
        label =
          categorical_variables[[v]],
        group_name =
          "Any AI emotional-support use"
      )
    )
  }
)


# ------------------------------------------------------------
# 7a. Overall participant characteristics / Table 1 source
# ------------------------------------------------------------

participant_characteristics <- bind_rows(
  continuous_overall %>%
    filter(variable == "Age") %>%
    transmute(
      variable,
      category = "Continuous",
      n,
      mean,
      sd,
      minimum,
      maximum,
      weighted_pct = NA_real_
    ),
  categorical_descriptives %>%
    filter(group == "Overall") %>%
    transmute(
      variable,
      category,
      n = unweighted_n,
      mean = NA_real_,
      sd = NA_real_,
      minimum = NA_real_,
      maximum = NA_real_,
      weighted_pct
    )
)

primary_qa_vars <- c(
  "ai_support_binary",
  "age",
  "gender_model",
  "education",
  "income",
  "religion_binary",
  "analysis_weight",
  "distress_z",
  "loneliness_z",
  "social_support_z"
)

primary_qa_complete <- complete.cases(
  analysis_dat[, primary_qa_vars]
)

retained_vs_primary_qa <- bind_rows(
  continuous_summary(
    analysis_dat,
    "age",
    "Age",
    "Retained T1 sample"
  ) %>%
    mutate(
      category = "Continuous",
      pct = NA_real_,
      .after = group
    ),
  continuous_summary(
    analysis_dat[primary_qa_complete, , drop = FALSE],
    "age",
    "Age",
    "Final primary analytic sample"
  ) %>%
    mutate(
      category = "Continuous",
      pct = NA_real_,
      .after = group
    ),
  categorical_summary_one(
    analysis_dat,
    "gender_full",
    "Gender",
    "Retained T1 sample"
  ) %>%
    transmute(
      variable,
      group,
      category,
      n = unweighted_n,
      pct = weighted_pct,
      mean = NA_real_,
      sd = NA_real_,
      minimum = NA_real_,
      maximum = NA_real_
    ),
  categorical_summary_one(
    analysis_dat[primary_qa_complete, , drop = FALSE],
    "gender_full",
    "Gender",
    "Final primary analytic sample"
  ) %>%
    transmute(
      variable,
      group,
      category,
      n = unweighted_n,
      pct = weighted_pct,
      mean = NA_real_,
      sd = NA_real_,
      minimum = NA_real_,
      maximum = NA_real_
    )
)


# ------------------------------------------------------------
# 8. AI prevalence helper
# ------------------------------------------------------------

frequency_prevalence <- function(
    variable,
    label
) {
  
  categories <-
    levels(
      analysis_dat[[variable]]
    )
  
  map_dfr(
    categories,
    
    function(category_name) {
      
      indicator <-
        as.integer(
          analysis_dat[[variable]] ==
            category_name
        )
      
      ci <- weighted_prop_ci(
        indicator,
        analysis_dat$analysis_weight
      )
      
      tibble(
        variable = label,
        category = category_name,
        
        unweighted_n =
          sum(
            indicator == 1,
            na.rm = TRUE
          ),
        
        weighted_pct =
          round(
            100 * ci["estimate"],
            2
          ),
        
        lower_95_CI =
          round(
            100 * ci["lower"],
            2
          ),
        
        upper_95_CI =
          round(
            100 * ci["upper"],
            2
          )
      )
    }
  )
}


# ------------------------------------------------------------
# 9. General AI frequency
# ------------------------------------------------------------

general_ai_frequency <- frequency_prevalence(
  variable =
    "ai_general_frequency",
  
  label =
    "General AI use frequency"
)


# ------------------------------------------------------------
# 10. Emotional-support AI frequency
# ------------------------------------------------------------

emotional_ai_frequency <- frequency_prevalence(
  variable =
    "ai_support_frequency",
  
  label =
    "AI emotional-support use frequency"
)


ai_frequency_prevalence <- bind_rows(
  general_ai_frequency,
  emotional_ai_frequency
)


# ------------------------------------------------------------
# 11. Binary AI prevalence
# ------------------------------------------------------------

binary_prevalence_one <- function(
    variable,
    label
) {
  
  indicator <-
    analysis_dat[[variable]]
  
  ci <- weighted_prop_ci(
    indicator,
    analysis_dat$analysis_weight
  )
  
  tibble(
    measure = label,
    
    unweighted_n =
      sum(
        indicator == 1,
        na.rm = TRUE
      ),
    
    denominator =
      sum(
        !is.na(indicator)
      ),
    
    weighted_pct =
      round(
        100 * ci["estimate"],
        2
      ),
    
    lower_95_CI =
      round(
        100 * ci["lower"],
        2
      ),
    
    upper_95_CI =
      round(
        100 * ci["upper"],
        2
      )
  )
}


ai_binary_prevalence <- bind_rows(
  
  binary_prevalence_one(
    variable =
      "general_ai_user",
    
    label =
      "Any general AI use"
  ),
  
  binary_prevalence_one(
    variable =
      "ai_support_binary",
    
    label =
      "Any AI emotional-support use"
  )
)


# ------------------------------------------------------------
# 12. Emotional-support prevalence
#     among general AI users
# ------------------------------------------------------------

general_ai_users <-
  analysis_dat %>%
  filter(
    general_ai_user == 1
  )


ci_conditional <-
  weighted_prop_ci(
    general_ai_users$
      ai_support_binary,
    
    general_ai_users$
      analysis_weight
  )


emotional_use_among_general_users <- tibble(
  
  measure =
    "AI emotional-support use among general AI users",
  
  unweighted_n =
    sum(
      general_ai_users$
        ai_support_binary == 1
    ),
  
  denominator =
    nrow(
      general_ai_users
    ),
  
  weighted_pct =
    round(
      100 *
        ci_conditional["estimate"],
      2
    ),
  
  lower_95_CI =
    round(
      100 *
        ci_conditional["lower"],
      2
    ),
  
  upper_95_CI =
    round(
      100 *
        ci_conditional["upper"],
      2
    )
)


ai_binary_prevalence <- bind_rows(
  ai_binary_prevalence,
  emotional_use_among_general_users
)


# ------------------------------------------------------------
# 13. General AI × Emotional-support AI cross-tab
# ------------------------------------------------------------

ai_cross_tab <- analysis_dat %>%
  group_by(
    ai_general_frequency,
    ai_support_frequency
  ) %>%
  summarise(
    unweighted_n = n(),
    
    weighted_cell =
      sum(
        analysis_weight
      ),
    
    .groups = "drop"
  ) %>%
  group_by(
    ai_general_frequency
  ) %>%
  mutate(
    weighted_row_pct =
      round(
        100 *
          weighted_cell /
          sum(weighted_cell),
        2
      )
  ) %>%
  ungroup() %>%
  select(
    general_AI_frequency =
      ai_general_frequency,
    
    emotional_support_frequency =
      ai_support_frequency,
    
    unweighted_n,
    
    weighted_row_pct
  )


# ------------------------------------------------------------
# 14. Weight status
# ------------------------------------------------------------

weight_status <- tibble(
  
  metric = c(
    "N",
    "Unique weight values",
    "Minimum weight",
    "Maximum weight",
    "Weight mode"
  ),
  
  value = c(
    as.character(
      nrow(
        analysis_dat
      )
    ),
    
    as.character(
      n_distinct(
        analysis_dat$
          analysis_weight
      )
    ),
    
    as.character(
      min(
        analysis_dat$
          analysis_weight
      )
    ),
    
    as.character(
      max(
        analysis_dat$
          analysis_weight
      )
    ),
    
    ifelse(
      n_distinct(
        analysis_dat$
          analysis_weight
      ) == 1 &&
        unique(
          analysis_dat$
            analysis_weight
        ) == 1,
      
      "Current run: weights are all 1",
      
      "Current run: non-constant weights"
    )
  )
)


# ------------------------------------------------------------
# 15. Export SAFE aggregate workbook
# ------------------------------------------------------------

output_file <- file.path(
  safe_outputs_dir,
  "SAFE_05_descriptives.xlsx"
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
    
    Group_Sizes =
      group_sizes,
    
    AI_Binary_Prevalence =
      ai_binary_prevalence,
    
    AI_Frequency_Prevalence =
      ai_frequency_prevalence,
    
    Continuous_Descriptives =
      continuous_descriptives,
    
    Continuous_SMD =
      continuous_smd,
    
    Categorical_Descriptives =
      categorical_descriptives,

    Participant_Characteristics =
      participant_characteristics,

    Retained_vs_Primary_QA =
      retained_vs_primary_qa,
    
    AI_CrossTab =
      ai_cross_tab,
    
    Weight_Status =
      weight_status
  ),
  
  path =
    output_file
)


# ------------------------------------------------------------
# 16. Completion
# ------------------------------------------------------------

message(
  "Descriptive analyses complete. SAFE output: ",
  project_relative(output_file)
)
