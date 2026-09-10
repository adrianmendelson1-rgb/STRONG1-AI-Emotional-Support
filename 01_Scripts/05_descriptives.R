# ============================================================
# 05_descriptives.R
# STRONG-1 Thesis: Descriptive Analyses
#
# Outputs only aggregate results.
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

# Estimates are unweighted. Design objects are constructed inside
# the helpers below with equal probabilities (ids = ~1, probs = ~1),
# which supplies design-based standard errors only.



# ------------------------------------------------------------
# 3. Helper functions
#
# Conventional sample mean and SD (n - 1 divisor).
# ------------------------------------------------------------

mean_na <- function(x) mean(x, na.rm = TRUE)

sd_na <- function(x) sd(x, na.rm = TRUE)

continuous_summary <- function(data, var_name, label, group_name) {
  
  x <- data[[var_name]]
  
  tibble(
    variable = label,
    group = group_name,
    n = sum(!is.na(x)),
    mean = round(
      mean_na(x),
      3
    ),
    sd = round(
      sd_na(x),
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


# Prevalence + 95% CI (design-based)
prop_ci <- function(indicator) {
  
  temp <- data.frame(
    indicator = indicator
  )

  temp <- temp[
    !is.na(temp$indicator),
    ,
    drop = FALSE
  ]
  
  temp_design <- svydesign(
    ids = ~1,
    probs = ~1,
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
    n = n(),
    .groups = "drop"
  ) %>%
  rename(
    group =
      ai_support_binary_factor
  )

group_sizes <- bind_rows(
  
  tibble(
    group = "Overall",
    n = nrow(
      analysis_dat
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
    
    m0 <- mean_na(
      d0[[v]]
    )
    
    m1 <- mean_na(
      d1[[v]]
    )
    
    sd0 <- sd_na(
      d0[[v]]
    )
    
    sd1 <- sd_na(
      d1[[v]]
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
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      pct =
        round(
          100 *
            n /
            sum(n),
          2
        ),
      
      variable = label,
      group = group_name
    ) %>%
    select(
      variable,
      category,
      group,
      n,
      pct
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
      pct = NA_real_
    ),
  categorical_descriptives %>%
    filter(group == "Overall") %>%
    transmute(
      variable,
      category,
      n,
      mean = NA_real_,
      sd = NA_real_,
      minimum = NA_real_,
      maximum = NA_real_,
      pct
    )
)

primary_qa_vars <- c(
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
      n,
      pct,
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
      n,
      pct,
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
      
      ci <- prop_ci(
        indicator
      )
      
      tibble(
        variable = label,
        category = category_name,
        
        n =
          sum(
            indicator == 1,
            na.rm = TRUE
          ),
        
        pct =
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
  
  ci <- prop_ci(
    indicator
  )
  
  tibble(
    measure = label,
    
    n =
      sum(
        indicator == 1,
        na.rm = TRUE
      ),
    
    denominator =
      sum(
        !is.na(indicator)
      ),
    
    pct =
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
  prop_ci(
    general_ai_users$
      ai_support_binary
  )


emotional_use_among_general_users <- tibble(
  
  measure =
    "AI emotional-support use among general AI users",
  
  n =
    sum(
      general_ai_users$
        ai_support_binary == 1
    ),
  
  denominator =
    nrow(
      general_ai_users
    ),
  
  pct =
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
    n = n(),
    
    .groups = "drop"
  ) %>%
  group_by(
    ai_general_frequency
  ) %>%
  mutate(
    row_pct =
      round(
        100 *
          n /
          sum(n),
        2
      )
  ) %>%
  ungroup() %>%
  select(
    general_AI_frequency =
      ai_general_frequency,
    
    emotional_support_frequency =
      ai_support_frequency,
    
    n,
    
    row_pct
  )


# ------------------------------------------------------------
# 14. Export the aggregate results workbook
# ------------------------------------------------------------

output_file <- file.path(
  analysis_outputs_dir,
  "Analysis_05_descriptives.xlsx"
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
      ai_cross_tab
  ),
  
  path =
    output_file
)


# ------------------------------------------------------------
# 15. Completion
# ------------------------------------------------------------

message(
  "Descriptive analyses complete. Output: ",
  project_relative(output_file)
)
