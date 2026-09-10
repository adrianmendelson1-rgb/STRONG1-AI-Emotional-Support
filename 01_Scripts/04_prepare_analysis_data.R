# ============================================================
# 04_prepare_analysis_data.R
# STRONG-1 Thesis: AI Emotional Support
#
# PURPOSE:
# Prepare the analysis dataset in memory.
#
# IMPORTANT:
# - Does NOT exclude participants already retained by STRONG QC.
# - Does NOT save participant-level data to disk.
# - Exports only aggregate quality-control information.
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
  "readxl",
  "dplyr",
  "purrr",
  "tibble",
  "writexl"
)

ensure_packages(packages)

library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(writexl)


# ------------------------------------------------------------
# 1. File settings
# ------------------------------------------------------------

file_path <- data_file

values_sheet <- "T1_Values_200826"



# ------------------------------------------------------------
# 2. Read participant data
# ------------------------------------------------------------

dat_raw <- read_excel(
  file_path,
  sheet = values_sheet,
  guess_max = 10000
)

# ------------------------------------------------------------
# 3. Identify AI multi-select variables
# ------------------------------------------------------------

experience_vars <- grep(
  "^MH_AI_Q04_R",
  names(dat_raw),
  value = TRUE
)

topic_vars <- grep(
  "^MH_AI_Q05_R",
  names(dat_raw),
  value = TRUE
)

if (length(experience_vars) != 7) {
  stop(
    paste(
      "Expected 7 AI experience variables;",
      "found",
      length(experience_vars)
    )
  )
}

if (length(topic_vars) != 12) {
  stop(
    paste(
      "Expected 12 AI topic variables;",
      "found",
      length(topic_vars)
    )
  )
}


# ------------------------------------------------------------
# 4. Required variables
# ------------------------------------------------------------

required_vars <- c(
  "UserLanguage",
  "Age",
  "Gender",
  "Education",
  "Income",
  "Religiosity_Q01",
  "PHQ_Score",
  "GAD_Score",
  "PSS_Score",
  "UCLA_Loneliness_Score",
  "MOSSS_Score",
  "MH_AI_Q01",
  "MH_AI_Q02",
  "MH_AI_Q03",
  "MH_AI_Q06",
  "MH_AI_Q07",
  "MH_Treatment_Q01",
  experience_vars,
  topic_vars
)

missing_required_vars <- setdiff(
  required_vars,
  names(dat_raw)
)

if (length(missing_required_vars) > 0) {
  stop(
    paste(
      "Missing required variables:",
      paste(missing_required_vars, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------
# 5. Validation functions
# ------------------------------------------------------------

assert_allowed <- function(x, allowed, var_name) {
  
  observed <- unique(x[!is.na(x)])
  invalid <- setdiff(observed, allowed)
  
  if (length(invalid) > 0) {
    stop(
      paste0(
        var_name,
        " contains unexpected value(s): ",
        paste(invalid, collapse = ", ")
      )
    )
  }
}


assert_range <- function(x, min_value, max_value, var_name) {
  
  invalid_n <- sum(
    !is.na(x) &
      (x < min_value | x > max_value)
  )
  
  if (invalid_n > 0) {
    stop(
      paste0(
        var_name,
        " contains ",
        invalid_n,
        " value(s) outside expected range."
      )
    )
  }
}


# ------------------------------------------------------------
# 6. Validate raw coding BEFORE recoding
# ------------------------------------------------------------

assert_range(
  dat_raw$Age,
  18,
  120,
  "Age"
)

assert_allowed(
  dat_raw$Gender,
  1:3,
  "Gender"
)

assert_allowed(
  dat_raw$Education,
  1:5,
  "Education"
)

assert_allowed(
  dat_raw$Income,
  1:5,
  "Income"
)

assert_allowed(
  dat_raw$Religiosity_Q01,
  1:6,
  "Religiosity_Q01"
)

assert_range(
  dat_raw$PHQ_Score,
  0,
  27,
  "PHQ_Score"
)

assert_range(
  dat_raw$GAD_Score,
  0,
  21,
  "GAD_Score"
)

assert_range(
  dat_raw$PSS_Score,
  0,
  40,
  "PSS_Score"
)

# UCLA is allowed to contain the known technical -97 code.
assert_allowed(
  dat_raw$UCLA_Loneliness_Score,
  c(-97, 3:9),
  "UCLA_Loneliness_Score"
)

assert_range(
  dat_raw$MOSSS_Score,
  0,
  100,
  "MOSSS_Score"
)

assert_allowed(
  dat_raw$MH_AI_Q01,
  0:4,
  "MH_AI_Q01"
)

assert_allowed(
  dat_raw$MH_AI_Q02,
  0:4,
  "MH_AI_Q02"
)

assert_allowed(
  dat_raw$MH_AI_Q03,
  0:4,
  "MH_AI_Q03"
)

assert_allowed(
  dat_raw$MH_AI_Q06,
  1:5,
  "MH_AI_Q06"
)

# Actual field version contains 4 categories: 0-3.
assert_allowed(
  dat_raw$MH_AI_Q07,
  0:3,
  "MH_AI_Q07"
)

assert_allowed(
  dat_raw$MH_Treatment_Q01,
  0:1,
  "MH_Treatment_Q01"
)


# Validate AI multi-select items
for (v in c(experience_vars, topic_vars)) {
  
  assert_allowed(
    dat_raw[[v]],
    0:1,
    v
  )
}


# ------------------------------------------------------------
# 7. Standardization
#
# Conventional sample standardization: the divisor is n - 1, so
# a one-unit change in a z-scored predictor is a change of one
# sample SD, matching the SDs reported in the descriptive table.
# ------------------------------------------------------------

standardize <- function(x) {

  (x - mean(x, na.rm = TRUE)) /
    sd(x, na.rm = TRUE)
}


# ------------------------------------------------------------
# 8. Pearson correlation on complete pairs
# ------------------------------------------------------------

pairwise_cor <- function(x, y) {

  ok <- complete.cases(x, y)

  cor(x[ok], y[ok])
}


# ------------------------------------------------------------
# 9. Build clean analysis variables
# ------------------------------------------------------------

analysis_dat <- dat_raw %>%
  transmute(
    
    # -----------------------
    # Demographics
    # -----------------------

    survey_language =
      factor(
        UserLanguage
      ),
    
    age =
      as.numeric(Age),
    
    
    # Full gender variable for descriptives
    gender_full =
      factor(
        Gender,
        levels = c(1, 2, 3),
        labels = c(
          "Female",
          "Male",
          "Other"
        )
      ),
    
    
    # Gender variable for regression adjustment.
    # All valid response categories are retained.
    gender_model =
      factor(
        Gender,
        levels = c(1, 2, 3),
        labels = c(
          "Female",
          "Male",
          "Other"
        )
      ),

    
    # Education kept categorical.
    # Secondary education is the reference group.
    education =
      factor(
        Education,
        levels = 1:5,
        labels = c(
          "Primary",
          "Secondary",
          "Post-secondary non-academic",
          "Bachelor's degree",
          "Master's degree or higher"
        )
      ),
    
    
    # Income kept categorical.
    # About average is the reference group.
    income =
      factor(
        Income,
        levels = 1:5,
        labels = c(
          "Much below average",
          "Below average",
          "About average",
          "Above average",
          "Much above average"
        )
      ),
    
    
    # Full affiliation variable retained for descriptives
    religion_full =
      factor(
        Religiosity_Q01,
        levels = 1:6,
        labels = c(
          "Jewish",
          "Muslim",
          "Christian",
          "Druze",
          "Atheist / no religion",
          "Other"
        )
      ),
    
    
    # Regression version:
    # Jewish vs non-Jewish because the smaller
    # individual categories are sparse.
    religion_binary =
      factor(
        if_else(
          Religiosity_Q01 == 1,
          "Jewish",
          "Non-Jewish"
        ),
        levels = c(
          "Jewish",
          "Non-Jewish"
        )
      ),
    
    
    # -----------------------
    # Mental-health scores
    # -----------------------
    
    phq =
      as.numeric(PHQ_Score),
    
    gad =
      as.numeric(GAD_Score),
    
    pss =
      as.numeric(PSS_Score),
    
    
    # Known technical code:
    # -97 means UCLA total score could not be calculated
    # because one item was missing.
    loneliness =
      if_else(
        UCLA_Loneliness_Score == -97,
        NA_real_,
        as.numeric(UCLA_Loneliness_Score)
      ),
    
    social_support =
      as.numeric(MOSSS_Score),
    
    
    # -----------------------
    # General AI use
    # -----------------------
    
    ai_general_raw =
      as.integer(MH_AI_Q01),
    
    ai_general_frequency =
      factor(
        MH_AI_Q01,
        levels = 0:4,
        labels = c(
          "Not at all",
          "A few times",
          "Several times a month",
          "Several times a week",
          "Almost every day / every day"
        )
      ),
    
    general_ai_user =
      as.integer(
        MH_AI_Q01 > 0
      ),
    
    
    # -----------------------
    # Emotional-support AI use
    # -----------------------
    
    ai_support_raw =
      as.integer(MH_AI_Q02),
    
    ai_support_frequency =
      factor(
        MH_AI_Q02,
        levels = 0:4,
        labels = c(
          "Not at all",
          "A few times",
          "Several times a month",
          "Several times a week",
          "Almost every day / every day"
        )
      ),
    
    # PRIMARY OUTCOME
    ai_support_binary =
      as.integer(
        MH_AI_Q02 > 0
      ),
    
    ai_support_binary_factor =
      factor(
        as.integer(MH_AI_Q02 > 0),
        levels = c(0, 1),
        labels = c(
          "No use",
          "Any use"
        )
      ),
    
    
    # Logical inconsistency flag:
    # general AI = never,
    # but emotional-support AI > never.
    ai_logic_inconsistent =
      as.integer(
        MH_AI_Q01 == 0 &
          MH_AI_Q02 > 0
      ),
    
    
    # -----------------------
    # AI follow-up questions
    # -----------------------
    
    ai_helpfulness =
      MH_AI_Q03,
    
    ai_vs_therapy =
      MH_AI_Q06,
    
    therapy_content_sharing =
      MH_AI_Q07,
    
    
    # -----------------------
    # Treatment
    # -----------------------
    
    current_mh_treatment =
      as.integer(
        MH_Treatment_Q01 == 1
      ),
    
    # Theoretical eligibility flag
    therapy_ai_eligible =
      as.integer(
        MH_Treatment_Q01 == 1 &
          MH_AI_Q02 > 0
      ),
    
    # Actual observed follow-up sample
    therapy_ai_followup_observed =
      as.integer(
        !is.na(MH_AI_Q06) |
          !is.na(MH_AI_Q07)
      )
  )


# ------------------------------------------------------------
# 10. Add AI multi-select items
# ------------------------------------------------------------

ai_multiselect <- dat_raw %>%
  select(
    all_of(experience_vars),
    all_of(topic_vars)
  )

analysis_dat <- bind_cols(
  analysis_dat,
  ai_multiselect
)


# ------------------------------------------------------------
# 11. Explicit reference categories
# ------------------------------------------------------------

analysis_dat <- analysis_dat %>%
  mutate(
    
    gender_model =
      relevel(
        gender_model,
        ref = "Female"
      ),
    
    education =
      relevel(
        education,
        ref = "Secondary"
      ),
    
    income =
      relevel(
        income,
        ref = "About average"
      ),
    
    religion_binary =
      relevel(
        religion_binary,
        ref = "Jewish"
      )
  )


# ------------------------------------------------------------
# 12. Standardize focal continuous variables
# ------------------------------------------------------------

analysis_dat <- analysis_dat %>%
  mutate(
    
    phq_z =
      standardize(phq),
    
    gad_z =
      standardize(gad),
    
    pss_z =
      standardize(pss),
    
    loneliness_z =
      standardize(loneliness),
    
    social_support_z =
      standardize(social_support)
  )


# ------------------------------------------------------------
# 13. Psychological-distress composite
# ------------------------------------------------------------

analysis_dat <- analysis_dat %>%
  mutate(
    
    # Equally weighted mean of the three standardized components
    distress_composite =
      (
        phq_z +
          gad_z +
          pss_z
      ) / 3
  )


# Re-standardize the resulting composite so the OR
# represents a 1-SD increase in general distress.
analysis_dat <- analysis_dat %>%
  mutate(
    
    distress_z =
      standardize(distress_composite)
  )


# ------------------------------------------------------------
# 14. Correlations among distress components
# ------------------------------------------------------------

distress_correlations <- tibble(
  pair = c(
    "PHQ - GAD",
    "PHQ - PSS",
    "GAD - PSS"
  ),
  
  correlation = c(
    
    pairwise_cor(analysis_dat$phq, analysis_dat$gad),
    
    pairwise_cor(analysis_dat$phq, analysis_dat$pss),
    
    pairwise_cor(analysis_dat$gad, analysis_dat$pss)
  )
) %>%
  mutate(
    correlation =
      round(
        correlation,
        3
      )
  )


# ------------------------------------------------------------
# 15. Determine model sample sizes
# ------------------------------------------------------------

step1_vars <- c(
  "ai_support_binary",
  "age",
  "gender_model",
  "education",
  "income",
  "religion_binary"
)

step2_vars <- c(
  step1_vars,
  "distress_z"
)

step3_vars <- c(
  step2_vars,
  "loneliness_z",
  "social_support_z"
)

n_step1 <- sum(
  complete.cases(
    analysis_dat[, step1_vars]
  )
)

n_step2 <- sum(
  complete.cases(
    analysis_dat[, step2_vars]
  )
)

n_step3 <- sum(
  complete.cases(
    analysis_dat[, step3_vars]
  )
)

primary_complete <- complete.cases(
  analysis_dat[, step3_vars]
)


# ------------------------------------------------------------
# 15a. Method-reporting sample flow and missingness
# ------------------------------------------------------------

thesis_sample_flow <- tibble(
  stage = c(
    "Retained T1 sample entering analysis",
    "Primary outcome available",
    "Step 1 covariate-complete",
    "Step 2 distress-complete",
    "Step 3 final complete-case",
    "Final primary analytic sample"
  ),
  n = c(
    nrow(analysis_dat),
    sum(!is.na(analysis_dat$ai_support_binary)),
    n_step1,
    n_step2,
    n_step3,
    sum(primary_complete)
  )
)

primary_variable_order <- c(
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

remaining_rows <- rep(TRUE, nrow(analysis_dat))
thesis_exclusion_reasons <- tibble()

for (v in primary_variable_order) {
  missing_v <- is.na(analysis_dat[[v]])
  n_before <- sum(remaining_rows)
  incremental_loss <- sum(remaining_rows & missing_v)
  remaining_rows <- remaining_rows & !missing_v

  thesis_exclusion_reasons <- bind_rows(
    thesis_exclusion_reasons,
    tibble(
      variable = v,
      raw_missing_n = sum(missing_v),
      n_before_variable = n_before,
      incremental_hierarchical_loss = incremental_loss,
      n_after_variable = sum(remaining_rows),
      note = if_else(
        v == "gender_model",
        "Female, Male, and Other are all valid; Gender=Other is not excluded.",
        NA_character_
      )
    )
  )
}

analysis_missingness_vars <- unique(
  c(
    primary_variable_order,
    "ai_general_raw",
    "general_ai_user",
    "ai_support_raw",
    "ai_support_frequency"
  )
)

analysis_missingness <- map_dfr(
  analysis_missingness_vars,
  function(v) {
    tibble(
      variable = v,
      n_total = nrow(analysis_dat),
      n_missing = sum(is.na(analysis_dat[[v]])),
      pct_missing = round(100 * mean(is.na(analysis_dat[[v]])), 3)
    )
  }
)

distress_source_components <- c(
  "PHQ_Score",
  "GAD_Score",
  "PSS_Score"
)

distress_standardized_components <- c(
  "phq_z",
  "gad_z",
  "pss_z"
)

distress_construction <- tibble(
  item = c(
    "Source components",
    "First transformation",
    "Composite",
    "Final transformation",
    "N complete on all source components",
    "N with distress_z"
  ),
  value = c(
    paste(distress_source_components, collapse = ", "),
    paste(distress_standardized_components, collapse = ", "),
    "distress_composite = (phq_z + gad_z + pss_z) / 3",
    "distress_z = standardize(distress_composite)",
    as.character(
      sum(
        complete.cases(
          analysis_dat[, c("phq", "gad", "pss")]
        )
      )
    ),
    as.character(sum(!is.na(analysis_dat$distress_z)))
  )
)

gender_count_table <- function(data, sample_name) {
  category_counts <- data %>%
    count(
      category = gender_model,
      .drop = FALSE,
      name = "n"
    ) %>%
    mutate(
      category = as.character(category),
      pct = round(100 * n / nrow(data), 3),
      sample = sample_name,
      .before = 1
    )

  bind_rows(
    category_counts,
    tibble(
      sample = sample_name,
      category = "Missing gender_model",
      n = sum(is.na(data$gender_model)),
      pct = round(100 * mean(is.na(data$gender_model)), 3)
    )
  )
}

gender_check <- bind_rows(
  gender_count_table(
    analysis_dat,
    "Retained T1 sample"
  ),
  gender_count_table(
    analysis_dat[primary_complete, , drop = FALSE],
    "Final primary analytic sample"
  )
)

# Audit targets: stop if the approved sample/gender handling changes silently.
if (
  nrow(analysis_dat) != 3602 ||
  n_step3 != 3600 ||
  sum(is.na(analysis_dat$gender_model)) != 0 ||
  sum(analysis_dat$gender_model == "Other", na.rm = TRUE) != 9 ||
  thesis_exclusion_reasons$incremental_hierarchical_loss[
    thesis_exclusion_reasons$variable == "gender_model"
  ] != 0
) {
  stop("Thesis sample-flow validation did not reproduce the audited result.")
}


# ------------------------------------------------------------
# 16. Aggregate QA summary
# ------------------------------------------------------------

prep_summary <- tibble(
  
  metric = c(
    "Total participants",
    "AI emotional-support users",
    "General AI users",
    "Logical AI inconsistencies",
    "UCLA -97 recoded to missing",
    "Gender 'Other' retained descriptively",
    "Primary model Step 1 complete N",
    "Primary model Step 2 complete N",
    "Primary model Step 3 complete N",
    "Current mental-health treatment",
    "Treatment + AI theoretical eligibility",
    "AI/therapy follow-up observed"
  ),
  
  value = as.character(
    c(
      nrow(analysis_dat),
      
      sum(
        analysis_dat$ai_support_binary == 1,
        na.rm = TRUE
      ),
      
      sum(
        analysis_dat$general_ai_user == 1,
        na.rm = TRUE
      ),
      
      sum(
        analysis_dat$ai_logic_inconsistent == 1,
        na.rm = TRUE
      ),
      
      sum(
        dat_raw$UCLA_Loneliness_Score == -97,
        na.rm = TRUE
      ),
      
      sum(
        analysis_dat$gender_full == "Other",
        na.rm = TRUE
      ),
      
      n_step1,
      n_step2,
      n_step3,
      
      sum(
        analysis_dat$current_mh_treatment == 1,
        na.rm = TRUE
      ),
      
      sum(
        analysis_dat$therapy_ai_eligible == 1,
        na.rm = TRUE
      ),
      
      sum(
        analysis_dat$therapy_ai_followup_observed == 1,
        na.rm = TRUE
      )
    )
  )
)


# ------------------------------------------------------------
# 17. Missingness summary
# ------------------------------------------------------------

qa_variables <- c(
  "age",
  "survey_language",
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
  "social_support",
  "social_support_z",
  "ai_general_raw",
  "ai_support_raw",
  "ai_support_binary"
)

missingness_summary <- map_dfr(
  qa_variables,
  
  function(v) {
    
    tibble(
      variable = v,
      
      n_missing =
        sum(
          is.na(
            analysis_dat[[v]]
          )
        ),
      
      pct_missing =
        round(
          mean(
            is.na(
              analysis_dat[[v]]
            )
          ) * 100,
          3
        )
    )
  }
)


# ------------------------------------------------------------
# 18. Factor distributions
# ------------------------------------------------------------

factor_count <- function(var_name) {
  
  analysis_dat %>%
    count(
      category =
        as.character(
          .data[[var_name]]
        ),
      name = "n"
    ) %>%
    mutate(
      
      variable =
        var_name,
      
      pct =
        round(
          100 * n / sum(n),
          2
        ),
      
      .before = 1
    )
}


factor_distributions <- bind_rows(
  
  factor_count(
    "gender_full"
  ),

  factor_count(
    "survey_language"
  ),
  
  factor_count(
    "education"
  ),
  
  factor_count(
    "income"
  ),
  
  factor_count(
    "religion_full"
  ),
  
  factor_count(
    "religion_binary"
  )
)


# ------------------------------------------------------------
# 19. Score-range checks after preparation
# ------------------------------------------------------------

score_variables <- c(
  "phq",
  "gad",
  "pss",
  "loneliness",
  "social_support",
  "distress_z",
  "loneliness_z",
  "social_support_z"
)

score_ranges <- map_dfr(
  score_variables,
  
  function(v) {
    
    x <- analysis_dat[[v]]
    
    tibble(
      variable = v,
      
      n_nonmissing =
        sum(
          !is.na(x)
        ),
      
      minimum =
        min(
          x,
          na.rm = TRUE
        ),
      
      maximum =
        max(
          x,
          na.rm = TRUE
        )
    )
  }
)


# ------------------------------------------------------------
# 20. Export the aggregate QA workbook
# ------------------------------------------------------------

output_file <- file.path(
  analysis_outputs_dir,
  "Analysis_04_preparation_check.xlsx"
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
    
    Preparation_Summary =
      prep_summary,
    
    Missingness =
      missingness_summary,
    
    Factor_Distributions =
      factor_distributions,
    
    Score_Ranges =
      score_ranges,
    
    Distress_Correlations =
      distress_correlations,

    Thesis_Sample_Flow =
      thesis_sample_flow,

    Thesis_Exclusion_Reasons =
      thesis_exclusion_reasons,

    Analysis_Missingness =
      analysis_missingness,

    Distress_Construction =
      distress_construction,

    Gender_Check =
      gender_check
  ),
  
  path =
    output_file
)


# ------------------------------------------------------------
# 21. Remove full raw dataset from R environment
# ------------------------------------------------------------

rm(
  dat_raw,
  ai_multiselect
)

gc()


# ------------------------------------------------------------
# 22. Completion
#
# analysis_dat exists only in memory. No participant-level file
# is written to disk at any point in this pipeline.
# ------------------------------------------------------------

message(
  "Analysis data prepared. QA output: ",
  project_relative(output_file)
)
