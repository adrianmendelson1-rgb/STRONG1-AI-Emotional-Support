# ============================================================
# 03_preanalysis_checks.R
# STRONG-1 PRE-ANALYSIS CHECKS
#
# Purpose:
# Validate the source data before any modelling: outcome counts
# within covariate cells, score ranges, missingness, the central
# STRONG quality-control flow, scale reliability, and agreement
# between supplied total scores and item sums.
#
# Validation targets are asserted, not merely reported: the
# script stops if the audited quality-control flow or the
# audited reliability values are not reproduced.
#
# Exports aggregate counts and checks only.
# ============================================================

source_files <- Filter(Negate(is.null), lapply(sys.frames(), `[[`, "ofile"))
file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(source_files) > 0) {
  script_file <- normalizePath(tail(source_files, 1)[[1]])
} else if (length(file_argument) > 0) {
  script_file <- normalizePath(sub("^--file=", "", tail(file_argument, 1)))
} else {
  stop("Could not determine the location of 03_preanalysis_checks.R.")
}

source(file.path(dirname(script_file), "00_paths.R"))
rm(source_files, file_argument, script_file)

# --------------------------------------------------
# 0. Packages
# --------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "purrr",
  "psych",
  "writexl"
)

ensure_packages(packages)

library(readxl)
library(dplyr)
library(purrr)
library(psych)
library(writexl)

file_path <- data_file

dat <- read_excel(
  file_path,
  sheet = "T1_Values_200826"
)

filtering <- read_excel(
  file_path,
  sheet = "T1_Filtering_200826"
)

# --------------------------------------------------
# Create binary primary outcome
# 0 = no emotional-support AI use
# 1 = any emotional-support AI use
# --------------------------------------------------

dat <- dat %>%
  mutate(
    AI_support_binary = if_else(
      MH_AI_Q02 == 0,
      0L,
      1L
    )
  )

# --------------------------------------------------
# 1. Outcome counts within categorical covariates
# --------------------------------------------------

covariates <- c(
  "Gender",
  "Education",
  "Income",
  "Religiosity_Q01"
)

model_cells <- map_dfr(covariates, function(v) {
  
  dat %>%
    count(
      category = .data[[v]],
      AI_support_binary,
      name = "n"
    ) %>%
    group_by(category) %>%
    mutate(
      category_total = sum(n),
      pct_AI_support = round(
        100 * sum(n[AI_support_binary == 1]) / category_total,
        2
      )
    ) %>%
    ungroup() %>%
    mutate(variable = v) %>%
    select(
      variable,
      category,
      AI_support_binary,
      n,
      category_total,
      pct_AI_support
    )
})

# --------------------------------------------------
# 2. General AI use × Emotional-support AI use
# Important logical check
# --------------------------------------------------

AI_cross_tab <- dat %>%
  count(
    General_AI = MH_AI_Q01,
    Emotional_AI = MH_AI_Q02,
    name = "n"
  ) %>%
  arrange(General_AI, Emotional_AI)

# --------------------------------------------------
# 3. Range / validity checks
# --------------------------------------------------

checks <- tibble(
  variable = c(
    "PHQ_Score",
    "GAD_Score",
    "PSS_Score",
    "UCLA_Loneliness_Score",
    "MOSSS_Score",
    "MH_AI_Q01",
    "MH_AI_Q02",
    "MH_Treatment_Q01"
  ),
  
  expected_range = c(
    "0-27",
    "0-21",
    "0-40",
    "3-9",
    "0-100",
    "0-4",
    "0-4",
    "0-1"
  ),
  
  invalid_n = c(
    sum(!is.na(dat$PHQ_Score) &
          !(dat$PHQ_Score >= 0 & dat$PHQ_Score <= 27)),
    
    sum(!is.na(dat$GAD_Score) &
          !(dat$GAD_Score >= 0 & dat$GAD_Score <= 21)),
    
    sum(!is.na(dat$PSS_Score) &
          !(dat$PSS_Score >= 0 & dat$PSS_Score <= 40)),
    
    sum(!is.na(dat$UCLA_Loneliness_Score) &
          !(dat$UCLA_Loneliness_Score >= 3 &
              dat$UCLA_Loneliness_Score <= 9)),
    
    sum(!is.na(dat$MOSSS_Score) &
          !(dat$MOSSS_Score >= 0 & dat$MOSSS_Score <= 100)),
    
    sum(!is.na(dat$MH_AI_Q01) &
          !(dat$MH_AI_Q01 %in% 0:4)),
    
    sum(!is.na(dat$MH_AI_Q02) &
          !(dat$MH_AI_Q02 %in% 0:4)),
    
    sum(!is.na(dat$MH_Treatment_Q01) &
          !(dat$MH_Treatment_Q01 %in% 0:1))
  )
)

# --------------------------------------------------
# 4. Missingness in variables planned for analysis
# --------------------------------------------------

analysis_vars <- c(
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
  "TemporaryWeights"
)

missingness <- map_dfr(analysis_vars, function(v) {
  
  tibble(
    variable = v,
    n_missing = sum(is.na(dat[[v]])),
    pct_missing = round(mean(is.na(dat[[v]])) * 100, 2)
  )
})

# --------------------------------------------------
# 5. Central STRONG QC flow and exclusion reasons
# --------------------------------------------------

qc_flags <- c(
  "MissingID",
  "TestID",
  "Incomplete",
  "BeforeSurveyMadePublic",
  "Inattentive",
  "Duplicates"
)

required_filtering_vars <- c(
  qc_flags,
  "DurationInSeconds",
  "Included"
)

missing_filtering_vars <- setdiff(
  required_filtering_vars,
  names(filtering)
)

if (length(missing_filtering_vars) > 0) {
  stop(
    paste(
      "Missing required T1 filtering variable(s):",
      paste(missing_filtering_vars, collapse = ", ")
    )
  )
}

if (any(is.na(filtering$Included))) {
  stop("T1_Filtering_200826 contains missing Included values.")
}

excluded_qc <- !filtering$Included
retained_qc <- filtering$Included

raw_flag_matrix <- as.matrix(
  filtering[, qc_flags]
)

raw_flag_matrix[is.na(raw_flag_matrix)] <- FALSE
storage.mode(raw_flag_matrix) <- "logical"

raw_reason_n <- rowSums(raw_flag_matrix)

fast_completion <-
  excluded_qc &
  raw_reason_n == 0 &
  !is.na(filtering$DurationInSeconds) &
  filtering$DurationInSeconds < 600

central_qc_flow <- tibble(
  stage = c(
    "Initial records in T1_Filtering_200826",
    "Excluded by central STRONG QC",
    "Retained by central STRONG QC",
    "Rows in T1_Values_200826"
  ),
  n = c(
    nrow(filtering),
    sum(excluded_qc),
    sum(retained_qc),
    nrow(dat)
  )
)

central_qc_reasons <- tibble(
  reason = c(
    qc_flags,
    "Fast completion (<600 sec) with no other QC flag"
  ),
  n_excluded = c(
    map_int(
      qc_flags,
      ~ sum(excluded_qc & filtering[[.x]] %in% TRUE)
    ),
    sum(fast_completion)
  ),
  counting_note = c(
    rep(
      paste(
        "Raw central-QC flag count; the first six flags can overlap",
        "and must not be summed as unique exclusions."
      ),
      length(qc_flags)
    ),
    paste(
      "Mutually exclusive additional reason:",
      "excluded, no first-six flag, duration <600 seconds."
    )
  )
)

profile_matrix <- cbind(
  raw_flag_matrix,
  `Fast completion (<600 sec)` = fast_completion
)

excluded_profile_matrix <- profile_matrix[excluded_qc, , drop = FALSE]

profile_labels <- apply(
  excluded_profile_matrix,
  1,
  function(x) {
    active <- colnames(excluded_profile_matrix)[x]
    if (length(active) == 0) "No coded reason" else paste(active, collapse = " + ")
  }
)

profile_reason_n <- rowSums(excluded_profile_matrix)

profile_counts <- tibble(
  record_type = "Exclusion profile",
  profile_or_summary = profile_labels,
  reasons_per_record = profile_reason_n
) %>%
  count(
    record_type,
    profile_or_summary,
    reasons_per_record,
    name = "n"
  ) %>%
  arrange(desc(n), profile_or_summary)

profile_summary <- tibble(
  record_type = "Reason-count summary",
  profile_or_summary = case_when(
    profile_reason_n == 0 ~ "No coded reason",
    profile_reason_n == 1 ~ "One reason",
    profile_reason_n > 1 ~ "Multiple reasons"
  ),
  reasons_per_record = NA_integer_
) %>%
  count(
    record_type,
    profile_or_summary,
    reasons_per_record,
    name = "n"
  )

central_qc_profiles <- bind_rows(
  profile_counts,
  profile_summary
)

# Audit targets: fail rather than silently exporting an unexpected QC flow.
if (
  nrow(filtering) != 4979 ||
  sum(excluded_qc) != 1377 ||
  sum(retained_qc) != 3602 ||
  nrow(dat) != 3602 ||
  sum(fast_completion) != 57 ||
  sum(retained_qc & filtering$DurationInSeconds < 600, na.rm = TRUE) != 0
) {
  stop("Central STRONG QC validation did not reproduce the audited flow.")
}

# --------------------------------------------------
# 6. Item-level reliability and score validation
# --------------------------------------------------

item_sets <- list(
  `PHQ-9` = paste0("PHQ9_Q0", 1:9),
  `GAD-7` = paste0("GAD7_Q0", 1:7),
  `PSS-10` = paste0("PSS_Q", sprintf("%02d", 1:10)),
  `UCLA-3` = paste0("UCLA_Loneliness_Q0", 1:3),
  `mMOS-SS` = paste0("MOSSS_Q0", 1:8)
)

missing_item_vars <- setdiff(
  unlist(item_sets, use.names = FALSE),
  names(dat)
)

if (length(missing_item_vars) > 0) {
  stop(
    paste(
      "Missing required reliability item(s):",
      paste(missing_item_vars, collapse = ", ")
    )
  )
}

prepare_items <- function(scale_name) {
  item_data <- as.data.frame(dat[item_sets[[scale_name]]])

  if (scale_name == "UCLA-3") {
    item_data[item_data < 0] <- NA
  }

  item_data
}

reliability <- map_dfr(
  names(item_sets),
  function(scale_name) {
    item_data <- prepare_items(scale_name)
    complete_rows <- complete.cases(item_data)

    alpha_result <- psych::alpha(
      item_data[complete_rows, , drop = FALSE],
      check.keys = FALSE,
      warnings = FALSE
    )

    tibble(
      scale = scale_name,
      item_count = length(item_sets[[scale_name]]),
      complete_case_n = sum(complete_rows),
      raw_cronbach_alpha = round(alpha_result$total$raw_alpha, 6),
      scoring_note = if_else(
        scale_name == "PSS-10",
        "Items used exactly as stored; no second reverse scoring.",
        if_else(
          scale_name == "UCLA-3",
          "Negative technical missing codes treated as NA.",
          "Items used as stored."
        )
      )
    )
  }
)

expected_alpha <- c(
  `PHQ-9` = 0.899,
  `GAD-7` = 0.932,
  `PSS-10` = 0.813,
  `UCLA-3` = 0.851,
  `mMOS-SS` = 0.957
)

if (
  any(
    abs(
      reliability$raw_cronbach_alpha -
        expected_alpha[reliability$scale]
    ) > 0.01
  )
) {
  stop("Reliability validation did not reproduce the audited alpha values.")
}

score_validation_specs <- list(
  `PHQ-9` = list(
    supplied = dat$PHQ_Score,
    calculated = rowSums(prepare_items("PHQ-9"))
  ),
  `GAD-7` = list(
    supplied = dat$GAD_Score,
    calculated = rowSums(prepare_items("GAD-7"))
  ),
  `PSS-10` = list(
    supplied = dat$PSS_Score,
    calculated = rowSums(prepare_items("PSS-10"))
  ),
  `UCLA-3` = list(
    supplied = replace(
      dat$UCLA_Loneliness_Score,
      dat$UCLA_Loneliness_Score < 0,
      NA_real_
    ),
    calculated = rowSums(prepare_items("UCLA-3"))
  ),
  `mMOS-SS` = list(
    supplied = dat$MOSSS_Score,
    calculated = (rowMeans(prepare_items("mMOS-SS")) - 1) * 25
  )
)

score_validation <- map_dfr(
  names(score_validation_specs),
  function(scale_name) {
    supplied <- score_validation_specs[[scale_name]]$supplied
    calculated <- score_validation_specs[[scale_name]]$calculated
    comparable <- !is.na(supplied) & !is.na(calculated)
    matches <- comparable & abs(supplied - calculated) < 1e-8

    tibble(
      scale = scale_name,
      n_compared = sum(comparable),
      n_exact_match = sum(matches),
      n_mismatch = sum(comparable & !matches),
      n_item_score_missing_but_supplied_available =
        sum(is.na(calculated) & !is.na(supplied)),
      maximum_absolute_difference = if_else(
        any(comparable),
        max(abs(supplied[comparable] - calculated[comparable])),
        NA_real_
      )
    )
  }
)

if (any(score_validation$n_mismatch > 0)) {
  stop("Item-level score validation found a mismatch with supplied scores.")
}

reproducibility <- tibble(
  item = "R version",
  value = R.version.string
)

# --------------------------------------------------
# 7. Export aggregate-only output
# --------------------------------------------------

output_file <- file.path(
  safe_outputs_dir,
  "SAFE_preanalysis_checks.xlsx"
)

if (file.exists(output_file)) {
  file.remove(output_file)
}

write_xlsx(
  list(
    Model_Cells = model_cells,
    AI_CrossTab = AI_cross_tab,
    Range_Checks = checks,
    Missingness = missingness,
    Central_QC_Flow = central_qc_flow,
    Central_QC_Reasons = central_qc_reasons,
    Central_QC_Profiles = central_qc_profiles,
    Reliability = reliability,
    Score_Validation = score_validation,
    Reproducibility = reproducibility
  ),
  output_file
)

message(
  "Pre-analysis checks written to ",
  project_relative(output_file)
)
