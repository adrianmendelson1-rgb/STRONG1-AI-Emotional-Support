# ============================================================
# 09_AI_experience_descriptives.R
# STRONG-1 Thesis
#
# SECONDARY DESCRIPTIVE AIM
#
# Among participants who used AI for emotional support:
#   Q03 = perceived helpfulness
#   Q04 = experiences of AI emotional support
#   Q05 = emotional / mental-health topics discussed
#
# Among participants who BOTH:
#   - currently receive mental-health treatment
#   - used AI for emotional support
#
#   Q06 = comparative helpfulness: therapy vs AI
#   Q07 = sharing therapy-related content with AI
#
# These analyses are DESCRIPTIVE ONLY.
# No hypothesis tests or regressions are performed here.
#
# Weighted estimates are produced using analysis_weight.
# Current results remain provisional while weights = 1.
#
# Free-text responses are NEVER exported.
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
  stop("Could not determine the location of 09_AI_experience_descriptives.R.")
}

source(file.path(dirname(script_file), "00_paths.R"))
rm(source_files, file_argument, script_file)


# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

packages <- c(
  "readxl",
  "dplyr",
  "purrr",
  "survey",
  "tibble",
  "writexl"
)

ensure_packages(packages)

library(dplyr)
library(purrr)
library(survey)
library(tibble)
library(writexl)


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
# 2. Build descriptive dataset
#
# Prefer variables already retained in analysis_dat.
# If some original AI questionnaire columns were not retained,
# recover them from the same raw Values sheet via data_file.
#
# Row order is preserved because analysis_dat is constructed
# directly from the same participant-level Values sheet.
# ------------------------------------------------------------

descriptive_dat <- analysis_dat


q04_vars <- sprintf(
  "MH_AI_Q04_R%02d",
  0:6
)

q05_vars <- sprintf(
  "MH_AI_Q05_R%02d",
  1:12
)

raw_needed <- c(
  "MH_AI_Q03",
  q04_vars,
  q05_vars,
  "MH_AI_Q06",
  "MH_AI_Q07",
  "MH_Treatment_Q01"
)


missing_raw <- setdiff(
  raw_needed,
  names(descriptive_dat)
)


if (length(missing_raw) > 0) {
  raw_values <- readxl::read_excel(
    data_file,
    sheet = "T1_Values_200826",
    guess_max = 10000
  )

  if (nrow(raw_values) != nrow(descriptive_dat)) {
    stop(
      "Raw Values and analysis_dat contain different numbers of rows."
    )
  }

  still_missing <- setdiff(
    missing_raw,
    names(raw_values)
  )

  if (length(still_missing) > 0) {
    stop(
      paste(
        "Required variables not found:",
        paste(
          still_missing,
          collapse = ", "
        )
      )
    )
  }

  for (v in missing_raw) {
    descriptive_dat[[v]] <- raw_values[[v]]
  }

  rm(raw_values)
}


# ------------------------------------------------------------
# 3. Required prepared variables
# ------------------------------------------------------------

prepared_needed <- c(
  "ai_support_binary",
  "analysis_weight"
)

prepared_missing <- setdiff(
  prepared_needed,
  names(descriptive_dat)
)

if (length(prepared_missing) > 0) {
  
  stop(
    paste(
      "Prepared variables missing:",
      paste(
        prepared_missing,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 4. Safe numeric conversion
# ------------------------------------------------------------

safe_numeric <- function(x) {
  
  if (is.factor(x)) {
    x <- as.character(x)
  }
  
  suppressWarnings(
    as.numeric(x)
  )
}


numeric_vars <- c(
  "MH_AI_Q03",
  q04_vars,
  q05_vars,
  "MH_AI_Q06",
  "MH_AI_Q07",
  "MH_Treatment_Q01"
)

for (v in numeric_vars) {
  descriptive_dat[[v]] <-
    safe_numeric(
      descriptive_dat[[v]]
    )
}


# ------------------------------------------------------------
# 5. Validate questionnaire coding
# ------------------------------------------------------------

check_allowed_codes <- function(
    x,
    allowed,
    variable_name,
    subgroup = rep(
      TRUE,
      length(x)
    )
) {
  
  observed <- sort(
    unique(
      x[
        subgroup &
          !is.na(x)
      ]
    )
  )
  
  invalid <- setdiff(
    observed,
    allowed
  )
  
  if (length(invalid) > 0) {
    
    stop(
      paste0(
        variable_name,
        " contains unexpected code(s): ",
        paste(
          invalid,
          collapse = ", "
        )
      )
    )
  }
}


ai_user_flag <-
  descriptive_dat$ai_support_binary == 1


check_allowed_codes(
  descriptive_dat$MH_AI_Q03,
  0:4,
  "MH_AI_Q03",
  ai_user_flag
)


for (v in q04_vars) {
  
  check_allowed_codes(
    descriptive_dat[[v]],
    0:1,
    v,
    ai_user_flag
  )
}


for (v in q05_vars) {
  
  check_allowed_codes(
    descriptive_dat[[v]],
    0:1,
    v,
    ai_user_flag
  )
}


check_allowed_codes(
  descriptive_dat$MH_Treatment_Q01,
  0:1,
  "MH_Treatment_Q01"
)


# ------------------------------------------------------------
# 6. Define relevant subgroups
# ------------------------------------------------------------

ai_users_dat <- descriptive_dat %>%
  filter(
    ai_support_binary == 1
  )


therapy_ai_dat <- descriptive_dat %>%
  filter(
    ai_support_binary == 1,
    MH_Treatment_Q01 == 1
  )


check_allowed_codes(
  therapy_ai_dat$MH_AI_Q06,
  1:5,
  "MH_AI_Q06"
)


# IMPORTANT:
# Actual field/data version of Q07 has FOUR categories:
#
# 0 = No, not at all
# 1 = Yes, infrequently
# 2 = Yes, frequently
# 3 = Yes, almost after every session / regularly
#
# The earlier draft questionnaire contained an additional
# "once or twice" option, but that category was NOT present
# in the administered dataset.
check_allowed_codes(
  therapy_ai_dat$MH_AI_Q07,
  0:3,
  "MH_AI_Q07"
)


# ------------------------------------------------------------
# 7. Weighted binary proportion + 95% CI
#
# Used for:
# - individual multi-select items
# - collapsed descriptive categories
#
# CI uses survey::svyciprop(logit) when possible,
# with a conservative normal-CI fallback.
# ------------------------------------------------------------

weighted_prop_ci <- function(
    indicator,
    weights
) {
  
  ok <-
    !is.na(indicator) &
    !is.na(weights)
  
  indicator <-
    as.numeric(
      indicator[ok]
    )
  
  weights <-
    weights[ok]
  
  
  n_valid <-
    length(
      indicator
    )
  
  n_selected <-
    sum(
      indicator == 1
    )
  
  
  if (n_valid == 0) {
    
    return(
      tibble(
        n_valid = 0,
        n_selected = 0,
        weighted_prop = NA_real_,
        lower_95_CI = NA_real_,
        upper_95_CI = NA_real_
      )
    )
  }
  
  
  temp_dat <- data.frame(
    indicator = indicator,
    w = weights
  )
  
  
  temp_design <- svydesign(
    ids = ~1,
    weights = ~w,
    data = temp_dat
  )
  
  
  weighted_prop <-
    as.numeric(
      coef(
        svymean(
          ~indicator,
          temp_design,
          na.rm = TRUE
        )
      )[1]
    )
  
  
  ci_result <- tryCatch(
    
    {
      
      ci_object <- svyciprop(
        ~indicator,
        temp_design,
        method = "logit",
        level = .95,
        na.rm = TRUE
      )
      
      ci <- confint(
        ci_object
      )
      
      c(
        lower =
          as.numeric(
            ci[1]
          ),
        
        upper =
          as.numeric(
            ci[2]
          )
      )
    },
    
    error = function(e) {
      
      mean_object <- svymean(
        ~indicator,
        temp_design,
        na.rm = TRUE
      )
      
      se <-
        SE(
          mean_object
        )[1]
      
      c(
        lower =
          max(
            0,
            weighted_prop -
              1.96 * se
          ),
        
        upper =
          min(
            1,
            weighted_prop +
              1.96 * se
          )
      )
    }
  )
  
  
  tibble(
    n_valid =
      n_valid,
    
    n_selected =
      n_selected,
    
    weighted_prop =
      weighted_prop,
    
    lower_95_CI =
      ci_result["lower"],
    
    upper_95_CI =
      ci_result["upper"]
  )
}


# ------------------------------------------------------------
# 8. Weighted categorical distribution helper
# ------------------------------------------------------------

weighted_category_table <- function(
    data,
    variable,
    codes,
    labels
) {
  
  stopifnot(
    length(codes) ==
      length(labels)
  )
  
  
  map2_dfr(
    
    codes,
    labels,
    
    function(
    code,
    label
    ) {
      
      x <-
        data[[variable]]
      
      
      result <-
        weighted_prop_ci(
          ifelse(
            is.na(x),
            NA,
            as.integer(
              x == code
            )
          ),
          data$analysis_weight
        )
      
      
      tibble(
        variable =
          variable,
        
        code =
          code,
        
        category =
          label
      ) %>%
        
        bind_cols(
          result
        ) %>%
        
        mutate(
          weighted_pct =
            100 *
            weighted_prop,
          
          lower_95_CI_pct =
            100 *
            lower_95_CI,
          
          upper_95_CI_pct =
            100 *
            upper_95_CI
        )
    }
  )
}


# ------------------------------------------------------------
# 9. Weighted continuous descriptive helper
#
# Used only for the numeric summary of Q03.
# Full ordinal distribution is also retained.
# ------------------------------------------------------------

weighted_continuous_summary <- function(
    data,
    variable
) {
  
  temp <- data %>%
    transmute(
      score =
        .data[[variable]],
      
      analysis_weight =
        analysis_weight
    ) %>%
    filter(
      !is.na(score),
      !is.na(analysis_weight)
    )
  
  
  if (nrow(temp) == 0) {
    
    return(
      tibble(
        variable = variable,
        n = 0,
        weighted_mean = NA_real_,
        weighted_SD = NA_real_,
        minimum = NA_real_,
        maximum = NA_real_
      )
    )
  }
  
  
  temp_design <- svydesign(
    ids = ~1,
    weights = ~analysis_weight,
    data = temp
  )
  
  
  mean_estimate <-
    as.numeric(
      coef(
        svymean(
          ~score,
          temp_design
        )
      )[1]
    )
  
  
  variance_estimate <-
    as.numeric(
      coef(
        svyvar(
          ~score,
          temp_design
        )
      )[1]
    )
  
  
  tibble(
    variable =
      variable,
    
    n =
      nrow(temp),
    
    weighted_mean =
      mean_estimate,
    
    weighted_SD =
      sqrt(
        variance_estimate
      ),
    
    minimum =
      min(
        temp$score
      ),
    
    maximum =
      max(
        temp$score
      )
  )
}


# ============================================================
# PART A
# Q03 — PERCEIVED HELPFULNESS
# ============================================================


# ------------------------------------------------------------
# 10. Full five-category helpfulness distribution
# ------------------------------------------------------------

helpfulness_distribution <- weighted_category_table(
  
  ai_users_dat,
  
  "MH_AI_Q03",
  
  codes = 0:4,
  
  labels = c(
    "Did not help at all",
    "Helped a little",
    "Helped moderately",
    "Helped a lot",
    "Helped a very great deal"
  )
)


# ------------------------------------------------------------
# 11. Helpfulness mean / SD
#
# Coding:
# 0 = did not help
# 4 = helped a very great deal
#
# This is supplementary to the full category distribution.
# ------------------------------------------------------------

helpfulness_summary <-
  weighted_continuous_summary(
    ai_users_dat,
    "MH_AI_Q03"
  )


# ------------------------------------------------------------
# 12. Clinically interpretable collapsed helpfulness summaries
#
# These are descriptive summaries only.
# ------------------------------------------------------------

helpfulness_collapsed <- bind_rows(
  
  tibble(
    summary =
      "At least moderately helpful"
  ) %>%
    bind_cols(
      weighted_prop_ci(
        ifelse(
          is.na(
            ai_users_dat$MH_AI_Q03
          ),
          NA,
          as.integer(
            ai_users_dat$MH_AI_Q03 >= 2
          )
        ),
        ai_users_dat$analysis_weight
      )
    ),
  
  tibble(
    summary =
      "Helped a lot or very great deal"
  ) %>%
    bind_cols(
      weighted_prop_ci(
        ifelse(
          is.na(
            ai_users_dat$MH_AI_Q03
          ),
          NA,
          as.integer(
            ai_users_dat$MH_AI_Q03 >= 3
          )
        ),
        ai_users_dat$analysis_weight
      )
    )
) %>%
  
  mutate(
    weighted_pct =
      100 *
      weighted_prop,
    
    lower_95_CI_pct =
      100 *
      lower_95_CI,
    
    upper_95_CI_pct =
      100 *
      upper_95_CI
  )


# ============================================================
# PART B
# Q04 — EXPERIENCES OF AI EMOTIONAL SUPPORT
# ============================================================


# ------------------------------------------------------------
# 13. Q04 item labels
#
# Order follows R00-R06 exactly.
# ------------------------------------------------------------

q04_labels <- c(
  
  "It helped me calm down or cope with distress",
  
  "It helped me understand my feelings or gain insight",
  
  "I shared things I couldn't tell people",
  
  "I used it instead of reaching out to people",
  
  "I found myself relying on it repeatedly for comfort",
  
  "The responses were inaccurate, unhelpful, or frustrating",
  
  "It made me feel worse, more confused, or misunderstood"
)


# ------------------------------------------------------------
# 14. Q04 weighted prevalence
# ------------------------------------------------------------

experiences_results <- map2_dfr(
  
  q04_vars,
  q04_labels,
  
  function(
    variable,
    label
  ) {
    
    result <-
      weighted_prop_ci(
        ai_users_dat[[variable]],
        ai_users_dat$analysis_weight
      )
    
    
    tibble(
      variable =
        variable,
      
      experience =
        label
    ) %>%
      
      bind_cols(
        result
      ) %>%
      
      mutate(
        weighted_pct =
          100 *
          weighted_prop,
        
        lower_95_CI_pct =
          100 *
          lower_95_CI,
        
        upper_95_CI_pct =
          100 *
          upper_95_CI
      )
  }
) %>%
  
  arrange(
    desc(
      weighted_pct
    )
  )


# ============================================================
# PART C
# Q05 — EMOTIONAL / MENTAL-HEALTH TOPICS
# ============================================================


# ------------------------------------------------------------
# 15. Q05 topic labels
#
# Order follows R01-R12 exactly.
# ------------------------------------------------------------

q05_labels <- c(
  
  "Anxiety / worries / panic attacks",
  
  "Stress / overload / burnout",
  
  "Low mood / depression",
  
  "Overwhelm or difficulty with emotional regulation",
  
  "Sleep difficulties",
  
  "Loneliness / need for conversation and support",
  
  "Interpersonal relationships and conflicts",
  
  "Trauma, mourning, or difficult memories",
  
  "Body image / eating or harmful habits",
  
  "Personal dilemmas / values / decision making",
  
  "Regarding professional help (before / after)",
  
  "Other"
)


# ------------------------------------------------------------
# 16. Q05 weighted prevalence
#
# IMPORTANT:
# Only the binary "Other" selection is summarized.
# MH_AI_Q05_Text is deliberately NEVER accessed or exported.
# ------------------------------------------------------------

topics_results <- map2_dfr(
  
  q05_vars,
  q05_labels,
  
  function(
    variable,
    label
  ) {
    
    result <-
      weighted_prop_ci(
        ai_users_dat[[variable]],
        ai_users_dat$analysis_weight
      )
    
    
    tibble(
      variable =
        variable,
      
      topic =
        label
    ) %>%
      
      bind_cols(
        result
      ) %>%
      
      mutate(
        weighted_pct =
          100 *
          weighted_prop,
        
        lower_95_CI_pct =
          100 *
          lower_95_CI,
        
        upper_95_CI_pct =
          100 *
          upper_95_CI
      )
  }
) %>%
  
  arrange(
    desc(
      weighted_pct
    )
  )


# ============================================================
# PART D
# Q06 — AI VERSUS CURRENT THERAPY
# ============================================================


# ------------------------------------------------------------
# 17. Full Q06 five-category comparison
#
# Actual coding:
# 1 = therapy much more helpful
# 2 = therapy somewhat more helpful
# 3 = similar
# 4 = AI somewhat more helpful
# 5 = AI much more helpful
# ------------------------------------------------------------

therapy_comparison_5cat <- weighted_category_table(
  
  therapy_ai_dat,
  
  "MH_AI_Q06",
  
  codes = 1:5,
  
  labels = c(
    "Psychological therapy helped much more than AI",
    "Psychological therapy helped somewhat more than AI",
    "Both helped to a similar extent",
    "AI helped somewhat more than psychological therapy",
    "AI helped much more than psychological therapy"
  )
)


# ------------------------------------------------------------
# 18. Collapsed Q06 three-category comparison
#
# Therapy favored = 1-2
# Similar          = 3
# AI favored       = 4-5
#
# This is descriptive and easier to communicate in the paper.
# ------------------------------------------------------------

q06_collapsed <- case_when(
  
  therapy_ai_dat$MH_AI_Q06 %in%
    c(
      1,
      2
    ) ~
    1,
  
  therapy_ai_dat$MH_AI_Q06 == 3 ~
    2,
  
  therapy_ai_dat$MH_AI_Q06 %in%
    c(
      4,
      5
    ) ~
    3,
  
  TRUE ~
    NA_real_
)


therapy_ai_dat$Q06_collapsed <-
  q06_collapsed


therapy_comparison_3cat <- weighted_category_table(
  
  therapy_ai_dat,
  
  "Q06_collapsed",
  
  codes = 1:3,
  
  labels = c(
    "Psychological therapy helped more",
    "AI and psychological therapy helped similarly",
    "AI helped more"
  )
)


# ============================================================
# PART E
# Q07 — SHARING THERAPY CONTENT WITH AI
# ============================================================


# ------------------------------------------------------------
# 19. Full ACTUAL four-category Q07 distribution
#
# The administered data contain:
#
# 0 = No, not at all
# 1 = Yes, infrequently
# 2 = Yes, frequently
# 3 = Yes, almost after every session / regularly
#
# There is NO "once or twice" response category in the
# administered data.
# ------------------------------------------------------------

therapy_sharing_4cat <- weighted_category_table(
  
  therapy_ai_dat,
  
  "MH_AI_Q07",
  
  codes = 0:3,
  
  labels = c(
    "No, not at all",
    "Yes, infrequently",
    "Yes, frequently",
    "Yes, almost after every session / regularly"
  )
)


# ------------------------------------------------------------
# 20. Q07 collapsed sharing summaries
# ------------------------------------------------------------

therapy_sharing_summary <- bind_rows(
  
  tibble(
    summary =
      "Any sharing of therapy-related content with AI"
  ) %>%
    
    bind_cols(
      weighted_prop_ci(
        ifelse(
          is.na(
            therapy_ai_dat$MH_AI_Q07
          ),
          NA,
          as.integer(
            therapy_ai_dat$MH_AI_Q07 >= 1
          )
        ),
        therapy_ai_dat$analysis_weight
      )
    ),
  
  
  tibble(
    summary =
      "Frequent or regular sharing of therapy-related content"
  ) %>%
    
    bind_cols(
      weighted_prop_ci(
        ifelse(
          is.na(
            therapy_ai_dat$MH_AI_Q07
          ),
          NA,
          as.integer(
            therapy_ai_dat$MH_AI_Q07 >= 2
          )
        ),
        therapy_ai_dat$analysis_weight
      )
    )
) %>%
  
  mutate(
    weighted_pct =
      100 *
      weighted_prop,
    
    lower_95_CI_pct =
      100 *
      lower_95_CI,
    
    upper_95_CI_pct =
      100 *
      upper_95_CI
  )


# ============================================================
# PART F
# SKIP-LOGIC / DENOMINATOR QA
# ============================================================


# ------------------------------------------------------------
# 21. Number of complete Q04 and Q05 sets among AI users
# ------------------------------------------------------------

q04_complete_n <- sum(
  complete.cases(
    ai_users_dat[
      ,
      q04_vars,
      drop = FALSE
    ]
  )
)


q05_complete_n <- sum(
  complete.cases(
    ai_users_dat[
      ,
      q05_vars,
      drop = FALSE
    ]
  )
)


# ------------------------------------------------------------
# 22. Non-eligible Q06 / Q07 responses
#
# Should normally be zero if skip logic worked correctly.
# ------------------------------------------------------------

therapy_eligible_full <-
  descriptive_dat$ai_support_binary == 1 &
  descriptive_dat$MH_Treatment_Q01 == 1


q06_noneligible_observed <- sum(
  !therapy_eligible_full &
    !is.na(
      descriptive_dat$MH_AI_Q06
    )
)


q07_noneligible_observed <- sum(
  !therapy_eligible_full &
    !is.na(
      descriptive_dat$MH_AI_Q07
    )
)


# ------------------------------------------------------------
# 23. Denominator / skip-logic table
# ------------------------------------------------------------

denominator_QA <- tibble(
  
  measure = c(
    "AI emotional-support users",
    "Q03 helpfulness observed",
    "Q04 complete multi-select set",
    "Q05 complete multi-select set",
    "Current treatment + AI support eligible",
    "Q06 therapy comparison observed",
    "Q07 therapy-sharing observed",
    "Q06 observed outside eligible subgroup",
    "Q07 observed outside eligible subgroup"
  ),
  
  n = c(
    
    nrow(
      ai_users_dat
    ),
    
    sum(
      !is.na(
        ai_users_dat$MH_AI_Q03
      )
    ),
    
    q04_complete_n,
    
    q05_complete_n,
    
    nrow(
      therapy_ai_dat
    ),
    
    sum(
      !is.na(
        therapy_ai_dat$MH_AI_Q06
      )
    ),
    
    sum(
      !is.na(
        therapy_ai_dat$MH_AI_Q07
      )
    ),
    
    q06_noneligible_observed,
    
    q07_noneligible_observed
  )
)


# ============================================================
# PART G
# SUMMARY / DOCUMENTATION
# ============================================================


# ------------------------------------------------------------
# 24. Analysis summary
# ------------------------------------------------------------

analysis_summary <- tibble(
  
  item = c(
    "Analysis role",
    "Primary descriptive subgroup",
    "Therapy comparison subgroup",
    "Q03",
    "Q04",
    "Q05",
    "Q06",
    "Q07",
    "Free-text handling",
    "Inferential testing"
  ),
  
  specification = c(
    
    "Secondary descriptive analysis of AI emotional-support experiences",
    
    paste0(
      "Participants reporting any AI emotional-support use; N = ",
      nrow(
        ai_users_dat
      )
    ),
    
    paste0(
      "Participants reporting both current mental-health treatment and AI emotional-support use; N = ",
      nrow(
        therapy_ai_dat
      )
    ),
    
    "Perceived helpfulness: full five-category distribution plus weighted mean/SD and collapsed summaries",
    
    "Seven multi-select experience items: weighted prevalence and 95% CI",
    
    "Twelve multi-select topic items: weighted prevalence and 95% CI",
    
    "Five-category therapy-versus-AI comparative helpfulness plus three-category descriptive collapse",
    
    "Actual administered four-category therapy-content-sharing variable plus collapsed sharing summaries",
    
    "No free-text response content is accessed or exported",
    
    "None; secondary aims are descriptive"
  )
)


# ------------------------------------------------------------
# 25. Questionnaire item dictionary for output
# ------------------------------------------------------------

item_dictionary <- bind_rows(
  
  tibble(
    variable =
      "MH_AI_Q03",
    
    domain =
      "Perceived helpfulness",
    
    label =
      "To what extent did using AI for emotional support help you?"
  ),
  
  
  tibble(
    variable =
      q04_vars,
    
    domain =
      "Experience",
    
    label =
      q04_labels
  ),
  
  
  tibble(
    variable =
      q05_vars,
    
    domain =
      "Topic",
    
    label =
      q05_labels
  ),
  
  
  tibble(
    variable =
      "MH_AI_Q06",
    
    domain =
      "AI versus psychotherapy",
    
    label =
      "What helped more in coping emotionally: psychological therapy or AI?"
  ),
  
  
  tibble(
    variable =
      "MH_AI_Q07",
    
    domain =
      "Sharing therapy content with AI",
    
    label =
      "Did the participant share therapy-related content with AI?"
  )
)


# ------------------------------------------------------------
# 26. Weight status
# ------------------------------------------------------------

weight_status <- tibble(
  
  item = c(
    "Total prepared N",
    "AI emotional-support user N",
    "Therapy + AI subgroup N",
    "Unique weight values",
    "Minimum weight",
    "Maximum weight",
    "Status"
  ),
  
  value = as.character(
    c(
      
      nrow(
        descriptive_dat
      ),
      
      nrow(
        ai_users_dat
      ),
      
      nrow(
        therapy_ai_dat
      ),
      
      n_distinct(
        descriptive_dat$analysis_weight
      ),
      
      min(
        descriptive_dat$analysis_weight,
        na.rm = TRUE
      ),
      
      max(
        descriptive_dat$analysis_weight,
        na.rm = TRUE
      ),
      
      if (
        n_distinct(
          descriptive_dat$analysis_weight
        ) == 1 &&
        unique(
          descriptive_dat$analysis_weight
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
# 27. Reproducibility
# ------------------------------------------------------------

run_info <- tibble(
  
  item = c(
    "R version",
    "survey version",
    "dplyr version",
    "purrr version",
    "writexl version"
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
        "purrr"
      )
    ),
    
    as.character(
      packageVersion(
        "writexl"
      )
    )
  )
)


# ============================================================
# EXPORT
# ============================================================


# ------------------------------------------------------------
# 28. Export SAFE aggregate output
# ------------------------------------------------------------

output_file <- file.path(
  safe_outputs_dir,
  "SAFE_09_AI_experience_descriptives.xlsx"
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
    
    Analysis_Summary =
      analysis_summary,
    
    Item_Dictionary =
      item_dictionary,
    
    Denominator_QA =
      denominator_QA,
    
    Helpfulness_Distribution =
      helpfulness_distribution,
    
    Helpfulness_Summary =
      helpfulness_summary,
    
    Helpfulness_Collapsed =
      helpfulness_collapsed,
    
    Experiences_Q04 =
      experiences_results,
    
    Topics_Q05 =
      topics_results,
    
    Therapy_vs_AI_5cat =
      therapy_comparison_5cat,
    
    Therapy_vs_AI_3cat =
      therapy_comparison_3cat,
    
    Therapy_Sharing_4cat =
      therapy_sharing_4cat,
    
    Therapy_Sharing_Summary =
      therapy_sharing_summary,
    
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
  "AI experience descriptives complete. SAFE output: ",
  project_relative(output_file)
)
