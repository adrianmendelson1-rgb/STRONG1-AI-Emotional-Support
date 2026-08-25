# ============================================================
# 02_analysis_mapping.R
# STRONG-1 ANALYSIS VARIABLE MAPPING
#
# Purpose:
# Record the exact response coding of the categorical variables
# used in the analysis, so that every recode in Script 04 can be
# checked against the coding actually present in the source data.
#
# Exports aggregate category counts only.
# ============================================================

source_files <- Filter(Negate(is.null), lapply(sys.frames(), `[[`, "ofile"))
file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(source_files) > 0) {
  script_file <- normalizePath(tail(source_files, 1)[[1]])
} else if (length(file_argument) > 0) {
  script_file <- normalizePath(sub("^--file=", "", tail(file_argument, 1)))
} else {
  stop("Could not determine the location of 02_analysis_mapping.R.")
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
  "writexl"
)

ensure_packages(packages)

library(readxl)
library(dplyr)
library(purrr)
library(writexl)

file_path <- data_file

values <- read_excel(
  file_path,
  sheet = "T1_Values_200826"
)

labels <- read_excel(
  file_path,
  sheet = "T1_Labels_200826"
)

legend <- read_excel(
  file_path,
  sheet = "Legend"
)

# Variables for which we need exact coding
target_vars <- c(
  "UserLanguage",
  "Gender",
  "Education",
  "Income",
  "Religiosity_Q01",
  "Religiosity_Q02",
  "MH_Treatment_Q01",
  "MH_Treatment_Q02_R01",
  "MH_Treatment_Q02_R02",
  "MH_Treatment_Q02_R03",
  "MH_Treatment_Q02_R04",
  "MH_Treatment_Q02_R05",
  "MH_Treatment_Q02_R06",
  "MH_Treatment_Q02_R07",
  "MH_Treatment_Q02_R08",
  "MH_AI_Q01",
  "MH_AI_Q02",
  "MH_AI_Q03",
  "MH_AI_Q06",
  "MH_AI_Q07"
)

# Keep only variables that actually exist in both sheets
target_vars <- target_vars[
  target_vars %in% names(values) &
    target_vars %in% names(labels)
]

# Create aggregate code-label tables only
mapping <- map_dfr(target_vars, function(v) {
  
  tibble(
    variable = v,
    code = as.character(values[[v]]),
    label = as.character(labels[[v]])
  )%>%
    filter(!is.na(code)) %>%
    count(variable, code, label, name = "n") %>%
    group_by(variable) %>%
    mutate(
      pct = round(100 * n / sum(n), 2)
    ) %>%
    ungroup()
})

# Summary of missingness / available responses
summary <- map_dfr(target_vars, function(v) {
  
  tibble(
    variable = v,
    n_total = nrow(values),
    n_answered = sum(!is.na(values[[v]])),
    n_missing = sum(is.na(values[[v]])),
    n_unique = n_distinct(values[[v]], na.rm = TRUE)
  )
})

# Model coding and reference categories used by 04_prepare_analysis_data.R.
# Source labels are taken directly from the supplied Labels sheet.
source_labels_by_code <- mapping %>%
  filter(
    variable %in% c(
      "Gender",
      "Education",
      "Income",
      "Religiosity_Q01"
    )
  ) %>%
  group_by(variable, code) %>%
  summarise(
    source_label = paste(sort(unique(label)), collapse = " | "),
    .groups = "drop"
  )

model_coding <- bind_rows(
  tibble(
    model_variable = "age",
    source_variable = "Age",
    source_code = NA_character_,
    analysis_category = "Continuous (years)",
    reference_category = NA_character_
  ),
  tibble(
    model_variable = "gender_model",
    source_variable = "Gender",
    source_code = as.character(1:3),
    analysis_category = c("Female", "Male", "Other"),
    reference_category = "Female"
  ),
  tibble(
    model_variable = "education",
    source_variable = "Education",
    source_code = as.character(1:5),
    analysis_category = c(
      "Primary",
      "Secondary",
      "Post-secondary non-academic",
      "Bachelor's degree",
      "Master's degree or higher"
    ),
    reference_category = "Secondary"
  ),
  tibble(
    model_variable = "income",
    source_variable = "Income",
    source_code = as.character(1:5),
    analysis_category = c(
      "Much below average",
      "Below average",
      "About average",
      "Above average",
      "Much above average"
    ),
    reference_category = "About average"
  ),
  tibble(
    model_variable = "religion_binary",
    source_variable = "Religiosity_Q01",
    source_code = as.character(1:6),
    analysis_category = c("Jewish", rep("Non-Jewish", 5)),
    reference_category = "Jewish"
  )
) %>%
  left_join(
    source_labels_by_code,
    by = c(
      "source_variable" = "variable",
      "source_code" = "code"
    )
  ) %>%
  select(
    model_variable,
    source_variable,
    source_code,
    source_label,
    analysis_category,
    reference_category
  )

age_question <- legend %>%
  filter(`Item Name` == "Age") %>%
  pull(`Question Phrasing / Item Meaning`) %>%
  first()

model_coding$source_label[
  model_coding$source_variable == "Age"
] <- age_question

treatment_question <- legend %>%
  filter(`Item Name` == "MH_Treatment_Q01") %>%
  pull(`Question Phrasing / Item Meaning`) %>%
  first()

if (length(treatment_question) == 0 || is.na(treatment_question)) {
  stop("MH_Treatment_Q01 wording was not found in the Legend sheet.")
}

treatment_item_mapping <- mapping %>%
  filter(variable == "MH_Treatment_Q01") %>%
  transmute(
    source_variable = variable,
    exact_question_wording = treatment_question,
    source_code = code,
    exact_source_label = label,
    n,
    note = "Exact supplied wording/labels; no broader or narrower interpretation inferred."
  )

output_file <- file.path(
  safe_outputs_dir,
  "SAFE_analysis_mapping.xlsx"
)

if (file.exists(output_file)) {
  file.remove(output_file)
}

write_xlsx(
  list(
    Code_Labels = mapping,
    Variable_Summary = summary,
    Model_Coding = model_coding,
    Treatment_Item = treatment_item_mapping
  ),
  output_file
)

message(
  "Analysis mapping written to ",
  project_relative(output_file)
)
