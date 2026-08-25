# ============================================================
# 01_make_dictionary.R
# STRONG-1 SAFE DATA DICTIONARY
#
# Purpose:
# Create metadata needed for analysis planning WITHOUT
# exporting participant-level data.
# ============================================================

source_files <- Filter(Negate(is.null), lapply(sys.frames(), `[[`, "ofile"))
file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(source_files) > 0) {
  script_file <- normalizePath(tail(source_files, 1)[[1]])
} else if (length(file_argument) > 0) {
  script_file <- normalizePath(sub("^--file=", "", tail(file_argument, 1)))
} else {
  stop("Could not determine the location of 01_make_dictionary.R.")
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
  "stringr",
  "tibble",
  "writexl"
)

ensure_packages(packages)

library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(writexl)


# ------------------------------------------------------------
# 1. Dataset location
# ------------------------------------------------------------

file_path <- data_file

if (!file.exists(file_path)) {
  stop(
    paste(
      "STRONG-1 data file not found at",
      project_relative(file_path),
      "- see README.md for where to place it."
    )
  )
}


# ------------------------------------------------------------
# 2. Sheet names
# ------------------------------------------------------------

sheet_values    <- "T1_Values_200826"
sheet_labels    <- "T1_Labels_200826"
sheet_legend    <- "Legend"
sheet_filtering <- "T1_Filtering_200826"


# ------------------------------------------------------------
# 3. Read workbook
# ------------------------------------------------------------

# Participant data with numeric/raw coding
dat_values <- read_excel(
  file_path,
  sheet = sheet_values
)

# Participant data with value labels
# IMPORTANT: this object stays local and is NEVER exported directly
dat_labels <- read_excel(
  file_path,
  sheet = sheet_labels
)

# Metadata / codebook
legend <- read_excel(
  file_path,
  sheet = sheet_legend
)


# ------------------------------------------------------------
# 4. Basic structural checks
# ------------------------------------------------------------

if (nrow(dat_values) != nrow(dat_labels)) {
  warning(
    "Values and Labels sheets contain different numbers of rows."
  )
}

common_variables <- intersect(
  names(dat_values),
  names(dat_labels)
)

values_only_variables <- setdiff(
  names(dat_values),
  names(dat_labels)
)

labels_only_variables <- setdiff(
  names(dat_labels),
  names(dat_values)
)


# ------------------------------------------------------------
# 5. Helper functions
# ------------------------------------------------------------

# Identify variables that might contain IDs or identifiers.
# Their values will NEVER be exported.
likely_identifier <- function(var_name) {
  
  x <- tolower(var_name)
  
  str_detect(
    x,
    "(^id$|_id$|^id_|participant|respondent|email|phone|name|identity)"
  )
}


# Check whether a vector looks numeric/integer-coded
is_numeric_coded <- function(x) {
  
  if (is.numeric(x) || is.integer(x)) {
    return(TRUE)
  }
  
  FALSE
}


# Safely obtain a code -> label mapping.
#
# We only expose labels when:
# 1. The VALUES variable is numeric-coded
# 2. It contains <= 20 unique values
# 3. The LABELS variable also contains <= 20 unique values
# 4. It is not an identifier
#
# This prevents free-text responses from being exported.
get_safe_mapping <- function(var_name) {
  
  if (!(var_name %in% common_variables)) {
    return(NULL)
  }
  
  if (likely_identifier(var_name)) {
    return(NULL)
  }
  
  x_value <- dat_values[[var_name]]
  x_label <- dat_labels[[var_name]]
  
  if (!is_numeric_coded(x_value)) {
    return(NULL)
  }
  
  n_value_unique <- n_distinct(x_value, na.rm = TRUE)
  n_label_unique <- n_distinct(x_label, na.rm = TRUE)
  
  if (
    n_value_unique == 0 ||
    n_value_unique > 20 ||
    n_label_unique == 0 ||
    n_label_unique > 20
  ) {
    return(NULL)
  }
  
  mapping <- tibble(
    variable = var_name,
    code = x_value,
    label = as.character(x_label)
  ) %>%
    filter(
      !is.na(code),
      !is.na(label),
      label != ""
    ) %>%
    distinct(variable, code, label) %>%
    arrange(code)
  
  # Additional safety check:
  # each numeric code should ideally correspond to only one label
  ambiguity_check <- mapping %>%
    count(variable, code) %>%
    filter(n > 1)
  
  if (nrow(ambiguity_check) > 0) {
    return(NULL)
  }
  
  mapping
}


# ------------------------------------------------------------
# 6. Create variable dictionary
# ------------------------------------------------------------

dictionary <- map_dfr(
  names(dat_values),
  
  function(var_name) {
    
    x <- dat_values[[var_name]]
    
    tibble(
      variable = var_name,
      
      r_class = paste(
        class(x),
        collapse = " / "
      ),
      
      n_missing = sum(is.na(x)),
      
      pct_missing = round(
        mean(is.na(x)) * 100,
        2
      ),
      
      n_unique = n_distinct(
        x,
        na.rm = TRUE
      ),
      
      numeric_coded = is_numeric_coded(x),
      
      likely_identifier = likely_identifier(var_name),
      
      exists_in_labels_sheet =
        var_name %in% names(dat_labels),
      
      safe_category_mapping_available =
        !is.null(get_safe_mapping(var_name))
    )
  }
)


# ------------------------------------------------------------
# 7. Create safe categorical codebook
# ------------------------------------------------------------

category_mapping <- map_dfr(
  names(dat_values),
  get_safe_mapping
)

if (nrow(category_mapping) == 0) {
  
  category_mapping <- tibble(
    variable = character(),
    code = numeric(),
    label = character()
  )
}


# ------------------------------------------------------------
# 8. Add category summary to dictionary
# ------------------------------------------------------------

if (nrow(category_mapping) > 0) {
  
  category_summary <- category_mapping %>%
    mutate(
      code_label = paste0(
        code,
        " = ",
        label
      )
    ) %>%
    group_by(variable) %>%
    summarise(
      category_labels =
        paste(
          code_label,
          collapse = " | "
        ),
      .groups = "drop"
    )
  
  dictionary <- dictionary %>%
    left_join(
      category_summary,
      by = "variable"
    )
  
} else {
  
  dictionary$category_labels <- NA_character_
}


# ------------------------------------------------------------
# 9. Prepare Legend metadata
# ------------------------------------------------------------

# Legend contains study metadata rather than participant data.
# We export it as-is so variable names, measure names,
# Question/Score status, and notes can be used for mapping.

legend_safe <- legend


# ------------------------------------------------------------
# 10. Workbook summary
# ------------------------------------------------------------

workbook_summary <- tibble(
  item = c(
    "Workbook",
    "Values sheet",
    "Labels sheet",
    "Legend sheet",
    "Filtering sheet",
    "Rows in Values",
    "Columns in Values",
    "Rows in Labels",
    "Columns in Labels",
    "Variables common to Values and Labels",
    "Variables only in Values",
    "Variables only in Labels"
  ),
  
  value = c(
    basename(file_path),
    sheet_values,
    sheet_labels,
    sheet_legend,
    sheet_filtering,
    as.character(nrow(dat_values)),
    as.character(ncol(dat_values)),
    as.character(nrow(dat_labels)),
    as.character(ncol(dat_labels)),
    as.character(length(common_variables)),
    as.character(length(values_only_variables)),
    as.character(length(labels_only_variables))
  )
)


# ------------------------------------------------------------
# 11. Sheet mismatch report
# ------------------------------------------------------------

sheet_mismatch <- bind_rows(
  
  tibble(
    variable = values_only_variables,
    location = "Values only"
  ),
  
  tibble(
    variable = labels_only_variables,
    location = "Labels only"
  )
)


if (nrow(sheet_mismatch) == 0) {
  
  sheet_mismatch <- tibble(
    variable = NA_character_,
    location = "No mismatches"
  )
}


# ------------------------------------------------------------
# 12. Export SAFE workbook
# ------------------------------------------------------------

output_file <- file.path(
  safe_outputs_dir,
  "SAFE_STRONG1_dictionary.xlsx"
)

# Delete previous output if it already exists
if (file.exists(output_file)) {
  file.remove(output_file)
}

write_xlsx(
  
  list(
    
    Workbook_Summary = workbook_summary,
    
    Variable_Dictionary = dictionary,
    
    Category_Mapping = category_mapping,
    
    Legend = legend_safe,
    
    Sheet_Mismatches = sheet_mismatch
  ),
  
  path = output_file
)


# ------------------------------------------------------------
# 13. Completion
#
# No participant rows and no free-text responses are exported.
# ------------------------------------------------------------

message(
  "SAFE dictionary written to ",
  project_relative(output_file)
)
