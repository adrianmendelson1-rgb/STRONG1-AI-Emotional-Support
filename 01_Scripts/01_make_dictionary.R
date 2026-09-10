# ============================================================
# 01_make_dictionary.R
# STRONG-1 DATA DICTIONARY
#
# Purpose:
# Create metadata needed for analysis planning WITHOUT
# exporting participant-level data.
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
# 6. The VALUES variable is numeric-coded
# 7. It contains <= 20 unique values
# 8. The LABELS variable also contains <= 20 unique values
# 9. It is not an identifier
#
# This prevents free-text responses from being exported.
get_category_mapping <- function(var_name) {
  
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
# 10. Create variable dictionary
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
      
      category_mapping_available =
        !is.null(get_category_mapping(var_name))
    )
  }
)


# ------------------------------------------------------------
# 11. Create the categorical codebook
# ------------------------------------------------------------

category_mapping <- map_dfr(
  names(dat_values),
  get_category_mapping
)

if (nrow(category_mapping) == 0) {
  
  category_mapping <- tibble(
    variable = character(),
    code = numeric(),
    label = character()
  )
}


# ------------------------------------------------------------
# 12. Add category summary to dictionary
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
# 13. Prepare Legend metadata
# ------------------------------------------------------------

# Legend contains study metadata rather than participant data.
# We export it as-is so variable names, measure names,
# Question/Score status, and notes can be used for mapping.

legend_table <- legend


# ------------------------------------------------------------
# 14. Workbook summary
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
# 15. Sheet mismatch report
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
# 16. Export the dictionary workbook
# ------------------------------------------------------------

output_file <- file.path(
  analysis_outputs_dir,
  "Dictionary_STRONG1.xlsx"
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
    
    Legend = legend_table,
    
    Sheet_Mismatches = sheet_mismatch
  ),
  
  path = output_file
)


# ------------------------------------------------------------
# 17. Completion
#
# No participant rows and no free-text responses are exported.
# ------------------------------------------------------------

message(
  "Data dictionary written to ",
  project_relative(output_file)
)
