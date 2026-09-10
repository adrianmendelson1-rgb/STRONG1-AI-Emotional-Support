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
# S2 = Robustness / sensitivity summary
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
  "tibble",
  "readxl",
  "writexl",
  "ggplot2",
  "patchwork",
  "officer",
  "flextable",
  "tidyr"
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
library(officer)
library(flextable)
library(tidyr)


# ------------------------------------------------------------
# Summary helpers
#
# Conventional unweighted sample statistics. sample_quantile()
# is R's default (type 7) quantile, the same definition
# splines::ns() uses internally to place its interior knots, so
# prediction grids and knots sit on one consistent scale.
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
# 0b. Publication-output directories
# ------------------------------------------------------------

main_tables_dir <- file.path(
  main_outputs_dir,
  "tables"
)

main_figures_dir <- file.path(
  main_outputs_dir,
  "figures"
)

main_figure_previews_dir <- file.path(
  main_figures_dir,
  "previews"
)

main_source_data_dir <- file.path(
  main_outputs_dir,
  "source_data"
)

supplement_tables_dir <- file.path(
  supplement_outputs_dir,
  "tables"
)

supplement_source_data_dir <- file.path(
  supplement_outputs_dir,
  "source_data"
)

for (output_directory in c(
  main_tables_dir,
  main_figures_dir,
  main_figure_previews_dir,
  main_source_data_dir,
  supplement_tables_dir,
  supplement_source_data_dir
)) {
  dir.create(
    output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


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


# Shared wording for published tables. All estimates are unweighted.
publication_labels <- list(
  percentage = "Percentage",
  percentage_ci = "Percentage (95% CI)",
  percentage_ci_short = "% (95% CI)",
  continuous_summary = "mean (SD)",
  categorical_summary = "n (%)"
)


# Every workbook read below is an aggregate output produced
# by Scripts 06-10. Define this only after Script 06 finishes so
# its source-data import continues to use readxl::read_excel.
read_analysis_excel <- function(path, ...) {
  readxl::read_excel(
    file.path(analysis_outputs_dir, path),
    ...
  )
}


# ------------------------------------------------------------
# 2. Required analysis output files
# ------------------------------------------------------------

# Each model workbook must be newer than the script that writes it.
# This catches the common failure of editing an analysis script and then
# building publication outputs from its previous results.
stale_model_outputs <- function() {

  pairs <- list(
    c("06_primary_models.R", "Analysis_06_primary_models.xlsx"),
    c("07_general_AI_users_model.R", "Analysis_07_general_AI_users_model.xlsx"),
    c("09_AI_experience_descriptives.R", "Analysis_09_AI_experience_descriptives.xlsx"),
    c("10_robustness_sensitivity.R", "Analysis_10_robustness_sensitivity.xlsx")
  )

  stale <- vapply(
    pairs,
    function(pair) {

      script_path <- file.path(scripts_dir, pair[1])
      output_path <- file.path(analysis_outputs_dir, pair[2])

      file.exists(output_path) &&
        file.mtime(output_path) < file.mtime(script_path)
    },
    logical(1)
  )

  vapply(pairs[stale], `[`, character(1), 2)
}


required_files <- file.path(
  analysis_outputs_dir,
  c(
    "Analysis_06_primary_models.xlsx",
    "Analysis_07_general_AI_users_model.xlsx",
    "Analysis_09_AI_experience_descriptives.xlsx",
    "Analysis_10_robustness_sensitivity.xlsx"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  
  stop(
    paste(
      "Missing analysis result file(s):",
      paste(
        project_relative(missing_files),
        collapse = ", "
      )
    )
  )
}


outdated_outputs <- stale_model_outputs()

if (length(outdated_outputs) > 0) {
  stop(
    paste0(
      "These analysis outputs are older than the script that writes them: ",
      paste(outdated_outputs, collapse = ", "),
      ". Re-run the pipeline in order before building publication outputs."
    )
  )
}


# ============================================================
# HELPERS
# ============================================================


format_mean_sd <- function(
    mean_value,
    sd_value,
    digits = 2
) {
  
  # A standardized mean is mathematically zero but can carry a tiny
  # negative floating-point residual, which formatC() renders as
  # "-0.00". Collapse any value that rounds to zero onto exact zero so
  # the table is stable across platforms and BLAS builds.
  mean_value <- ifelse(
    round(mean_value, digits) == 0,
    0,
    mean_value
  )
  
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


# Table S2 Panel A reports a bound that rounds to 1.00 at two decimals
# while its p value exceeds .05. Three decimals keep the interval and the
# p value visibly consistent.
format_or_ci_3 <- function(
    OR,
    lower,
    upper
) {
  
  paste0(
    formatC(
      OR,
      digits = 3,
      format = "f"
    ),
    " [",
    formatC(
      lower,
      digits = 3,
      format = "f"
    ),
    ", ",
    formatC(
      upper,
      digits = 3,
      format = "f"
    ),
    "]"
  )
}


format_p <- function(p) {
  
  if (is.na(p)) {
    return(NA_character_)
  }

  if (p < .001) {
    return("< .001")
  }

  sub(
    "^0",
    "",
    formatC(
      p,
      digits = 3,
      format = "f"
    )
  )
}


format_pct_ci <- function(
    estimate,
    lower,
    upper
) {

  paste0(
    formatC(
      estimate,
      digits = 1,
      format = "f"
    ),
    " [",
    formatC(
      lower,
      digits = 1,
      format = "f"
    ),
    ", ",
    formatC(
      upper,
      digits = 1,
      format = "f"
    ),
    "]"
  )
}


# ------------------------------------------------------------
# Publication-table styling and Word export
# ------------------------------------------------------------

publication_flextable <- function(
    data,
    widths,
    alignments,
    font_size = 9.5
) {

  ft <- flextable(data)
  ft <- theme_booktabs(ft)
  ft <- font(
    ft,
    fontname = "Times New Roman",
    part = "all"
  )
  ft <- fontsize(
    ft,
    size = font_size,
    part = "all"
  )
  ft <- bold(
    ft,
    part = "header"
  )
  ft <- align(
    ft,
    align = "center",
    part = "header"
  )

  for (column_index in seq_along(alignments)) {
    ft <- align(
      ft,
      j = column_index,
      align = alignments[[column_index]],
      part = "body"
    )
  }

  ft <- valign(
    ft,
    valign = "center",
    part = "all"
  )
  ft <- padding(
    ft,
    padding.top = 3.5,
    padding.bottom = 3.5,
    padding.left = 5,
    padding.right = 5,
    part = "all"
  )
  ft <- line_spacing(
    ft,
    space = 1,
    part = "all"
  )
  ft <- set_table_properties(
    ft,
    layout = "fixed",
    width = 1,
    align = "center"
  )
  ft <- width(
    ft,
    j = seq_along(widths),
    width = widths
  )

  ft
}


style_section_rows <- function(
    ft,
    section_rows,
    n_columns
) {

  if (length(section_rows) == 0) {
    return(ft)
  }

  for (row_index in section_rows) {
    ft <- merge_at(
      ft,
      i = row_index,
      j = seq_len(n_columns),
      part = "body"
    )
  }

  ft <- bg(
    ft,
    i = section_rows,
    bg = "#E9EFF5",
    part = "body"
  )
  ft <- bold(
    ft,
    i = section_rows,
    bold = TRUE,
    part = "body"
  )
  ft <- align(
    ft,
    i = section_rows,
    j = 1,
    align = "left",
    part = "body"
  )

  ft
}


publication_section <- function(
    landscape = FALSE
) {

  prop_section(
    page_size = page_size(
      orient = ifelse(
        landscape,
        "landscape",
        "portrait"
      )
    ),
    page_margins = page_mar(
      top = ifelse(
        landscape,
        0.42,
        0.65
      ),
      bottom = ifelse(
        landscape,
        0.42,
        0.65
      ),
      left = ifelse(
        landscape,
        0.55,
        0.7
      ),
      right = ifelse(
        landscape,
        0.55,
        0.7
      )
    ),
    type = "continuous"
  )
}


add_table_heading <- function(
    doc,
    table_number,
    table_title
) {

  doc <- body_add_fpar(
    doc,
    fpar(
      ftext(
        table_number,
        fp_text(
          font.family = "Times New Roman",
          font.size = 11,
          bold = TRUE
        )
      ),
      fp_p = fp_par(
        padding.bottom = 2
      )
    )
  )

  body_add_fpar(
    doc,
    fpar(
      ftext(
        table_title,
        fp_text(
          font.family = "Times New Roman",
          font.size = 11,
          italic = TRUE
        )
      ),
      fp_p = fp_par(
        padding.bottom = 5
      )
    )
  )
}


add_table_note <- function(
    doc,
    note
) {

  body_add_fpar(
    doc,
    fpar(
      ftext(
        "Note. ",
        fp_text(
          font.family = "Times New Roman",
          font.size = 9,
          italic = TRUE
        )
      ),
      ftext(
        note,
        fp_text(
          font.family = "Times New Roman",
          font.size = 9
        )
      ),
      fp_p = fp_par(
        padding.top = 5,
        line_spacing = 1
      )
    )
  )
}


export_publication_table <- function(
    ft,
    table_number,
    table_title,
    note,
    path,
    landscape = FALSE
) {

  doc <- read_docx()
  doc <- body_set_default_section(
    doc,
    value = publication_section(landscape)
  )
  doc <- add_table_heading(
    doc,
    table_number,
    table_title
  )
  doc <- body_add_flextable(
    doc,
    value = ft,
    align = "center"
  )
  doc <- add_table_note(
    doc,
    note
  )

  invisible(
    print(
      doc,
      target = path
    )
  )

  invisible(path)
}


# ------------------------------------------------------------
# Delta-method marginal probability and 95% CI
# ------------------------------------------------------------

marginal_probability_ci_11 <- function(
    model,
    newdata
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
    rep(
      1 / nrow(X),
      nrow(X)
    )
  
  
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

clean_near_zero <- function(x, digits = 2) {
  if (!is.na(x) && abs(x) < 0.5 * 10^(-digits)) {
    return(0)
  }
  x
}

continuous_row <- function(
    variable,
    label,
    digits = 2
) {
  
  tibble(
    Characteristic = label,
    
    Overall =
      format_mean_sd(
        clean_near_zero(
          mean_na(
            overall_dat[[variable]]
          ),
          digits
        ),
        sd_na(
          overall_dat[[variable]]
        ),
        digits
      ),
    
    No_AI_emotional_support =
      format_mean_sd(
        clean_near_zero(
          mean_na(
            nonuser_dat[[variable]]
          ),
          digits
        ),
        sd_na(
          nonuser_dat[[variable]]
        ),
        digits
      ),
    
    AI_emotional_support =
      format_mean_sd(
        clean_near_zero(
          mean_na(
            user_dat[[variable]]
          ),
          digits
        ),
        sd_na(
          user_dat[[variable]]
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
              mean_na(
                overall_indicator
              )
          ),
        
        No_AI_emotional_support =
          format_n_pct(
            sum(
              nonuser_indicator,
              na.rm = TRUE
            ),
            100 *
              mean_na(
                nonuser_indicator
              )
          ),
        
        AI_emotional_support =
          format_n_pct(
            sum(
              user_indicator,
              na.rm = TRUE
            ),
            100 *
              mean_na(
                user_indicator
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
          mean_na(
            overall_indicator
          )
      ),
    
    No_AI_emotional_support =
      format_n_pct(
        sum(
          nonuser_indicator,
          na.rm = TRUE
        ),
        100 *
          mean_na(
            nonuser_indicator
          )
      ),
    
    AI_emotional_support =
      format_n_pct(
        sum(
          user_indicator,
          na.rm = TRUE
        ),
        100 *
          mean_na(
            user_indicator
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

section_row <- function(label) {

  tibble(
    row_type = "section",
    Characteristic = label,
    Overall = "",
    No_AI_emotional_support = "",
    AI_emotional_support = ""
  )
}


regular_row <- function(data) {

  data %>%
    mutate(
      row_type = "regular",
      .before = 1
    )
}


ordered_category_rows <- function(
    variable,
    variable_label,
    category_order
) {

  category_data <- categorical_rows(
    variable,
    variable_label
  ) %>%
    mutate(
      category = sub(
        paste0(
          "^",
          variable_label,
          ": "
        ),
        "",
        Characteristic
      )
    )

  missing_categories <- setdiff(
    category_order,
    category_data$category
  )

  if (length(missing_categories) > 0) {
    stop(
      paste0(
        "Table 1 is missing expected ",
        variable_label,
        " categories: ",
        paste(
          missing_categories,
          collapse = ", "
        )
      )
    )
  }

  category_data %>%
    slice(
      match(
        category_order,
        category
      )
    ) %>%
    transmute(
      row_type = "category",
      Characteristic = category,
      Overall,
      No_AI_emotional_support,
      AI_emotional_support
    )
}


table1_publication <- bind_rows(
  regular_row(
    continuous_row(
      "age",
      "Age, years",
      digits = 2
    )
  ),
  section_row("Gender"),
  ordered_category_rows(
    "gender_full",
    "Gender",
    c(
      "Female",
      "Male",
      "Other"
    )
  ),
  section_row("Education"),
  ordered_category_rows(
    "education",
    "Education",
    c(
      "Primary",
      "Secondary",
      "Post-secondary non-academic",
      "Bachelor's degree",
      "Master's degree or higher"
    )
  ),
  section_row("Income"),
  ordered_category_rows(
    "income",
    "Income",
    c(
      "Much below average",
      "Below average",
      "About average",
      "Above average",
      "Much above average"
    )
  ),
  section_row("Religious affiliation"),
  ordered_category_rows(
    "religion_full",
    "Religious affiliation",
    c(
      "Jewish",
      "Muslim",
      "Christian",
      "Druze",
      "Atheist / no religion",
      "Other"
    )
  ),
  regular_row(
    continuous_row(
      "distress_z",
      "Psychological distress, z score",
      digits = 2
    )
  ),
  regular_row(
    continuous_row(
      "loneliness",
      "Loneliness, UCLA-3 score",
      digits = 2
    )
  ),
  regular_row(
    continuous_row(
      "social_support",
      "Perceived social support, 0-100",
      digits = 2
    )
  ),
  regular_row(
    binary_row(
      "general_ai_user",
      1,
      "Any general AI use"
    )
  )
)


table1 <- table1_publication %>%
  select(
    -row_type
  )


table1_display <- table1_publication %>%
  select(
    -row_type
  )

table1_ft <- publication_flextable(
  table1_display,
  widths = c(
    2.55,
    1.85,
    2.55,
    2.45
  ),
  alignments = c(
    "left",
    "center",
    "center",
    "center"
  ),
  font_size = 9
)

table1_ft <- padding(
  table1_ft,
  padding.top = 2.5,
  padding.bottom = 2.5,
  part = "all"
)

table1_ft <- set_header_labels(
  table1_ft,
  Characteristic = "Characteristic",
  Overall = paste0(
    "Overall (N = ",
    formatC(
      N_total,
      format = "d",
      big.mark = ","
    ),
    ")"
  ),
  No_AI_emotional_support = paste0(
    "No AI emotional-support use (n = ",
    formatC(
      N_nonusers,
      format = "d",
      big.mark = ","
    ),
    ")"
  ),
  AI_emotional_support = paste0(
    "AI emotional-support use (n = ",
    formatC(
      N_users,
      format = "d",
      big.mark = ","
    ),
    ")"
  )
)

table1_section_rows <- which(
  table1_publication$row_type == "section"
)

table1_category_rows <- which(
  table1_publication$row_type == "category"
)

table1_ft <- style_section_rows(
  table1_ft,
  table1_section_rows,
  n_columns = 4
)

table1_ft <- padding(
  table1_ft,
  i = table1_category_rows,
  j = 1,
  padding.left = 18,
  part = "body"
)


# ============================================================
# TABLE 2
# KEY MODEL RESULTS
# ============================================================


# ------------------------------------------------------------
# 7b. Analytic sample sizes for Table 2
#
# Derived from the fitted primary model and from the stored
# outputs, so the Table 2 labels can never drift away from the
# analysis that actually ran.
# ------------------------------------------------------------

read_analysis_n <- function(file, metric) {

  read_analysis_excel(
    file,
    sheet = "Diagnostics"
  ) %>%
    filter(metric == !!metric) %>%
    pull(value) %>%
    as.numeric()
}


primary_n <- nrow(primary_dat)

primary_n_reported <- read_analysis_n(
  "Analysis_06_primary_models.xlsx",
  "Primary analytic N"
)

general_n <- read_analysis_n(
  "Analysis_07_general_AI_users_model.xlsx",
  "General-AI-user analytic N"
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
      ") and Analysis_06 (",
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
# 8. Wald-test parsing
#
# regTermTest() prints the F statistic and its two degrees of
# freedom inside a single text line. Table 2 reports those, so
# they are parsed back out of the stored test output rather than
# retyped, which keeps the table tied to the model that ran.
# ------------------------------------------------------------

parse_wald <- function(test_output) {

  numbers <- regmatches(
    test_output,
    regexpr(
      "F = +[0-9.eE+-]+ +on +[0-9]+ +and +[0-9]+ +df",
      test_output
    )
  )

  if (length(numbers) == 0) {
    return(
      list(
        F = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_
      )
    )
  }

  parts <- as.numeric(
    regmatches(
      numbers,
      gregexpr(
        "[0-9.eE+-]+",
        numbers
      )
    )[[1]]
  )

  list(
    F = parts[1],
    df1 = parts[2],
    df2 = parts[3]
  )
}


format_wald <- function(test_output) {

  parsed <- parse_wald(test_output)

  if (is.na(parsed$F)) {
    return(NA_character_)
  }

  paste0(
    formatC(
      parsed$F,
      digits = 2,
      format = "f"
    ),
    " (",
    parsed$df1,
    ", ",
    parsed$df2,
    ")"
  )
}


# ------------------------------------------------------------
# 9. Table 2 rows for one model
#
# Each focal predictor enters the final model as a set of
# natural-cubic-spline terms, so the association is summarised
# by a global Wald test over all of that predictor's terms.
# The departure-from-linearity test is reported separately, and
# spline coefficients are not tabulated because they carry no
# direct interpretation.
# ------------------------------------------------------------

EM_DASH <- "\u2014"


PREDICTOR_ORDER <- c(
  "Psychological distress",
  "Loneliness",
  "Perceived social support"
)


build_table2_panel <- function(
    file,
    global_labels,
    joint_label
) {

  global_tests <- read_analysis_excel(
    file,
    sheet = "Flexible_Global_Tests"
  )

  form_tests <- read_analysis_excel(
    file,
    sheet = "Functional_Form_Tests"
  )

  global_rows <- map_dfr(
    PREDICTOR_ORDER,
    function(predictor) {

      global_row <- global_tests %>%
        filter(
          test == global_labels[[predictor]]
        )

      form_row <- form_tests %>%
        filter(
          predictor == !!predictor
        )

      tibble(
        Predictor = predictor,

        `Overall association F (df)` =
          format_wald(global_row$output[1]),

        p =
          format_p(global_row$p_value[1]),

        `Nonlinearity F (df)` =
          format_wald(form_row$test_output[1]),

        `p ` =
          format_p(form_row$p_value[1]),

        `OR (95% CI)` = EM_DASH
      )
    }
  )

  joint_row <- global_tests %>%
    filter(
      test == joint_label
    )

  bind_rows(
    global_rows,
    tibble(
      Predictor =
        "Loneliness and perceived social support jointly",

      `Overall association F (df)` =
        format_wald(joint_row$output[1]),

      p =
        format_p(joint_row$p_value[1]),

      `Nonlinearity F (df)` = EM_DASH,

      `p ` = EM_DASH,

      `OR (95% CI)` = EM_DASH
    )
  )
}


primary_table2 <- build_table2_panel(
  "Analysis_06_primary_models.xlsx",
  global_labels = list(
    `Psychological distress` =
      "H1: Distress - flexible final model",
    `Loneliness` =
      "H2: Loneliness - flexible final model",
    `Perceived social support` =
      "H3: Social support - flexible final model"
  ),
  joint_label = "Loneliness + social support jointly"
)


general_table2 <- build_table2_panel(
  "Analysis_07_general_AI_users_model.xlsx",
  global_labels = list(
    `Psychological distress` =
      "Psychological distress",
    `Loneliness` =
      "Loneliness",
    `Perceived social support` =
      "Perceived social support"
  ),
  joint_label = "Loneliness + social support jointly"
)


# General AI-use frequency is categorical, so it has a global
# test but no functional-form test.
general_ai_global <- read_analysis_excel(
  "Analysis_07_general_AI_users_model.xlsx",
  sheet = "Flexible_Global_Tests"
) %>%
  filter(
    test == "General AI-use frequency"
  )


# General AI-use frequency is categorical, so its category contrasts
# stay directly interpretable and are reported alongside the overall
# test. The splined focal predictors carry no such single estimate.
general_ai_category_ORs <- read_analysis_excel(
  "Analysis_07_general_AI_users_model.xlsx",
  sheet = "Flexible_General_AI_ORs"
)


general_ai_rows <- bind_rows(
  tibble(
    Predictor = "General AI-use frequency",

    `Overall association F (df)` =
      format_wald(general_ai_global$output[1]),

    p =
      format_p(general_ai_global$p_value[1]),

    `Nonlinearity F (df)` = EM_DASH,

    `p ` = EM_DASH,

    `OR (95% CI)` = EM_DASH
  ),

  general_ai_category_ORs %>%
    transmute(
      Predictor =
        paste0("   ", category),

      `Overall association F (df)` = "",

      p =
        vapply(
          p_value,
          format_p,
          character(1)
        ),

      `Nonlinearity F (df)` = "",

      `p ` = "",

      `OR (95% CI)` =
        pmap_chr(
          list(
            OR,
            CI_lower,
            CI_upper
          ),
          format_or_ci
        )
    )
)


general_table2 <- bind_rows(
  general_ai_rows,
  general_table2
)


table2_panel_row <- function(label) {

  tibble(
    row_type = "section",
    Predictor = label,
    `Overall association F (df)` = "",
    p = "",
    `Nonlinearity F (df)` = "",
    `p ` = "",
    `OR (95% CI)` = ""
  )
}


table2_publication <- bind_rows(
  table2_panel_row(
    paste0(
      "Panel A. Primary analysis - full sample (N = ",
      format_analysis_n(primary_n),
      ")"
    )
  ),
  primary_table2 %>%
    mutate(
      row_type = "regular",
      .before = 1
    ),
  table2_panel_row(
    paste0(
      "Panel B. Secondary analysis - general AI users (N = ",
      format_analysis_n(general_n),
      ")"
    )
  ),
  general_table2 %>%
    mutate(
      row_type = "regular",
      .before = 1
    )
)


table2 <- table2_publication %>%
  select(
    -row_type
  )


table2_ft <- publication_flextable(
  table2,
  widths = c(
    2.85,
    1.45,
    0.7,
    1.35,
    0.7,
    1.60
  ),
  alignments = c(
    "left",
    "center",
    "center",
    "center",
    "center",
    "center"
  ),
  font_size = 9.5
)

table2_ft <- style_section_rows(
  table2_ft,
  which(
    table2_publication$row_type == "section"
  ),
  n_columns = 6
)


# ------------------------------------------------------------
# 10. Publication notes
# ------------------------------------------------------------

table_notes <- tibble(
  
  item = c(
    "Table 1",
    "Table 2 - full sample",
    "Table 2 - secondary analysis",
    "Table 2 - interpretation"
  ),
  
  note = c(
    
    paste0(
      "Continuous variables are shown as ",
      publication_labels$continuous_summary,
      "; categorical variables as ",
      publication_labels$categorical_summary,
      ". ",
      "Denominators may vary because of item-level missingness.",
      " ",
      "No significance tests are presented."
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
      "The three focal predictors (psychological distress, loneliness,",
      "and perceived social support) were standardized and modeled as",
      "natural cubic splines (3 df for psychological distress and",
      "perceived social support; 2 df for loneliness). Age was modeled",
      "linearly. The overall association is a design-based Wald test",
      "over all spline terms for that predictor. The nonlinearity test",
      "compares a model constraining that predictor to be linear against",
      "the flexible model. Spline coefficients are not reported because",
      "they have no direct interpretation; the form and magnitude of each",
      "association are shown in Figure 1. General AI-use frequency is",
      "categorical, so its category contrasts are reported as odds ratios",
      "with 95% CIs, relative to a reference of using AI a few times."
    )
  )
)


table1_output_file <- file.path(
  main_tables_dir,
  "Table_1_Participant_Characteristics.docx"
)

table2_output_file <- file.path(
  main_tables_dir,
  "Table_2_Primary_and_Secondary_Models.docx"
)

table1_page2_start <- which(
  table1_publication$Characteristic ==
    "Religious affiliation"
)

table1_page1_data <- table1_publication[
  seq_len(
    table1_page2_start - 1
  ),
]

table1_page2_data <- table1_publication[
  table1_page2_start:nrow(table1_publication),
]

make_table1_page_flextable <- function(data) {

  page_ft <- publication_flextable(
    data %>%
      select(
        -row_type
      ),
    widths = c(
      2.55,
      1.85,
      2.55,
      2.45
    ),
    alignments = c(
      "left",
      "center",
      "center",
      "center"
    ),
    font_size = 9.25
  )

  page_ft <- set_header_labels(
    page_ft,
    Characteristic = "Characteristic",
    Overall = paste0(
      "Overall (N = ",
      formatC(
        N_total,
        format = "d",
        big.mark = ","
      ),
      ")"
    ),
    No_AI_emotional_support = paste0(
      "No AI emotional-support use (n = ",
      formatC(
        N_nonusers,
        format = "d",
        big.mark = ","
      ),
      ")"
    ),
    AI_emotional_support = paste0(
      "AI emotional-support use (n = ",
      formatC(
        N_users,
        format = "d",
        big.mark = ","
      ),
      ")"
    )
  )

  page_ft <- style_section_rows(
    page_ft,
    which(
      data$row_type == "section"
    ),
    n_columns = 4
  )

  padding(
    page_ft,
    i = which(
      data$row_type == "category"
    ),
    j = 1,
    padding.left = 18,
    part = "body"
  )
}

table1_page1_ft <- make_table1_page_flextable(
  table1_page1_data
)

table1_page2_ft <- make_table1_page_flextable(
  table1_page2_data
)

table1_doc <- read_docx()
table1_doc <- body_set_default_section(
  table1_doc,
  value = publication_section(
    landscape = TRUE
  )
)
table1_doc <- add_table_heading(
  table1_doc,
  "Table 1",
  "Participant Characteristics by AI Emotional-Support Use"
)
table1_doc <- body_add_flextable(
  table1_doc,
  value = table1_page1_ft,
  align = "center"
)
table1_doc <- body_add_break(
  table1_doc
)
table1_doc <- body_add_fpar(
  table1_doc,
  fpar(
    ftext(
      "Table 1 (continued)",
      fp_text(
        font.family = "Times New Roman",
        font.size = 10,
        bold = TRUE
      )
    ),
    fp_p = fp_par(
      padding.bottom = 4
    )
  )
)
table1_doc <- body_add_flextable(
  table1_doc,
  value = table1_page2_ft,
  align = "center"
)
table1_doc <- add_table_note(
  table1_doc,
  table_notes$note[
    table_notes$item == "Table 1"
  ]
)

invisible(
  print(
    table1_doc,
    target = table1_output_file
  )
)

export_publication_table(
  table2_ft,
  "Table 2",
  paste(
    "Associations of Psychological Distress, Loneliness, and",
    "Perceived Social Support With AI Emotional-Support Use"
  ),
  paste(
    table_notes$note[
      table_notes$item == "Table 2 - full sample"
    ],
    table_notes$note[
      table_notes$item == "Table 2 - secondary analysis"
    ],
    table_notes$note[
      table_notes$item == "Table 2 - interpretation"
    ]
  ),
  table2_output_file,
  landscape = TRUE
)


# ============================================================
# FIGURE 1
# ADJUSTED PROBABILITY CURVES
# ============================================================


# ------------------------------------------------------------
# 11. Dense prediction grids
#
# Distress and social support:
# dense grids between the 5th and 95th percentiles.
#
# Loneliness:
# predictions are evaluated at the observed UCLA-3 raw scores 3-9,
# but displayed on the corresponding standardized scale.
# ------------------------------------------------------------

distress_bounds <- sample_quantile(
  primary_dat$distress_z,
  c(
    .05,
    .95
  )
)


support_bounds <- sample_quantile(
  primary_dat$social_support_z,
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
      newdata
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
      newdata
    )
    
    
    tibble(
      predictor =
        "Loneliness",
      
      predictor_value =
        z_value,

      predictor_raw_value =
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
      newdata
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
          "Loneliness (z-score)",
        
        `Perceived social support` =
          "Perceived social support (z-score)"
      ),
    
    Predictor =
      factor(
        Predictor,
        levels = c(
          "Psychological distress (z-score)",
          "Loneliness (z-score)",
          "Perceived social support (z-score)"
        )
      )
  )


# ------------------------------------------------------------
# 16. Figure 1
# ------------------------------------------------------------

figure1_panel <- function(
    data,
    panel_label,
    panel_title,
    x_title,
    show_y_title = FALSE,
    x_breaks = waiver()
) {

  ggplot(
    data,
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
      fill = "#8DB7D8",
      alpha = 0.30
    ) +
    geom_line(
      color = "#174A73",
      linewidth = 1.05,
      lineend = "round"
    ) +
    scale_y_continuous(
      limits = c(
        0,
        55
      ),
      breaks = seq(
        0,
        50,
        by = 10
      ),
      expand = expansion(
        mult = c(
          0,
          .02
        )
      )
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      expand = expansion(
        mult = c(
          .02,
          .02
        )
      )
    ) +
    labs(
      x = x_title,
      y = ifelse(
        show_y_title,
        "Adjusted probability (%)",
        ""
      ),
      title = paste0(
        panel_label,
        "   ",
        panel_title
      )
    ) +
    theme_classic(
      base_size = 11
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11.5,
        margin = margin(
          b = 8
        )
      ),
      axis.title.x = element_text(
        size = 10.5,
        margin = margin(
          t = 7
        )
      ),
      axis.title.y = element_text(
        size = 10.5,
        margin = margin(
          r = 9
        )
      ),
      axis.text = element_text(
        color = "#263238"
      ),
      axis.line = element_line(
        color = "#56636B",
        linewidth = 0.35
      ),
      axis.ticks = element_line(
        color = "#56636B",
        linewidth = 0.35
      ),
      plot.margin = margin(
        8,
        7,
        7,
        7
      )
    )
}


figure1_A <- figure1_panel(
  figure1_distress %>%
    mutate(
      adjusted_probability_pct = 100 * adjusted_probability,
      lower_95_CI_pct = 100 * lower_95_CI,
      upper_95_CI_pct = 100 * upper_95_CI
    ),
  "A.",
  "Psychological distress",
  "z score",
  show_y_title = TRUE
)

figure1_B <- figure1_panel(
  figure1_loneliness %>%
    mutate(
      adjusted_probability_pct = 100 * adjusted_probability,
      lower_95_CI_pct = 100 * lower_95_CI,
      upper_95_CI_pct = 100 * upper_95_CI
    ),
  "B.",
  "Loneliness",
  "z score"
)

figure1_C <- figure1_panel(
  figure1_support %>%
    mutate(
      adjusted_probability_pct = 100 * adjusted_probability,
      lower_95_CI_pct = 100 * lower_95_CI,
      upper_95_CI_pct = 100 * upper_95_CI
    ),
  "C.",
  "Perceived social support",
  "z score"
)

figure1 <- figure1_A |
  figure1_B |
  figure1_C


# Use macOS Quartz for publication PDFs. Unlike the default pdf()
# device, Quartz embeds the fonts used by ggplot2, so labels remain
# visible in journal submission systems and independent PDF renderers.
# Quartz exists only on macOS, so fall back to cairo_pdf elsewhere,
# which also embeds fonts. Figures produced on macOS are unchanged.
publication_pdf_device <- function(filename, width, height, ...) {
  if (capabilities("aqua")) {
    grDevices::quartz(
      file = filename,
      type = "pdf",
      width = width,
      height = height,
      ...
    )
  } else {
    grDevices::cairo_pdf(
      filename = filename,
      width = width,
      height = height
    )
  }
}


ggsave(
  filename =
    file.path(
      main_outputs_dir,
      "figures",
      "previews",
      "Figure_1_Adjusted_Probability_Curves.png"
    ),
  
  plot =
    figure1,
  
  width =
    9.5,
  
  height =
    4.0,
  
  dpi =
    600
)


ggsave(
  filename =
    file.path(
      main_outputs_dir,
      "figures",
      "Figure_1_Adjusted_Probability_Curves.pdf"
    ),
  device =
    publication_pdf_device,
  
  plot =
    figure1,
  
  width =
    9.5,
  
  height =
    4.0
)


# ============================================================
# FIGURE 2
# AI EXPERIENCES AND PSYCHOTHERAPY CONTEXT
# ============================================================


# ------------------------------------------------------------
# 16b. Subgroup sizes for the Figure 2 panel titles
#
# Read from the n_valid column of the Script 09 outputs, so the
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

figure2_experiences <- read_analysis_excel(
  "Analysis_09_AI_experience_descriptives.xlsx",
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
    pct
  ) %>%
  mutate(
    display_label =
      factor(
        display_label,
        levels =
          display_label
      )
  )


figure2_dot_panel <- function(
    data,
    title,
    x_limit,
    plot_margin = margin(
      7,
      8,
      7,
      8
    )
) {

  ggplot(
    data,
    aes(
      x = pct,
      y = display_label
    )
  ) +
    geom_errorbar(
      aes(
        xmin = lower_95_CI_pct,
        xmax = upper_95_CI_pct
      ),
      width = 0.16,
      orientation = "y",
      color = "#6F8798",
      linewidth = 0.55
    ) +
    geom_point(
      size = 2.7,
      color = "#174A73"
    ) +
    scale_x_continuous(
      limits = c(
        0,
        x_limit
      ),
      breaks = seq(
        0,
        x_limit,
        by = 20
      ),
      expand = expansion(
        mult = c(
          0,
          .02
        )
      )
    ) +
    labs(
      x = publication_labels$percentage_ci,
      y = NULL,
      title = title
    ) +
    theme_classic(
      base_size = 10.5
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11.5,
        margin = margin(
          b = 8
        )
      ),
      axis.text.y = element_text(
        size = 8.8,
        color = "#263238",
        lineheight = .95
      ),
      axis.text.x = element_text(
        color = "#263238"
      ),
      axis.title.x = element_text(
        size = 10,
        margin = margin(
          t = 7
        )
      ),
      axis.line = element_line(
        color = "#56636B",
        linewidth = 0.35
      ),
      axis.ticks = element_line(
        color = "#56636B",
        linewidth = 0.35
      ),
      plot.margin = plot_margin
    )
}


figure2_A <- figure2_dot_panel(
  figure2_experiences,
  paste0(
    "A.  Experiences with AI emotional support\n(n = ",
    figure2_subgroup_n(figure2_experiences),
    ")"
  ),
  x_limit = 60,
  plot_margin = margin(
    7,
    12,
    7,
    2
  )
)


# ------------------------------------------------------------
# 18. Panel B - most commonly discussed topics
# ------------------------------------------------------------

figure2_topics_all <- read_analysis_excel(
  "Analysis_09_AI_experience_descriptives.xlsx",
  sheet = "Topics_Q05"
)


# Panel B shows the eight most frequently reported topics together with
# trauma, mourning or difficult memories, which is retained for its
# relevance to this cohort although it falls outside the top eight. The
# panel is therefore a selection; Table S1 reports all twelve topics.
figure2_topics <- figure2_topics_all %>%
  filter(
    topic %in% c(
      slice_max(
        figure2_topics_all,
        pct,
        n = 8,
        with_ties = FALSE
      )$topic,
      grep(
        "Trauma",
        figure2_topics_all$topic,
        value = TRUE
      )
    )
  ) %>%
  mutate(
    display_label = case_when(
      grepl(
        "Stress / overload",
        topic,
        ignore.case = TRUE
      ) ~ "Stress / overload / burnout",
      grepl(
        "Personal dilemmas",
        topic,
        ignore.case = TRUE
      ) ~ "Personal dilemmas / values / decisions",
      grepl(
        "Interpersonal relationships",
        topic,
        ignore.case = TRUE
      ) ~ "Relationships and conflicts",
      grepl(
        "Anxiety / worries",
        topic,
        ignore.case = TRUE
      ) ~ "Anxiety / worries / panic attacks",
      grepl(
        "Low mood",
        topic,
        ignore.case = TRUE
      ) ~ "Low mood / depression",
      grepl(
        "emotional regulation",
        topic,
        ignore.case = TRUE
      ) ~ "Overwhelm / emotional regulation",
      grepl(
        "Loneliness / need",
        topic,
        ignore.case = TRUE
      ) ~ "Loneliness / need for support",
      grepl(
        "Trauma",
        topic,
        ignore.case = TRUE
      ) ~ "Trauma / mourning / difficult memories",
      TRUE ~ topic
    )
  ) %>%
  arrange(
    pct
  ) %>%
  mutate(
    display_label = factor(
      display_label,
      levels = display_label
    )
  )


figure2_B <- figure2_dot_panel(
  figure2_topics,
  paste0(
    "B.  Selected commonly discussed topics\n(n = ",
    figure2_subgroup_n(figure2_topics),
    ")"
  ),
  x_limit = 60,
  plot_margin = margin(
    7,
    18,
    7,
    14
  )
)


# ------------------------------------------------------------
# 19. Panel C - psychotherapy context
# ------------------------------------------------------------

figure2_therapy <- read_analysis_excel(
  "Analysis_09_AI_experience_descriptives.xlsx",
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


figure2_sharing <- read_analysis_excel(
  "Analysis_09_AI_experience_descriptives.xlsx",
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


stacked_context_panel <- function(
    data,
    title,
    palette,
    label_small_externally = FALSE
) {

  plot_data <- data %>%
    mutate(
      bar_label = "",
      percent_label = ifelse(
        pct >= 8,
        paste0(
          formatC(
            pct,
            digits = 1,
            format = "f"
          ),
          "%"
        ),
        ""
      ),
      small_percent_label = ifelse(
        pct < 8,
        paste0(
          formatC(
            pct,
            digits = 1,
            format = "f"
          ),
          "%"
        ),
        ""
      )
    )

  small_label_data <- plot_data %>%
    filter(
      label_small_externally,
      small_percent_label != ""
    )

  ggplot(
    plot_data,
    aes(
      x = pct,
      y = bar_label,
      fill = display_label
    )
  ) +
    geom_col(
      width = .52,
      color = "white",
      linewidth = .45
    ) +
    geom_text(
      aes(
        label = percent_label
      ),
      position = position_stack(
        vjust = .5
      ),
      color = "white",
      size = 3.1,
      fontface = "bold"
    ) +
    geom_text(
      data = small_label_data,
      aes(
        x = 101.0,
        y = bar_label,
        label = small_percent_label
      ),
      inherit.aes = FALSE,
      hjust = 0,
      color = "#263238",
      size = 2.75,
      fontface = "bold"
    ) +
    scale_x_continuous(
      breaks = seq(
        0,
        100,
        by = 20
      ),
      expand = c(
        0,
        0
      )
    ) +
    coord_cartesian(
      xlim = c(
        0,
        106
      ),
      clip = "off"
    ) +
    scale_fill_manual(
      values = palette,
      drop = FALSE
    ) +
    guides(
      fill = guide_legend(
        title = NULL,
        nrow = 2,
        byrow = TRUE,
        reverse = TRUE
      )
    ) +
    labs(
      x = publication_labels$percentage,
      y = NULL,
      title = title
    ) +
    theme_classic(
      base_size = 10
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 10.5,
        margin = margin(
          b = 5
        )
      ),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(
        size = 8.2
      ),
      legend.key.height = grid::unit(
        .35,
        "cm"
      ),
      legend.key.width = grid::unit(
        .45,
        "cm"
      ),
      plot.margin = margin(
        7,
        16,
        9,
        14
      )
    )
}


therapy_palette <- c(
  "Therapy helped more" = "#174A73",
  "Helped similarly" = "#4F86A8",
  "AI helped more" = "#8DB7D8"
)

sharing_palette <- c(
  "Not at all" = "#174A73",
  "Infrequently" = "#3E7398",
  "Frequently" = "#74A4C2",
  "Regularly / almost every session" = "#B7D3E5"
)

figure2_C_helpfulness <- stacked_context_panel(
  figure2_therapy,
  "Perceived helpfulness: psychotherapy versus AI",
  therapy_palette
)

figure2_C_sharing <- stacked_context_panel(
  figure2_sharing,
  "Sharing therapy-related content with AI",
  sharing_palette,
  label_small_externally = TRUE
)


# ------------------------------------------------------------
# 20. Combine Figure 2
# ------------------------------------------------------------

figure2_C <- (
  figure2_C_helpfulness |
    figure2_C_sharing
) +
  plot_annotation(
    title = paste0(
      "C.  AI in the context of mental health treatment (n = ",
      figure2_subgroup_n(figure2_therapy),
      ")"
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 11.5,
        margin = margin(
          t = 4,
          b = 7
        )
      )
    )
  )

figure2_C <- wrap_elements(
  full = figure2_C
)

figure2_vertical_divider <- ggplot() +
  annotate(
    "segment",
    x = .5,
    xend = .5,
    y = .08,
    yend = .92,
    color = "#D5DDE2",
    linewidth = .45
  ) +
  theme_void()

figure2_horizontal_divider <- ggplot() +
  annotate(
    "segment",
    x = .02,
    xend = .98,
    y = .5,
    yend = .5,
    color = "#D5DDE2",
    linewidth = .45
  ) +
  theme_void()

figure2_top <- (
  figure2_A |
    figure2_vertical_divider |
    figure2_B
) +
  plot_layout(
    widths = c(
      .90,
      .035,
      1.065
    )
  )

figure2 <- (
  figure2_top /
    figure2_horizontal_divider /
    figure2_C
) +
  plot_layout(
    heights = c(
      1.16,
      .035,
      .90
    )
  )


ggsave(
  filename =
    file.path(
      main_outputs_dir,
      "figures",
      "previews",
      "Figure_2_AI_Experiences_and_Psychotherapy.png"
    ),
  
  plot =
    figure2,
  
  width =
    9.5,
  
  height =
    8.0,
  
  dpi =
    600
)


ggsave(
  filename =
    file.path(
      main_outputs_dir,
      "figures",
      "Figure_2_AI_Experiences_and_Psychotherapy.pdf"
    ),
  device =
    publication_pdf_device,
  
  plot =
    figure2,
  
  width =
    9.5,
  
  height =
    8.0
)


# ============================================================
# SUPPLEMENT S1
# DETAILED AI EXPERIENCE DESCRIPTIVES
# ============================================================


s1_helpfulness <- read_analysis_excel(
  "Analysis_09_AI_experience_descriptives.xlsx",
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
    
    Percent =
      pct,
    
    Lower_95_CI =
      lower_95_CI_pct,
    
    Upper_95_CI =
      upper_95_CI_pct
  )


s1_experiences <- read_analysis_excel(
  "Analysis_09_AI_experience_descriptives.xlsx",
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
    
    Percent =
      pct,
    
    Lower_95_CI =
      lower_95_CI_pct,
    
    Upper_95_CI =
      upper_95_CI_pct
  )


s1_topics <- read_analysis_excel(
  "Analysis_09_AI_experience_descriptives.xlsx",
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
    
    Percent =
      pct,
    
    Lower_95_CI =
      lower_95_CI_pct,
    
    Upper_95_CI =
      upper_95_CI_pct
  )


s1_therapy <- read_analysis_excel(
  "Analysis_09_AI_experience_descriptives.xlsx",
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
    
    Percent =
      pct,
    
    Lower_95_CI =
      lower_95_CI_pct,
    
    Upper_95_CI =
      upper_95_CI_pct
  )


s1_sharing <- read_analysis_excel(
  "Analysis_09_AI_experience_descriptives.xlsx",
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
    
    Percent =
      pct,
    
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


clean_s1_item_label <- function(
    section,
    item
) {

  case_when(
    section == "Experiences" &
      grepl(
        "understand my feelings",
        item,
        ignore.case = TRUE
      ) ~ "Helped understand feelings or gain insight",
    section == "Experiences" &
      grepl(
        "couldn't tell people",
        item,
        ignore.case = TRUE
      ) ~ "Shared things I couldn't tell other people",
    section == "Experiences" &
      grepl(
        "instead of reaching out",
        item,
        ignore.case = TRUE
      ) ~ "Used AI instead of reaching out to someone",
    section == "Experiences" &
      grepl(
        "calm down",
        item,
        ignore.case = TRUE
      ) ~ "Helped calm down or cope with distress",
    section == "Experiences" &
      grepl(
        "relying on it repeatedly",
        item,
        ignore.case = TRUE
      ) ~ "Repeatedly relied on AI for comfort",
    section == "Experiences" &
      grepl(
        "inaccurate",
        item,
        ignore.case = TRUE
      ) ~ "Responses were inaccurate or unhelpful",
    section == "Experiences" &
      grepl(
        "feel worse",
        item,
        ignore.case = TRUE
      ) ~ "Felt worse, confused, or misunderstood",
    section == "AI compared with psychotherapy" &
      grepl(
        "Psychological therapy helped more",
        item,
        ignore.case = TRUE
      ) ~ "Therapy helped more",
    section == "AI compared with psychotherapy" &
      grepl(
        "similarly",
        item,
        ignore.case = TRUE
      ) ~ "Helped similarly",
    section == "AI compared with psychotherapy" &
      grepl(
        "AI helped more",
        item,
        ignore.case = TRUE
      ) ~ "AI helped more",
    section == "Sharing therapy content with AI" &
      item == "No, not at all" ~ "Not at all",
    section == "Sharing therapy content with AI" &
      item == "Yes, infrequently" ~ "Infrequently",
    section == "Sharing therapy content with AI" &
      item == "Yes, frequently" ~ "Frequently",
    section == "Sharing therapy content with AI" &
      grepl(
        "almost after every session",
        item,
        ignore.case = TRUE
      ) ~ "Regularly / almost every session",
    TRUE ~ item
  )
}


s1_publication_rows <- supplement_S1_AI_experiences %>%
  mutate(
    Item = clean_s1_item_label(
      Section,
      Item
    ),
    percentage_ci = pmap_chr(
      list(
        Percent,
        Lower_95_CI,
        Upper_95_CI
      ),
      format_pct_ci
    )
  ) %>%
  transmute(
    Section,
    Item,
    n = as.integer(n),
    percentage_ci
  )


s1_section_order <- c(
  "Perceived helpfulness",
  "Experiences",
  "Topics discussed",
  "AI compared with psychotherapy",
  "Sharing therapy content with AI"
)


s1_publication <- map_dfr(
  s1_section_order,
  function(section_name) {

    section_data <- s1_publication_rows %>%
      filter(
        Section == section_name
      )

    bind_rows(
      tibble(
        row_type = "section",
        Domain = section_name,
        Item = "",
        n = NA_integer_,
        percentage_ci = ""
      ),
      section_data %>%
        transmute(
          row_type = "regular",
          Domain = "",
          Item,
          n,
          percentage_ci
        )
    )
  }
)


s1_ft <- publication_flextable(
  s1_publication %>%
    select(
      -row_type
    ),
  widths = c(
    1.75,
    4.65,
    0.75,
    2.15
  ),
  alignments = c(
    "left",
    "left",
    "center",
    "center"
  ),
  font_size = 8.9
)

s1_ft <- set_header_labels(
  s1_ft,
  percentage_ci = publication_labels$percentage_ci_short
)

s1_ft <- style_section_rows(
  s1_ft,
  which(
    s1_publication$row_type == "section"
  ),
  n_columns = 4
)


# ============================================================
# SUPPLEMENT S2
# ROBUSTNESS / SENSITIVITY
# ============================================================


component_focal <- read_analysis_excel(
  "Analysis_10_robustness_sensitivity.xlsx",
  sheet = "Components_Focal"
)

component_joint <- read_analysis_excel(
  "Analysis_10_robustness_sensitivity.xlsx",
  sheet = "Components_Joint_Test"
)

inconsistency_flexible <- read_analysis_excel(
  "Analysis_10_robustness_sensitivity.xlsx",
  sheet = "Inconsistency_Flexible"
)

inconsistency_magnitude <- read_analysis_excel(
  "Analysis_10_robustness_sensitivity.xlsx",
  sheet = "Inconsistency_Magnitude"
)

s2_components <- component_focal %>%
  transmute(
    Component = dplyr::recode(
      term,
      phq_component_z = "Depression (PHQ-9)",
      gad_component_z = "Anxiety (GAD-7)",
      pss_component_z = "Perceived stress (PSS-10)"
    ),
    `OR (95% CI)` = pmap_chr(
      list(
        OR,
        CI_lower,
        CI_upper
      ),
      format_or_ci_3
    ),
    p = vapply(
      p_value,
      format_p,
      character(1)
    )
  )


s2_joint_p <- format_p(
  component_joint$p_value[[1]]
)


# Panel B repeats the flexible model after excluding the
# logically inconsistent responses, so the two columns are the
# global Wald p values from the same specification.

s2_inconsistency <- inconsistency_flexible %>%
  mutate(
    Predictor = dplyr::recode(
      test,
      `Loneliness + social support jointly` =
        "Loneliness and perceived social support jointly"
    ),
    p_formatted = vapply(
      p_value,
      format_p,
      character(1)
    )
  ) %>%
  select(
    Predictor,
    model,
    p_formatted
  ) %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = p_formatted
  ) %>%
  rename(
    `Primary sample p` =
      `Primary full sample`,
    `Exclusion sample p` =
      `Exclude logical inconsistencies`
  )


maximum_curve_difference <- max(
  inconsistency_magnitude$
  maximum_absolute_difference_percentage_points,
  na.rm = TRUE
)




s2_components_ft <- publication_flextable(
  s2_components,
  widths = c(
    3.05,
    2.25,
    0.8
  ),
  alignments = c(
    "left",
    "center",
    "center"
  ),
  font_size = 10
)

s2_inconsistency_ft <- publication_flextable(
  s2_inconsistency,
  widths = c(
    3.10,
    1.55,
    1.55
  ),
  alignments = c(
    "left",
    "center",
    "center"
  ),
  font_size = 10
)


# ------------------------------------------------------------
# Export supplementary publication tables
# ------------------------------------------------------------

table_s1_output_file <- file.path(
  supplement_tables_dir,
  "Table_S1_AI_Experience_Descriptives.docx"
)

table_s2_output_file <- file.path(
  supplement_tables_dir,
  "Table_S2_Sensitivity_Analyses.docx"
)

s1_page2_start <- which(
  s1_publication$Domain ==
    "AI compared with psychotherapy"
)

s1_page1_data <- s1_publication[
  seq_len(
    s1_page2_start - 1
  ),
]

s1_page2_data <- s1_publication[
  s1_page2_start:nrow(s1_publication),
]

make_s1_page_flextable <- function(data) {

  page_ft <- publication_flextable(
    data %>%
      select(
        -row_type
      ),
    widths = c(
      1.75,
      4.65,
      0.75,
      2.15
    ),
    alignments = c(
      "left",
      "left",
      "center",
      "center"
    ),
    font_size = 8.9
  )

  page_ft <- set_header_labels(
    page_ft,
    percentage_ci = publication_labels$percentage_ci_short
  )

  style_section_rows(
    page_ft,
    which(
      data$row_type == "section"
    ),
    n_columns = 4
  )
}

s1_page1_ft <- make_s1_page_flextable(
  s1_page1_data
)

s1_page2_ft <- make_s1_page_flextable(
  s1_page2_data
)

s1_doc <- read_docx()
s1_doc <- body_set_default_section(
  s1_doc,
  value = publication_section(
    landscape = TRUE
  )
)
s1_doc <- add_table_heading(
  s1_doc,
  "Table S1",
  paste(
    "Descriptive Characteristics of AI Emotional-Support Use",
    "and Psychotherapy-Related AI Use"
  )
)
s1_doc <- body_add_flextable(
  s1_doc,
  value = s1_page1_ft,
  align = "center"
)
s1_doc <- body_add_break(
  s1_doc
)
s1_doc <- body_add_fpar(
  s1_doc,
  fpar(
    ftext(
      "Table S1 (continued)",
      fp_text(
        font.family = "Times New Roman",
        font.size = 10,
        bold = TRUE
      )
    ),
    fp_p = fp_par(
      padding.bottom = 4
    )
  )
)
s1_doc <- body_add_flextable(
  s1_doc,
  value = s1_page2_ft,
  align = "center"
)
s1_doc <- add_table_note(
  s1_doc,
  paste(
    "For perceived helpfulness, experiences, and topics, N = 1,059.",
    "For the mental health treatment subgroup, N = 196.",
    "The n column reports the observed number of participants."
  )
)

invisible(
  print(
    s1_doc,
    target = table_s1_output_file
  )
)

table_s2_doc <- read_docx()
table_s2_doc <- body_set_default_section(
  table_s2_doc,
  value = publication_section(
    landscape = FALSE
  )
)
table_s2_doc <- add_table_heading(
  table_s2_doc,
  "Table S2",
  "Sensitivity Analyses of the Primary Model"
)
table_s2_doc <- body_add_fpar(
  table_s2_doc,
  fpar(
    ftext(
      "Panel A. Distress components entered simultaneously",
      fp_text(
        font.family = "Times New Roman",
        font.size = 10,
        bold = TRUE
      )
    ),
    fp_p = fp_par(
      padding.bottom = 3
    )
  )
)
table_s2_doc <- body_add_flextable(
  table_s2_doc,
  value = s2_components_ft,
  align = "center"
)
table_s2_doc <- body_add_par(
  table_s2_doc,
  value = "",
  style = "Normal"
)
table_s2_doc <- body_add_fpar(
  table_s2_doc,
  fpar(
    ftext(
      "Panel B. Exclusion of inconsistent AI-use responses",
      fp_text(
        font.family = "Times New Roman",
        font.size = 10,
        bold = TRUE
      )
    ),
    fp_p = fp_par(
      padding.bottom = 3
    )
  )
)
table_s2_doc <- body_add_flextable(
  table_s2_doc,
  value = s2_inconsistency_ft,
  align = "center"
)
table_s2_doc <- add_table_note(
  table_s2_doc,
  paste0(
    "OR = odds ratio; CI = confidence interval. ",
    "Panel A enters the three distress components linearly, because it ",
    "tests whether the composite result depends on any single component ",
    "rather than characterizing each component's functional form; ",
    "loneliness and perceived social support retain their spline forms. ",
    "Panel A joint Wald test: p ",
    s2_joint_p,
    ". Panel B reports the global Wald test for each predictor in the ",
    "flexible model, before and after excluding logically inconsistent ",
    "responses. Maximum absolute changes in adjusted ",
    "probabilities were approximately ",
    formatC(
      maximum_curve_difference,
      digits = 2,
      format = "f"
    ),
    " percentage points."
  )
)

invisible(
  print(
    table_s2_doc,
    target = table_s2_output_file
  )
)


# ============================================================
# EXPORT MAIN RESULTS
# ============================================================


main_output_file <- file.path(
  main_source_data_dir,
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

    Figure_2_Topics =
      figure2_topics,
    
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
  supplement_source_data_dir,
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
    
    S2_Distress_Components =
      component_focal,

    S2_Component_Joint_Test =
      component_joint,

    S2_Inconsistency_Exclusion =
      inconsistency_flexible,

    S2_Curve_Differences =
      inconsistency_magnitude
  ),
  
  path =
    supplement_output_file
)


# ============================================================
# FINAL QA
# ============================================================


publication_files <- c(
  table1_output_file,
  table2_output_file,
  file.path(
    main_figures_dir,
    "Figure_1_Adjusted_Probability_Curves.pdf"
  ),
  file.path(
    main_figures_dir,
    "Figure_2_AI_Experiences_and_Psychotherapy.pdf"
  ),
  file.path(
    main_figure_previews_dir,
    "Figure_1_Adjusted_Probability_Curves.png"
  ),
  file.path(
    main_figure_previews_dir,
    "Figure_2_AI_Experiences_and_Psychotherapy.png"
  ),
  table_s1_output_file,
  table_s2_output_file,
  main_output_file,
  supplement_output_file
)

missing_publication_files <- publication_files[
  !file.exists(publication_files)
]

if (length(missing_publication_files) > 0) {
  stop(
    paste0(
      "Publication pipeline did not create: ",
      paste(
        project_relative(missing_publication_files),
        collapse = ", "
      )
    )
  )
}

# Every test statistic printed in Table 2 is re-read from the model
# workbooks and compared with the rendered table.
table2_sync_check <- function(file, label, expected) {

  tests <- read_analysis_excel(
    file,
    sheet = "Flexible_Global_Tests"
  ) %>%
    filter(test == label)

  identical(
    format_wald(tests$output[1]),
    expected
  )
}

if (
  !table2_sync_check(
    "Analysis_06_primary_models.xlsx",
    "H1: Distress - flexible final model",
    primary_table2$`Overall association F (df)`[1]
  ) ||
  !table2_sync_check(
    "Analysis_07_general_AI_users_model.xlsx",
    "Psychological distress",
    general_table2$`Overall association F (df)`[
      general_table2$Predictor == "Psychological distress"
    ][1]
  )
) {
  stop(
    "Table 2 is out of sync with the model output workbooks."
  )
}

if (
  nrow(primary_table2) != 4 ||
  nrow(general_table2) != 8 ||
  nrow(figure2_topics) != 9 ||
  nrow(s2_components) != 3 ||
  nrow(s2_inconsistency) != 4
) {
  stop(
    "Publication row-count QA failed."
  )
}


message(
  "Publication outputs complete.\n",
  "  Main tables:   ", project_relative(main_tables_dir), "\n",
  "  Figures:       ", project_relative(main_figures_dir), "\n",
  "  Supplement:    ", project_relative(supplement_tables_dir), "\n",
  "  Source data:   ", project_relative(main_output_file), ", ",
  project_relative(supplement_output_file)
)
