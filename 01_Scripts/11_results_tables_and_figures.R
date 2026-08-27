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
  "patchwork",
  "officer",
  "flextable"
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


# Remove only the obsolete flat publication exports created by
# earlier versions of this script. Their replacements are written
# into the structured directories above during the current run.
legacy_publication_files <- c(
  file.path(
    main_outputs_dir,
    "Figure_1_Adjusted_Probability_Curves.png"
  ),
  file.path(
    main_outputs_dir,
    "Figure_1_Adjusted_Probability_Curves.pdf"
  ),
  file.path(
    main_outputs_dir,
    "Figure_2_AI_Experiences_and_Psychotherapy.png"
  ),
  file.path(
    main_outputs_dir,
    "Figure_2_AI_Experiences_and_Psychotherapy.pdf"
  ),
  file.path(
    main_outputs_dir,
    "MAIN_Results_Tables.xlsx"
  ),
  file.path(
    supplement_outputs_dir,
    "SUPPLEMENT_Results_Tables.xlsx"
  )
)

invisible(
  file.remove(
    legacy_publication_files[
      file.exists(legacy_publication_files)
    ]
  )
)


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


# Publication wording follows the weights that were actually supplied to
# the analysis. Internal weighted_* objects remain unchanged so the existing
# calculations and future non-constant weights continue to use one code path.
all_weights_equal_one <-
  length(analysis_dat$analysis_weight) == nrow(analysis_dat) &&
  all(is.finite(analysis_dat$analysis_weight)) &&
  all(analysis_dat$analysis_weight == 1)

using_post_sampling_weights <- !all_weights_equal_one

publication_labels <- list(
  percentage = if (using_post_sampling_weights) {
    "Weighted percentage"
  } else {
    "Percentage"
  },
  percentage_ci = if (using_post_sampling_weights) {
    "Weighted percentage (95% CI)"
  } else {
    "Percentage (95% CI)"
  },
  percentage_ci_short = if (using_post_sampling_weights) {
    "Weighted % (95% CI)"
  } else {
    "% (95% CI)"
  },
  continuous_summary = if (using_post_sampling_weights) {
    "weighted mean (SD)"
  } else {
    "mean (SD)"
  },
  categorical_summary = if (using_post_sampling_weights) {
    "unweighted n (weighted %)"
  } else {
    "n (%)"
  }
)

neutralize_weight_names_for_export <- function(data) {

  if (using_post_sampling_weights) {
    return(data)
  }

  names(data) <- dplyr::recode(
    names(data),
    weighted_prop = "proportion",
    weighted_pct = "percentage",
    Weighted_percent = "Percentage"
  )

  data
}

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


# Table S3 Panel A reports a bound that rounds to 1.00 at two decimals
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
          weighted_mean_safe_11(
            overall_dat[[variable]],
            overall_dat$analysis_weight
          ),
          digits
        ),
        weighted_sd_safe_11(
          overall_dat[[variable]],
          overall_dat$analysis_weight
        ),
        digits
      ),
    
    No_AI_emotional_support =
      format_mean_sd(
        clean_near_zero(
          weighted_mean_safe_11(
            nonuser_dat[[variable]],
            nonuser_dat$analysis_weight
          ),
          digits
        ),
        weighted_sd_safe_11(
          nonuser_dat[[variable]],
          nonuser_dat$analysis_weight
        ),
        digits
      ),
    
    AI_emotional_support =
      format_mean_sd(
        clean_near_zero(
          weighted_mean_safe_11(
            user_dat[[variable]],
            user_dat$analysis_weight
          ),
          digits
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


primary_table2 <- primary_linear_final %>%
  mutate(
    `OR (95% CI)` =
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
        linear_p,
        format_p,
        character(1)
      )
  ) %>%
  select(
    Predictor,
    `OR (95% CI)`,
    p
  )


# ------------------------------------------------------------
# 9. General-AI-user secondary model
# ------------------------------------------------------------

general_linear <- read_safe_excel(
  "SAFE_07_general_AI_users_model.xlsx",
  sheet = "Linear_Focal_Results"
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


general_table2 <- general_linear_final %>%
  mutate(
    `OR (95% CI)` =
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
        linear_p,
        format_p,
        character(1)
      )
  ) %>%
  select(
    Predictor,
    `OR (95% CI)`,
    p
  )


table2_panel_row <- function(label) {

  tibble(
    row_type = "section",
    Predictor = label,
    `OR (95% CI)` = "",
    p = ""
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
    3.1,
    2.25,
    0.75
  ),
  alignments = c(
    "left",
    "center",
    "center"
  ),
  font_size = 10
)

table2_ft <- style_section_rows(
  table2_ft,
  which(
    table2_publication$row_type == "section"
  ),
  n_columns = 3
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
      "and perceived social support) were standardized before modeling;",
      "ORs correspond to a 1-SD increase.",
      "ORs represent the pre-specified linear models.",
      "Nonlinear associations are characterized separately in Figure 1."
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
  landscape = FALSE
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
# predictions are evaluated at the observed UCLA-3 raw scores 3-9,
# but displayed on the corresponding standardized scale.
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
      x = weighted_pct,
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

figure2_topics_all <- read_safe_excel(
  "SAFE_09_AI_experience_descriptives.xlsx",
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
        weighted_pct,
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
    weighted_pct
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
        weighted_pct >= 8,
        paste0(
          formatC(
            weighted_pct,
            digits = 1,
            format = "f"
          ),
          "%"
        ),
        ""
      ),
      small_percent_label = ifelse(
        weighted_pct < 8,
        paste0(
          formatC(
            weighted_pct,
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
      x = weighted_pct,
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
        Weighted_percent,
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
# FREQUENCY-THRESHOLD ANALYSIS
# ============================================================


threshold_linear <- read_safe_excel(
  "SAFE_08_ordinal_frequency_model.xlsx",
  sheet = "Threshold_Linear_Focal"
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
    
    Frequency_threshold = dplyr::recode(
      threshold,
      `>0` = "Any use vs none",
      `>1` = "At least several times/month vs less",
      `>2` = "At least several times/week vs less",
      `>3` = "Almost daily/daily vs less"
    ),

    `OR (95% CI)` =
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
  ) %>%
  select(
    Frequency_threshold,
    Predictor,
    `OR (95% CI)`,
    p
  )


supplement_S2_thresholds <- s2_linear


s2_ft <- publication_flextable(
  supplement_S2_thresholds,
  widths = c(
    3.15,
    2.45,
    2.1,
    0.75
  ),
  alignments = c(
    "left",
    "left",
    "center",
    "center"
  ),
  font_size = 9.4
)

s2_ft <- set_header_labels(
  s2_ft,
  Frequency_threshold = "Frequency threshold",
  Predictor = "Predictor",
  p = "p"
)

s2_ft <- merge_v(
  s2_ft,
  j = 1,
  part = "body"
)

s2_ft <- hline(
  s2_ft,
  i = c(
    3,
    6,
    9
  ),
  border = fp_border(
    color = "#A8B2BA",
    width = .6
  ),
  part = "body"
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

s3_components <- component_focal %>%
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


s3_joint_p <- format_p(
  component_joint$p_value[[1]]
)


s3_inconsistency <- inconsistency_linear %>%
  transmute(
    Predictor = dplyr::recode(
      term,
      distress_z = "Psychological distress",
      loneliness_z = "Loneliness",
      social_support_z = "Perceived social support"
    ),
    `Primary OR (95% CI)` = pmap_chr(
      list(
        full_OR,
        full_CI_lower,
        full_CI_upper
      ),
      format_or_ci
    ),
    `Sensitivity OR (95% CI)` = pmap_chr(
      list(
        sensitivity_OR,
        sensitivity_CI_lower,
        sensitivity_CI_upper
      ),
      format_or_ci
    )
  )


maximum_curve_difference <- max(
  inconsistency_magnitude$
  maximum_absolute_difference_percentage_points,
  na.rm = TRUE
)


supplement_S3_robustness <- bind_rows(
  component_focal %>%
    mutate(
      source_section = "Distress components"
    ),
  inconsistency_linear %>%
    mutate(
      source_section = "Inconsistency exclusion"
    ),
  .id = "source_block"
)


s3_components_ft <- publication_flextable(
  s3_components,
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

s3_inconsistency_ft <- publication_flextable(
  s3_inconsistency,
  widths = c(
    2.65,
    1.75,
    1.80
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
  "Table_S2_Frequency_Threshold_Models.docx"
)

table_s3_output_file <- file.path(
  supplement_tables_dir,
  "Table_S3_Sensitivity_Analyses.docx"
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
    if (using_post_sampling_weights) {
      "Percentages and confidence intervals are survey-weighted; n values are unweighted."
    } else {
      "The n column reports the observed number of participants."
    }
  )
)

invisible(
  print(
    s1_doc,
    target = table_s1_output_file
  )
)

export_publication_table(
  s2_ft,
  "Table S2",
  paste(
    "Cumulative Binary Logistic Models Across AI Emotional-Support",
    "Frequency Thresholds"
  ),
  paste(
    "Each model adjusts for age, gender, education, income, and",
    "religious affiliation. The three focal predictors (psychological",
    "distress, loneliness, and perceived social support) were standardized",
    "before modeling; ORs correspond to a 1-SD increase."
  ),
  table_s2_output_file,
  landscape = TRUE
)


table_s3_doc <- read_docx()
table_s3_doc <- body_set_default_section(
  table_s3_doc,
  value = publication_section(
    landscape = FALSE
  )
)
table_s3_doc <- add_table_heading(
  table_s3_doc,
  "Table S3",
  "Sensitivity Analyses of the Primary Model"
)
table_s3_doc <- body_add_fpar(
  table_s3_doc,
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
table_s3_doc <- body_add_flextable(
  table_s3_doc,
  value = s3_components_ft,
  align = "center"
)
table_s3_doc <- body_add_par(
  table_s3_doc,
  value = "",
  style = "Normal"
)
table_s3_doc <- body_add_fpar(
  table_s3_doc,
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
table_s3_doc <- body_add_flextable(
  table_s3_doc,
  value = s3_inconsistency_ft,
  align = "center"
)
table_s3_doc <- add_table_note(
  table_s3_doc,
  paste0(
    "OR = odds ratio; CI = confidence interval. ",
    "Panel A joint Wald test: p ",
    s3_joint_p,
    ". In Panel B, maximum absolute changes in adjusted ",
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
    table_s3_doc,
    target = table_s3_output_file
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
      neutralize_weight_names_for_export(
        figure2_experiences
      ),

    Figure_2_Topics =
      neutralize_weight_names_for_export(
        figure2_topics
      ),
    
    Figure_2_Therapy =
      neutralize_weight_names_for_export(
        figure2_therapy
      ),
    
    Figure_2_Sharing =
      neutralize_weight_names_for_export(
        figure2_sharing
      )
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
      neutralize_weight_names_for_export(
        supplement_S1_AI_experiences
      ),
    
    S2_Frequency_Thresholds =
      supplement_S2_thresholds,
    
    S3_Distress_Components =
      component_focal,

    S3_Component_Joint_Test =
      component_joint,

    S3_Inconsistency_Exclusion =
      inconsistency_linear,

    S3_Curve_Differences =
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
  table_s3_output_file,
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

if (
  nrow(primary_table2) != 3 ||
  nrow(general_table2) != 3 ||
  nrow(figure2_topics) != 9 ||
  nrow(s3_components) != 3 ||
  nrow(s3_inconsistency) != 3
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
