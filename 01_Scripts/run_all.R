# ============================================================
# run_all.R
# STRONG-1 Thesis: AI Emotional Support
#
# Runs the whole pipeline in order, in one fresh session, and
# stops at the first script that fails.
#
# Usage, from the project root:
#   Rscript 01_Scripts/run_all.R
#
# Each numbered script is run in its own R process, so no object
# from one step can silently satisfy a dependency in the next.
# That is what makes a clean run evidence of reproducibility
# rather than of a warm workspace.
#
# The child processes are started from the project root and are
# given RELATIVE script paths. This matters when the project sits
# under directory names containing non-ASCII characters: an
# absolute path would carry those characters through the shell
# and into the child process, where the encoding round trip can
# mangle them. A relative path inside the project is pure ASCII,
# and the working directory is inherited from the operating
# system rather than passed as text, so neither can be corrupted.
# ============================================================


# ------------------------------------------------------------
# 1. Character locale
#
# Rscript does not always inherit a UTF-8 locale. Without one,
# any non-ASCII character in the project path becomes unusable as
# soon as a package re-encodes it. Repairing LC_CTYPE here also
# fixes the child processes, which inherit this environment.
# Only LC_CTYPE is touched, so sort order is left alone.
# ------------------------------------------------------------

ensure_utf8_ctype <- function() {

  if (isTRUE(l10n_info()$MBCS)) {
    return(invisible(TRUE))
  }

  candidates <- c(
    Sys.getenv("LANG", unset = ""),
    "en_US.UTF-8",
    "C.UTF-8",
    "C.utf8",
    "UTF-8"
  )

  candidates <- unique(
    candidates[grepl("utf-?8", candidates, ignore.case = TRUE)]
  )

  for (candidate in candidates) {

    suppressWarnings(
      try(
        Sys.setlocale("LC_CTYPE", candidate),
        silent = TRUE
      )
    )

    if (isTRUE(l10n_info()$MBCS)) {
      Sys.setenv(LC_CTYPE = candidate)
      return(invisible(TRUE))
    }
  }

  invisible(FALSE)
}


ensure_utf8_ctype()


# ------------------------------------------------------------
# 2. Locate the project root
#
# Candidates are validated by looking for the marker file, so a
# path that arrives mangled simply fails the test and the next
# candidate is used.
# ------------------------------------------------------------

locate_project_root <- function() {

  has_marker <- function(path) {

    isTRUE(
      file.exists(
        file.path(path, "01_Scripts", "run_all.R")
      )
    )
  }

  candidates <- character(0)

  configured_root <- Sys.getenv(
    "STRONG1_PROJECT",
    unset = ""
  )

  if (nzchar(configured_root)) {
    candidates <- c(candidates, configured_root)
  }

  candidates <- c(
    candidates,
    ".",
    "..",
    file.path("..", "..")
  )

  # Rscript encodes spaces in the --file= argument as "~+~" before
  # handing it to the R front end, so a path containing a space
  # comes back mangled. Undo that substitution.
  unescape_rscript_path <- function(path) {
    gsub("~+~", " ", path, fixed = TRUE)
  }

  reported <- unescape_rscript_path(
    sub(
      "^--file=",
      "",
      grep(
        "^--file=",
        commandArgs(trailingOnly = FALSE),
        value = TRUE
      )
    )
  )

  if (length(reported) > 0) {
    candidates <- c(
      candidates,
      dirname(
        dirname(
          tail(reported, 1)
        )
      )
    )
  }

  usable <- candidates[vapply(candidates, has_marker, logical(1))]

  if (length(usable) == 0) {
    stop(
      paste(
        "Could not locate the project root.",
        "Run this from the project root:",
        "cd <project root> && Rscript 01_Scripts/run_all.R",
        "Alternatively set the STRONG1_PROJECT environment",
        "variable to the project directory."
      )
    )
  }

  usable[1]
}


project_root <- locate_project_root()

original_working_directory <- setwd(project_root)

on.exit(
  setwd(original_working_directory),
  add = TRUE
)


# ------------------------------------------------------------
# 3. Pipeline
# ------------------------------------------------------------

pipeline <- c(
  "01_make_dictionary.R",
  "02_analysis_mapping.R",
  "03_preanalysis_checks.R",
  "04_prepare_analysis_data.R",
  "05_descriptives.R",
  "06_primary_models.R",
  "07_general_AI_users_model.R",
  "09_AI_experience_descriptives.R",
  "10_robustness_sensitivity.R",
  "11_results_tables_and_figures.R"
)


# Relative to the project root, and therefore ASCII only.
script_paths <- file.path("01_Scripts", pipeline)

missing_scripts <- script_paths[!file.exists(script_paths)]

if (length(missing_scripts) > 0) {
  stop(
    paste0(
      "Missing pipeline script(s): ",
      paste(missing_scripts, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------
# 4. Run each script in its own process
#
# R.home() is inside the R installation, which is ASCII on every
# supported platform, so quoting it is safe.
# ------------------------------------------------------------

r_executable <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
)


started_at <- Sys.time()

for (index in seq_along(pipeline)) {

  message("\n=== ", pipeline[index], " ===")

  status <- system2(
    r_executable,
    args = script_paths[index]
  )

  if (status != 0) {
    stop(
      paste0(
        pipeline[index],
        " failed with status ",
        status,
        ". Fix it before continuing; later scripts read its output."
      )
    )
  }
}


message(
  "\nPipeline complete in ",
  round(
    as.numeric(
      difftime(
        Sys.time(),
        started_at,
        units = "mins"
      )
    ),
    1
  ),
  " minutes."
)
