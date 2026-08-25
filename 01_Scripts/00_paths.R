# ============================================================
# 00_paths.R
# Shared project configuration for the STRONG-1 thesis scripts
#
# Sourced by every other script in this pipeline. Defines the
# project directories, the expected location of the STRONG-1
# data file, and two small helpers used across scripts.
#
# All paths are resolved relative to this file's location, so
# the repository can be cloned to any directory on any machine.
# ============================================================


# ------------------------------------------------------------
# 1. Locate this file, then derive the project root
#
# This file lives in <project root>/01_Scripts. Every project
# directory is derived from that stable location rather than
# from getwd(), so the pipeline is independent of the working
# directory and of any machine-specific path.
# ------------------------------------------------------------

locate_paths_file <- function() {

  # a. Sourced from another script (the normal case).
  source_frames <- Filter(
    Negate(is.null),
    lapply(sys.frames(), `[[`, "ofile")
  )

  if (length(source_frames) > 0) {
    return(normalizePath(tail(source_frames, 1)[[1]]))
  }

  # b. Run with Rscript.
  command_args <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command_args, value = TRUE)

  if (length(file_argument) > 0) {
    return(
      normalizePath(
        sub("^--file=", "", tail(file_argument, 1))
      )
    )
  }

  # c. Fallback: the working directory is the script folder
  #    or the project root.
  candidates <- c(
    "00_paths.R",
    file.path("01_Scripts", "00_paths.R")
  )

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(normalizePath(candidate))
    }
  }

  stop(
    paste(
      "00_paths.R could not locate itself.",
      "Source the pipeline scripts from the project root,",
      "or set the working directory to the repository folder."
    )
  )
}


paths_file <- locate_paths_file()
scripts_dir <- dirname(paths_file)
project_dir <- dirname(scripts_dir)


# ------------------------------------------------------------
# 2. Output directories
# ------------------------------------------------------------

safe_outputs_dir <- file.path(project_dir, "02_SAFE_Outputs")
main_outputs_dir <- file.path(project_dir, "03_Main_Outputs")
supplement_outputs_dir <- file.path(project_dir, "04_Supplement")

data_dir <- file.path(project_dir, "data")

for (path_directory in c(
  safe_outputs_dir,
  main_outputs_dir,
  supplement_outputs_dir,
  data_dir
)) {
  dir.create(
    path_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ------------------------------------------------------------
# 3. STRONG-1 data file
#
# The participant-level dataset is NOT distributed with this
# repository. An authorized researcher places STRONG1Data.xlsx
# in the (git-ignored) data/ directory, or points the
# STRONG1_DATA environment variable at it.
# ------------------------------------------------------------

data_file <- Sys.getenv(
  "STRONG1_DATA",
  unset = file.path(data_dir, "STRONG1Data.xlsx")
)


# ------------------------------------------------------------
# 4. Shared helpers
# ------------------------------------------------------------

# Install any declared package that is not already available.
# Each script declares the packages it needs in a `packages`
# vector and then calls ensure_packages(packages).
ensure_packages <- function(packages) {

  missing_packages <- packages[
    !(packages %in% installed.packages()[, "Package"])
  ]

  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }

  invisible(packages)
}


# Render a path relative to the project root. Used for console
# messages, so that no machine-specific location is ever printed
# to a log, an issue, or a commit.
project_relative <- function(path) {

  prefix <- paste0(project_dir, .Platform$file.sep)

  ifelse(
    startsWith(path, prefix),
    substring(path, nchar(prefix) + 1L),
    basename(path)
  )
}


rm(locate_paths_file, paths_file, path_directory)
