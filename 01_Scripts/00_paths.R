# ============================================================
# 00_paths.R
# Shared project configuration for the STRONG-1 thesis scripts
#
# Sourced by every other script in this pipeline. Defines the
# project directories, the expected location of the STRONG-1
# data file, and two small helpers used across scripts.
#
# Paths are resolved relative to the project root, so the
# repository works from any location, including one whose parent
# directories contain non-ASCII characters.
# ============================================================


# ------------------------------------------------------------
# 1. Character locale
#
# Non-ASCII characters in a file path are only usable when R is
# running in a multi-byte (UTF-8) character locale. Under the "C"
# locale R can still list and stat such a path, but anything that
# re-encodes it fails: readxl, for instance, normalises the data
# file to an absolute path and then hands it to utils::unzip(),
# which reports that the file "cannot be opened" even though it
# plainly exists.
#
# Rscript does not always inherit a UTF-8 locale, so LC_CTYPE is
# repaired here if necessary. Only LC_CTYPE is touched: changing
# LC_COLLATE would alter sort order, and therefore factor level
# order, which must not depend on the machine.
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

      # Child processes should start in this locale too.
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
# The root is identified by testing candidate directories for the
# marker file 01_Scripts/00_paths.R, and the first candidate that
# has it wins. Relative candidates are tried before absolute ones
# on purpose.
#
# The reason is encoding. The project may sit under directory
# names containing non-ASCII characters, and an absolute path
# therefore contains them too. Those characters do not reliably
# survive the round trip between R, commandArgs(), the shell and
# a child R process: the path comes back mangled and no longer
# points at anything. A relative path inside the project is pure
# ASCII, so it always survives. Working directories are inherited
# from the operating system rather than passed as text, so
# resolving against the working directory sidesteps the problem
# entirely.
#
# Every candidate is validated by file.exists(), so a mangled
# path simply fails the test and the next candidate is used.
# ------------------------------------------------------------

locate_project_root <- function() {

  has_marker <- function(path) {

    isTRUE(
      file.exists(
        file.path(path, "01_Scripts", "00_paths.R")
      )
    )
  }

  candidates <- character(0)

  # a. Explicit override, for schedulers and unusual setups.
  configured_root <- Sys.getenv(
    "STRONG1_PROJECT",
    unset = ""
  )

  if (nzchar(configured_root)) {
    candidates <- c(candidates, configured_root)
  }

  # b. The working directory and a few levels above it. This is
  #    the normal route when the pipeline is run from the project
  #    root, and it never encodes a path as text.
  candidates <- c(
    candidates,
    ".",
    "..",
    file.path("..", ".."),
    file.path("..", "..", "..")
  )

  # c. The location R reports for this file, if it is usable.
  #    dirname() is applied twice: 00_paths.R lives in 01_Scripts.
  # Rscript encodes spaces in the --file= argument as "~+~" before
  # handing it to the R front end, so a path containing a space
  # comes back mangled. Undo that substitution.
  unescape_rscript_path <- function(path) {
    gsub("~+~", " ", path, fixed = TRUE)
  }

  reported <- c(
    unlist(
      Filter(
        Negate(is.null),
        lapply(sys.frames(), `[[`, "ofile")
      )
    ),
    unescape_rscript_path(
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
        "Run the pipeline from the project root, for example:",
        "cd <project root> && Rscript 01_Scripts/run_all.R",
        "Alternatively set the STRONG1_PROJECT environment",
        "variable to the project directory."
      )
    )
  }

  usable[1]
}


project_dir <- locate_project_root()
scripts_dir <- file.path(project_dir, "01_Scripts")


# The project root is usually "." and therefore ASCII, but the
# absolute path underneath it may not be, and packages that
# normalise paths internally will meet that absolute form.
if (
  !isTRUE(l10n_info()$MBCS) &&
  grepl("[^\x01-\x7F]", getwd())
) {
  stop(
    paste(
      "The project path contains non-ASCII characters but R is",
      "running in a non-UTF-8 character locale, so file paths",
      "cannot be encoded reliably. Set a UTF-8 locale before",
      "running the pipeline, for example:",
      "export LANG=en_US.UTF-8"
    )
  )
}


# ------------------------------------------------------------
# 3. Output directories
# ------------------------------------------------------------

analysis_outputs_dir <- file.path(project_dir, "02_Analysis_Outputs")
main_outputs_dir <- file.path(project_dir, "03_Main_Outputs")
supplement_outputs_dir <- file.path(project_dir, "04_Supplement")

data_dir <- file.path(project_dir, "data")

for (path_directory in c(
  analysis_outputs_dir,
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
# 4. STRONG-1 data file
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
# 5. Shared helpers
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

  trimmed <- ifelse(
    startsWith(path, prefix),
    substring(path, nchar(prefix) + 1L),
    basename(path)
  )

  sub("^\\./", "", trimmed)
}


rm(ensure_utf8_ctype, locate_project_root, path_directory)
