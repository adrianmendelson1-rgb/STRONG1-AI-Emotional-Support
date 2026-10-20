# run_all.R
# Runs the pipeline from the project root, each script in a fresh R
# process, stopping at the first failure:
#   Rscript 01_Scripts/run_all.R
# 00_setup.R and 01_prepare_data.R are sourced by the scripts below.

source(if (file.exists("01_Scripts/00_setup.R")) "01_Scripts/00_setup.R" else "00_setup.R")

scripts <- c("02_data_checks.R", "03_descriptives.R", "04_models.R", "05_intensive_users.R", "06_tables_figures.R")
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
start <- Sys.time()

for (s in scripts) {
  message("\n== ", s)
  if (system2(rscript, file.path("01_Scripts", s)) != 0) stop(s, " failed.")
}

message("\nPipeline complete in ", round(difftime(Sys.time(), start, units = "mins"), 1), " minutes.")
