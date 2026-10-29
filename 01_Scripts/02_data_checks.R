# 02_data_checks.R
# Data-integrity checks on the analysis dataset: central QC flow, item
# coding and scale scores, reliability, AI follow-up and PCL-5 routing,
# and every analytic sample. Stops if any audited value is not
# reproduced. Output: Data_Checks.xlsx.

source("01_Scripts/00_setup.R")

data <- readRDS(analysis_file)
d <- data$participants
screening <- data$screening


# ---- Central STRONG QC flow --------------------------------------

qc_flags <- c("MissingID", "TestID", "Incomplete", "BeforeSurveyMadePublic", "Inattentive", "Duplicates")
flags <- as.matrix(screening[qc_flags])
flags[is.na(flags)] <- FALSE
excluded <- !screening$Included
# Excluded with no coded flag: completed in under 10 minutes.
fast_completion <- excluded & rowSums(flags) == 0 & screening$DurationInSeconds < 600

qc_flow <- tibble(
  stage = c("Records screened", "Excluded by central QC", "Retained", "Participants in the analysis dataset"),
  n = c(nrow(screening), sum(excluded), sum(!excluded), nrow(d))
)
qc_reasons <- tibble(
  reason = c(qc_flags, "Completion under 600 s, no other flag"),
  n_excluded = c(colSums(flags[excluded, ] == 1), sum(fast_completion, na.rm = TRUE)),
  note = c(rep("Flags can overlap; do not sum", 6), "Mutually exclusive with the flags above")
)

if (!identical(qc_flow$n, c(4979L, 1377L, 3602L, 3602L)) ||
    !setequal(d$UserID, screening$UserID[!excluded]) ||
    sum(fast_completion, na.rm = TRUE) != 57 ||
    any(!excluded & screening$DurationInSeconds < 600, na.rm = TRUE)) {
  stop("Central QC flow does not reproduce the audited counts.")
}


# ---- Item coding, scale scores and reliability -------------------

scales <- list(
  `PHQ-9` = list(items = sprintf("PHQ9_Q%02d", 1:9), codes = 0:3, score = d$PHQ_Score),
  `GAD-7` = list(items = sprintf("GAD7_Q%02d", 1:7), codes = 0:3, score = d$GAD_Score),
  # Items 4, 5, 7 and 8 are stored reverse-scored.
  `PSS-10` = list(items = sprintf("PSS_Q%02d", 1:10), codes = 0:4, score = d$PSS_Score),
  # -99: technical missing code; the score is then not computable.
  `UCLA-3` = list(items = sprintf("UCLA_Loneliness_Q%02d", 1:3), codes = c(-99, 1:3), score = d$loneliness),
  `mMOS-SS` = list(items = sprintf("MOSSS_Q%02d", 1:8), codes = 1:5, score = d$MOSSS_Score),
  `PCL-5` = list(items = sprintf("PCL_Q%02d", 1:20), codes = 0:4, score = d$PCL_Total_Score),
  `ACE-10` = list(items = sprintf("ACE_Q%02d", 1:10), codes = 0:1, score = d$ACE_Score),
  # Code 1 = happened to me (5 = not sure).
  `LEC-5` = list(items = sprintf("LEC5_Q%02d", 1:17), codes = 0:5, score = d$LEC5_Direct_Score)
)
item_matrix <- function(scale) {
  x <- as.matrix(d[scales[[scale]]$items])
  x[x == -99] <- NA
  x
}

score_validation <- imap_dfr(scales, function(s, scale) {
  x <- item_matrix(scale)
  calculated <- switch(scale, `mMOS-SS` = (rowMeans(x) - 1) * 25, `LEC-5` = rowSums(x == 1), rowSums(x))
  both <- !is.na(s$score) & !is.na(calculated)
  tibble(scale, items = length(s$items), codes_valid = all(as.matrix(d[s$items]) %in% s$codes),
         n_compared = sum(both), n_mismatch = sum(abs(s$score[both] - calculated[both]) > 1e-8))
})
if (!all(score_validation$codes_valid) || any(score_validation$n_mismatch > 0)) {
  stop("Item codes or supplied scale scores are invalid.")
}

cronbach_alpha <- function(scale, x, note = NA_character_) {
  x <- x[complete.cases(x), , drop = FALSE]
  k <- ncol(x)
  tibble(scale, items = k, n = nrow(x), alpha = k / (k - 1) * (1 - sum(apply(x, 2, var)) / var(rowSums(x))), note)
}
reliability <- bind_rows(
  map_dfr(c("PHQ-9", "GAD-7", "PSS-10", "UCLA-3", "mMOS-SS"), ~ cronbach_alpha(.x, item_matrix(.x))),
  cronbach_alpha("PCL-5", item_matrix("PCL-5")[d$pcl_routed == 0, ], "Participants administered the PCL-5"),
  cronbach_alpha("Distress composite", as.matrix(d[c("phq_z", "gad_z", "pss_z")]), "Standardized PHQ-9, GAD-7 and PSS-10 totals")
)


# ---- Routing and analytic samples --------------------------------

# AI follow-up items were answered by exactly the emotional-support users;
# participants routed past the PCL-5 are all scored 0.
follow_ups <- c("MH_AI_Q03", q04_items, q05_items)
if (!all((rowSums(is.na(d[follow_ups])) == 0) == (d$ai_support_binary == 1)) ||
    any(d$pcl[d$pcl_routed == 1] != 0)) {
  stop("AI follow-up or PCL-5 routing is not as expected.")
}

# Every analytic sample with its number of emotional-support users.
es_user <- d$ai_support_binary == 1
intensive <- es_user & d$general_ai_daily == 1
samples <- list(
  "Retained T1 sample (descriptives, Table 1, Figure 1)" = rep(TRUE, nrow(d)),
  "Excluded from the models: loneliness score not computable" = !d$model_sample,
  "Hierarchical models M1-M5 (complete cases)" = d$model_sample,
  "General AI users" = d$general_ai_user == 1,
  "Secondary model (general AI users, complete cases)" = d$model_sample & d$general_ai_user == 1,
  "Emotional-support use without general AI use (inconsistent)" = d$ai_logic_inconsistent == 1,
  "Sensitivity: inconsistent responders excluded" = d$model_sample & d$ai_logic_inconsistent == 0,
  "Routed past the PCL-5 (no index event; scored 0)" = d$pcl_routed == 1,
  "  of whom in the hierarchical-model sample" = d$model_sample & d$pcl_routed == 1,
  "Sensitivity: administered the PCL-5" = d$model_sample & d$pcl_routed == 0,
  "AI emotional-support users (Table S1)" = es_user,
  "Daily emotional-support users (almost every day / every day)" = es_user & d$ai_support_daily == 1,
  "Emotional-support users with daily general AI use (21 comparisons)" = intensive,
  "  of whom daily emotional-support users (comparisons)" = intensive & d$ai_support_daily == 1,
  "Intensive-user model (gender 'Other' excluded)" = intensive & d$gender != "Other",
  "  of whom daily emotional-support users (model)" = intensive & d$gender != "Other" & d$ai_support_daily == 1
)
sample_flow <- imap_dfr(samples, ~ tibble(sample = .y, n = sum(.x), emotional_support_users = sum(es_user[.x])))

audited_samples <- c(3602, 2, 3600, 2785, 2784, 15, 3585, 533, 532, 3068, 1059, 115, 406, 111, 404, 111)
audited_users <- c(1059, 0, 1059, 1044, 1044, 15, 1044, 104, 104, 955, 1059, 115, 406, 111, 404, 111)
if (!identical(sample_flow$n, as.integer(audited_samples)) ||
    !identical(sample_flow$emotional_support_users, as.integer(audited_users))) {
  stop("Analytic samples do not reproduce the audited counts.")
}

# Missing values other than the follow-up items skipped by design.
missingness <- tibble(variable = names(d), n_missing = colSums(is.na(d))) %>%
  filter(n_missing > 0, !variable %in% c(follow_ups, "ai_helpfulness", "ai_support_daily"))

write_analysis(
  list(
    QC_Flow = qc_flow,
    QC_Reasons = qc_reasons,
    Score_Validation = score_validation,
    Reliability = reliability,
    Response_Coding = data$value_labels,
    Sample_Flow = sample_flow,
    Missingness = missingness
  ),
  "Data_Checks.xlsx"
)
