# 02_data_checks.R
# Data-integrity checks: central STRONG QC flow, scale reliability,
# validation of the supplied scale scores against their items,
# response coding, and the analytic sample flow. Stops if any audited
# value is not reproduced. Output: Data_Checks.xlsx.

source(if (file.exists("01_Scripts/01_prepare_data.R")) "01_Scripts/01_prepare_data.R" else "01_prepare_data.R")
use_packages("psych")

values <- read_excel(data_file, sheet = "T1_Values_200826", guess_max = 10000)
labels <- read_excel(data_file, sheet = "T1_Labels_200826", guess_max = 10000)
filtering <- read_excel(data_file, sheet = "T1_Filtering_200826", guess_max = 10000)


# ---- Central STRONG QC flow --------------------------------------

qc_flags <- c("MissingID", "TestID", "Incomplete", "BeforeSurveyMadePublic", "Inattentive", "Duplicates")
flags <- as.matrix(filtering[qc_flags])
flags[is.na(flags)] <- FALSE
excluded <- !filtering$Included
# Excluded with no coded flag: completed in under 10 minutes.
fast_completion <- excluded & rowSums(flags) == 0 & filtering$DurationInSeconds < 600

qc_flow <- tibble(
  stage = c("Records in T1_Filtering", "Excluded by central QC", "Retained", "Rows in T1_Values"),
  n = c(nrow(filtering), sum(excluded), sum(!excluded), nrow(values))
)
qc_reasons <- tibble(
  reason = c(qc_flags, "Completion under 600 s, no other flag"),
  n_excluded = c(colSums(flags[excluded, ] == 1), sum(fast_completion, na.rm = TRUE)),
  note = c(rep("Flags can overlap; do not sum", 6), "Mutually exclusive with the flags above")
)

if (!identical(qc_flow$n, c(4979L, 1377L, 3602L, 3602L)) ||
    sum(fast_completion, na.rm = TRUE) != 57 ||
    any(!excluded & filtering$DurationInSeconds < 600, na.rm = TRUE)) {
  stop("Central QC flow does not reproduce the audited counts.")
}


# ---- Reliability and score validation ----------------------------

item_sets <- list(
  `PHQ-9` = sprintf("PHQ9_Q%02d", 1:9), `GAD-7` = sprintf("GAD7_Q%02d", 1:7),
  `PSS-10` = sprintf("PSS_Q%02d", 1:10), `UCLA-3` = sprintf("UCLA_Loneliness_Q%02d", 1:3),
  `mMOS-SS` = sprintf("MOSSS_Q%02d", 1:8)
)
items <- function(scale) {
  x <- as.data.frame(values[item_sets[[scale]]])
  if (scale == "UCLA-3") x[x < 0] <- NA  # technical missing codes
  x
}

reliability <- imap_dfr(item_sets, function(vars, scale) {
  x <- items(scale)
  x <- x[complete.cases(x), ]
  tibble(scale, items = length(vars), n = nrow(x),
         alpha = psych::alpha(x, check.keys = FALSE, warnings = FALSE)$total$raw_alpha)
})
audited_alpha <- c(`PHQ-9` = 0.899, `GAD-7` = 0.932, `PSS-10` = 0.813, `UCLA-3` = 0.851, `mMOS-SS` = 0.957)
if (any(abs(reliability$alpha - audited_alpha[reliability$scale]) > 0.01)) {
  stop("Reliability does not reproduce the audited alpha values.")
}

supplied <- list(
  `PHQ-9` = values$PHQ_Score, `GAD-7` = values$GAD_Score, `PSS-10` = values$PSS_Score,
  `UCLA-3` = replace(values$UCLA_Loneliness_Score, values$UCLA_Loneliness_Score < 0, NA),
  `mMOS-SS` = values$MOSSS_Score
)
score_validation <- imap_dfr(supplied, function(score, scale) {
  # mMOS-SS is the 0-100 transformation of the mean of its 8 items.
  calculated <- if (scale == "mMOS-SS") (rowMeans(items(scale)) - 1) * 25 else rowSums(items(scale))
  both <- !is.na(score) & !is.na(calculated)
  tibble(scale, n_compared = sum(both), n_mismatch = sum(abs(score[both] - calculated[both]) > 1e-8))
})
if (any(score_validation$n_mismatch > 0)) stop("Supplied scale scores do not match their items.")


# ---- Response coding of categorical analysis variables -----------

coded_vars <- c("Gender", "Education", "Income", "Religiosity_Q01", "MH_AI_Q01", "MH_AI_Q02",
                "MH_AI_Q03", "Iran_Q01", "Oct7_Evacuation", "Oct7_Videos", "Army_Y_N")
response_coding <- map_dfr(coded_vars, function(v) {
  tibble(variable = v, code = values[[v]], label = as.character(labels[[v]])) %>%
    filter(!is.na(code)) %>%
    count(variable, code, label, name = "n")
})


# ---- Analytic samples and PCL-5 routing --------------------------

users <- filter(analysis_dat, ai_support_binary == 1)

sample_flow <- tibble(
  sample = c(
    "Retained T1 sample",
    "Hierarchical models (complete cases; 2 lack a loneliness score)",
    "General AI users",
    "Secondary model (general AI users, complete cases)",
    "AI emotional-support users",
    "Daily emotional-support users",
    "Emotional-support users with daily general AI use",
    "  of whom daily emotional-support users",
    "Emotional-support use without general AI use (inconsistent)",
    "Routed past the PCL-5 (no index event; scored 0)",
    "  of whom in the hierarchical-model sample"
  ),
  n = c(
    nrow(analysis_dat),
    sum(model_complete),
    sum(analysis_dat$general_ai_user),
    sum(model_complete & analysis_dat$general_ai_user == 1),
    nrow(users),
    sum(users$ai_support_daily),
    sum(users$ai_general_raw == 4),
    sum(users$ai_support_daily[users$ai_general_raw == 4]),
    sum(analysis_dat$ai_logic_inconsistent),
    sum(analysis_dat$pcl_routed),
    sum(analysis_dat$pcl_routed[model_complete])
  )
)

routing_profile <- analysis_dat[model_complete, ] %>%
  group_by(group = if_else(pcl_routed == 1, "Routed past the PCL-5", "Administered the PCL-5")) %>%
  summarise(
    n = n(),
    pct_ai_support_use = 100 * mean(ai_support_binary),
    mean_distress_z = mean(distress_z),
    mean_ace = mean(ace),
    mean_lec_direct = mean(lec_direct),
    mean_oct7_total_14 = mean(oct7_total_14),
    n_pcl_zero = sum(pcl == 0),
    .groups = "drop"
  )

missingness <- tibble(
  variable = names(analysis_dat),
  n_missing = colSums(is.na(analysis_dat))
) %>%
  # Items asked only of emotional-support users are missing by design.
  filter(n_missing > 0, !grepl("^MH_AI_Q0[45]", variable), !variable %in% c("ai_support_daily", "ai_helpfulness"))

write_analysis(
  list(
    QC_Flow = qc_flow,
    QC_Reasons = qc_reasons,
    Reliability = reliability,
    Score_Validation = score_validation,
    Response_Coding = response_coding,
    Sample_Flow = sample_flow,
    PCL_Routing_Profile = routing_profile,
    Missingness = missingness
  ),
  "Data_Checks.xlsx"
)
