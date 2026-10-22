# 01_prepare_data.R
# Builds the analysis dataset (`analysis_dat`) in memory from the
# STRONG-1 T1 workbook. Sourced by scripts 02-05; nothing at
# participant level is written to disk.

source(if (file.exists("01_Scripts/00_setup.R")) "01_Scripts/00_setup.R" else "00_setup.R")

raw <- read_excel(data_file, sheet = "T1_Values_200826", guess_max = 10000)

q04_items <- sprintf("MH_AI_Q04_R%02d", 0:6)   # experiences of AI support
q05_items <- sprintf("MH_AI_Q05_R%02d", 1:12)  # topics discussed
pcl_items <- sprintf("PCL_Q%02d", 1:20)
oct7_sources <- c(
  sprintf("Oct7_Exposures_R%02d", c(1:4, 7)), "Oct7_Evacuation", "Oct7_Videos",
  "Army_Y_N", "Iran_Q01", sprintf("Oct7_Family_Q01_R%02d", 1:4)
)


# ---- Validate raw coding -----------------------------------------

allowed_codes <- c(
  list(
    Gender = 1:3, Education = 1:5, Income = 1:5, Religiosity_Q01 = 1:6,
    UCLA_Loneliness_Score = c(-97, 3:9),  # -97: score not computable (one item missing)
    MH_AI_Q01 = 0:4, MH_AI_Q02 = 0:4, MH_AI_Q03 = 0:4,
    ACE_Score = 0:10, Iran_Q01 = 0:1, Oct7_Evacuation = 0:3, Oct7_Videos = 0:4, Army_Y_N = 0:2
  ),
  setNames(rep(list(0:1), 7 + 12 + 9), c(q04_items, q05_items,
    sprintf("Oct7_Exposures_R%02d", c(1:4, 7)), sprintf("Oct7_Family_Q01_R%02d", 1:4)))
)
score_ranges <- list(
  Age = c(18, 120), PHQ_Score = c(0, 27), GAD_Score = c(0, 21), PSS_Score = c(0, 40),
  MOSSS_Score = c(0, 100), LEC5_Direct_Score = c(0, 17), PCL_Total_Score = c(0, 80)
)

required <- unique(c(names(allowed_codes), names(score_ranges), oct7_sources, pcl_items, "TraumaRank"))
if (length(setdiff(required, names(raw))) > 0) {
  stop("Missing variables: ", paste(setdiff(required, names(raw)), collapse = ", "))
}
for (v in names(allowed_codes)) {
  bad <- setdiff(unique(na.omit(raw[[v]])), allowed_codes[[v]])
  if (length(bad) > 0) stop(v, " has unexpected codes: ", paste(bad, collapse = ", "))
}
for (v in names(score_ranges)) {
  if (any(raw[[v]] < score_ranges[[v]][1] | raw[[v]] > score_ranges[[v]][2], na.rm = TRUE)) {
    stop(v, " has values outside ", paste(score_ranges[[v]], collapse = "-"))
  }
}
if (!all(rowSums(raw[pcl_items]) == raw$PCL_Total_Score)) {
  stop("PCL_Total_Score does not equal the sum of its 20 items.")
}


# ---- Analysis variables ------------------------------------------

frequency_labels <- c("Not at all", "A few times", "Several times a month",
                      "Several times a week", "Almost every day / every day")

analysis_dat <- raw %>%
  transmute(
    # Demographics (model reference categories: female, secondary
    # education, about-average income, Jewish)
    age = Age,
    gender_full = factor(Gender, 1:3, c("Female", "Male", "Other")),
    gender_model = gender_full,
    education = relevel(factor(Education, 1:5, c("Primary", "Secondary", "Post-secondary non-academic",
                                                 "Bachelor's degree", "Master's degree or higher")), "Secondary"),
    income = relevel(factor(Income, 1:5, c("Much below average", "Below average", "About average",
                                           "Above average", "Much above average")), "About average"),
    religion_full = factor(Religiosity_Q01, 1:6, c("Jewish", "Muslim", "Christian", "Druze",
                                                   "Atheist / no religion", "Other")),
    religion_binary = factor(if_else(Religiosity_Q01 == 1, "Jewish", "Non-Jewish"), c("Jewish", "Non-Jewish")),

    # Psychological and interpersonal scores
    phq = PHQ_Score, gad = GAD_Score, pss = PSS_Score,
    loneliness = if_else(UCLA_Loneliness_Score == -97, NA_real_, UCLA_Loneliness_Score),
    social_support = MOSSS_Score,

    # AI use (past 3 months). Primary outcome: any emotional-support use.
    ai_general_frequency = factor(MH_AI_Q01, 0:4, frequency_labels),
    ai_general_raw = MH_AI_Q01,
    general_ai_user = as.integer(MH_AI_Q01 > 0),
    ai_support_frequency = factor(MH_AI_Q02, 0:4, frequency_labels),
    ai_support_binary = as.integer(MH_AI_Q02 > 0),
    ai_support_daily = if_else(MH_AI_Q02 > 0, as.integer(MH_AI_Q02 == 4), NA_integer_),
    ai_logic_inconsistent = as.integer(MH_AI_Q01 == 0 & MH_AI_Q02 > 0),
    ai_helpfulness = MH_AI_Q03,
    across(all_of(c(q04_items, q05_items))),

    # Trauma exposure windows
    ace = ACE_Score,                 # childhood adversity to age 18
    lec_direct = LEC5_Direct_Score,  # adulthood events experienced directly ("not sure" excluded)
    # Since 7 October 2023: Dalia's Oct7_Total_14, unmodified. The
    # evacuation items are nested (still displaced scores 2) and one of
    # the 14 items is relocation from home during the Iran war.
    oct7_total_14 = Oct7_Exposures_R01 + Oct7_Exposures_R02 + Oct7_Exposures_R03 +
      Oct7_Exposures_R04 + Oct7_Exposures_R07 +
      (Oct7_Evacuation %in% 1:3) + (Oct7_Evacuation %in% 1:2) + (Oct7_Videos %in% 1:4) +
      (Army_Y_N %in% 1:2) + (Iran_Q01 == 1) +
      Oct7_Family_Q01_R01 + Oct7_Family_Q01_R02 + Oct7_Family_Q01_R03 + Oct7_Family_Q01_R04,

    # PTSD symptoms. The PCL-5 refers to the event chosen in TraumaRank;
    # TraumaRank = 50 ("no event most significant") skipped it and the
    # STRONG data team stored all items as 0: a structural zero, not an
    # observed absence of symptoms.
    pcl = PCL_Total_Score,
    pcl_routed = as.integer(TraumaRank %in% 50),
    trauma_rank_missing = as.integer(TraumaRank %in% -99)
  ) %>%
  mutate(
    across(c(phq, gad, pss, loneliness, social_support, pcl, ace, lec_direct),
           standardize, .names = "{.col}_z"),
    oct7_z = standardize(oct7_total_14),
    # Distress composite: mean of standardized PHQ-9, GAD-7 and PSS-10,
    # re-standardized.
    distress_z = standardize((phq_z + gad_z + pss_z) / 3)
  )

rm(raw)


# ---- Audited counts ----------------------------------------------

model_complete <- complete.cases(analysis_dat[c(
  "age", "gender_model", "education", "income", "religion_binary",
  "distress_z", "loneliness_z", "social_support_z", "pcl_z", "ace_z", "lec_direct_z", "oct7_z"
)])

audit <- c(
  retained = nrow(analysis_dat) == 3602,
  model_sample = sum(model_complete) == 3600,
  # The 2 retained participants outside the model sample lack only a
  # loneliness score (UCLA-3 not computable).
  model_exclusions = all(is.na(analysis_dat$loneliness[!model_complete])) &&
    sum(is.na(analysis_dat$loneliness)) == 2,
  model_events = sum(analysis_dat$ai_support_binary[model_complete]) == 1059,
  gender_other = sum(analysis_dat$gender_full == "Other") == 9,
  ai_support_users = sum(analysis_dat$ai_support_binary) == 1059,
  daily_users = sum(analysis_dat$ai_support_daily, na.rm = TRUE) == 115,
  pcl_routed = sum(analysis_dat$pcl_routed) == 533,
  pcl_routed_in_sample = sum(analysis_dat$pcl_routed[model_complete]) == 532,
  pcl_routed_zero = all(analysis_dat$pcl[analysis_dat$pcl_routed == 1] == 0),
  oct7_total = sum(analysis_dat$oct7_total_14) == 9434 && sum(analysis_dat$oct7_total_14 == 0) == 177
)
if (!all(audit)) {
  stop("Audited counts not reproduced: ", paste(names(audit)[!audit], collapse = ", "))
}
