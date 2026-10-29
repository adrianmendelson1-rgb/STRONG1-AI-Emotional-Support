# 01_prepare_data.R
# Builds the cleaned analysis dataset from the STRONG-1 T1 workbook, which
# is only read. Output: data/STRONG1_T1_analysis_data.rds, the only data
# source of scripts 02-05, holding
#   participants  the 3,602 retained participants: every source item and
#                 supplied score used, in its original coding, plus the
#                 derived analysis variables;
#   screening     the central STRONG quality-control records (4,979);
#   value_labels  the response labels of the coded categorical items.

source("01_Scripts/00_setup.R")

values <- read_excel(raw_file, sheet = "T1_Values_200826", guess_max = 10000)
labels <- read_excel(raw_file, sheet = "T1_Labels_200826", guess_max = 10000)
screening <- read_excel(raw_file, sheet = "T1_Filtering_200826", guess_max = 10000)

source_vars <- c(
  # Identification, administration and design (TemporaryWeights is a
  # placeholder of 1s; all analyses are unweighted)
  "UserID", "UserLanguage", "DurationInMinutes", "TemporaryWeights",
  "Age", "Gender", "Education", "Income", "Religiosity_Q01",
  # AI use; items 03-05 were asked only when MH_AI_Q02 > 0
  "MH_AI_Q01", "MH_AI_Q02", "MH_AI_Q03", q04_items, q05_items,
  sprintf("PHQ9_Q%02d", 1:9), "PHQ_Score", sprintf("GAD7_Q%02d", 1:7), "GAD_Score",
  sprintf("PSS_Q%02d", 1:10), "PSS_Score",
  sprintf("UCLA_Loneliness_Q%02d", 1:3), "UCLA_Loneliness_Score",
  sprintf("MOSSS_Q%02d", 1:8), "MOSSS_Score",
  # TraumaRank is the PCL-5 index event and routing item
  "TraumaRank", sprintf("PCL_Q%02d", 1:20), "PCL_Total_Score",
  sprintf("ACE_Q%02d", 1:10), "ACE_Score", sprintf("LEC5_Q%02d", 1:17), "LEC5_Direct_Score",
  sprintf("Oct7_Exposures_R%02d", 0:9), "Oct7_Evacuation", "Oct7_Videos",
  sprintf("Oct7_Family_Q01_R%02d", 0:5), "Army_Y_N", "Iran_Q01"
)

coded_vars <- c("Gender", "Education", "Income", "Religiosity_Q01", "MH_AI_Q01", "MH_AI_Q02", "MH_AI_Q03",
                "Oct7_Evacuation", "Oct7_Videos", "Army_Y_N", "Iran_Q01")
allowed_codes <- c(
  setNames(list(1:3, 1:5, 1:5, 1:6, 0:4, 0:4, 0:4, 0:3, 0:4, 0:2, 0:1), coded_vars),
  list(UCLA_Loneliness_Score = c(-97, 3:9)),  # -97: not computable (an item is missing)
  setNames(rep(list(0:1), 28), c(q04_items, q05_items, sprintf("Oct7_Exposures_R%02d", c(1:4, 7)),
                                 sprintf("Oct7_Family_Q01_R%02d", 1:4)))
)
for (v in names(allowed_codes)) {
  bad <- setdiff(na.omit(values[[v]]), allowed_codes[[v]])
  if (length(bad) > 0) stop(v, " has unexpected codes: ", paste(bad, collapse = ", "))
}
if (any(values$Age < 18)) stop("Participants under 18 in the data.")

value_labels <- map_dfr(coded_vars, function(v) {
  tibble(variable = v, code = values[[v]], label = as.character(labels[[v]])) %>%
    filter(!is.na(code)) %>%
    count(variable, code, label, name = "n")
})

standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
frequency_labels <- c("Not at all", "A few times", "Several times a month",
                      "Several times a week", "Almost every day / every day")

participants <- values %>%
  select(all_of(source_vars)) %>%
  mutate(
    # Model reference categories: female, secondary education,
    # about-average income, Jewish.
    age = Age,
    gender = factor(Gender, 1:3, c("Female", "Male", "Other")),
    education = relevel(factor(Education, 1:5, c("Primary", "Secondary", "Post-secondary non-academic",
                                                 "Bachelor's degree", "Master's degree or higher")), "Secondary"),
    income = relevel(factor(Income, 1:5, c("Much below average", "Below average", "About average",
                                           "Above average", "Much above average")), "About average"),
    religion = factor(Religiosity_Q01, 1:6, c("Jewish", "Muslim", "Christian", "Druze",
                                              "Atheist / no religion", "Other")),
    religion_binary = factor(if_else(Religiosity_Q01 == 1, "Jewish", "Non-Jewish"), c("Jewish", "Non-Jewish")),

    phq = PHQ_Score, gad = GAD_Score, pss = PSS_Score,
    loneliness = na_if(UCLA_Loneliness_Score, -97),
    social_support = MOSSS_Score,

    # AI use in the past 3 months. Primary outcome: any emotional-support use.
    ai_general_frequency = factor(MH_AI_Q01, 0:4, frequency_labels),
    general_ai_user = as.integer(MH_AI_Q01 > 0),
    general_ai_daily = as.integer(MH_AI_Q01 == 4),
    ai_support_frequency = factor(MH_AI_Q02, 0:4, frequency_labels),
    ai_support_binary = as.integer(MH_AI_Q02 > 0),
    ai_support_daily = if_else(MH_AI_Q02 > 0, as.integer(MH_AI_Q02 == 4), NA_integer_),
    ai_logic_inconsistent = as.integer(MH_AI_Q01 == 0 & MH_AI_Q02 > 0),
    ai_helpfulness = MH_AI_Q03,

    ace = ACE_Score,                 # childhood adversity to age 18
    lec_direct = LEC5_Direct_Score,  # event types experienced directly ("not sure" excluded)
    # The STRONG team's 14-item October 7 index, unmodified: evacuation is
    # nested (still displaced scores 2) and one item is relocation during
    # the Iran war.
    oct7_total_14 = Oct7_Exposures_R01 + Oct7_Exposures_R02 + Oct7_Exposures_R03 +
      Oct7_Exposures_R04 + Oct7_Exposures_R07 +
      (Oct7_Evacuation %in% 1:3) + (Oct7_Evacuation %in% 1:2) + (Oct7_Videos %in% 1:4) +
      (Army_Y_N %in% 1:2) + (Iran_Q01 == 1) +
      Oct7_Family_Q01_R01 + Oct7_Family_Q01_R02 + Oct7_Family_Q01_R03 + Oct7_Family_Q01_R04,

    # TraumaRank = 50 ("no event most significant") skipped the PCL-5 and
    # the STRONG data team stored its items as 0: a structural zero, not
    # an observed absence of symptoms.
    pcl = PCL_Total_Score,
    pcl_routed = as.integer(TraumaRank == 50),

    across(c(phq, gad, pss, loneliness, social_support, pcl, ace, lec_direct), standardize, .names = "{.col}_z"),
    oct7_z = standardize(oct7_total_14),
    # Distress composite: mean of the standardized PHQ-9, GAD-7 and
    # PSS-10 totals, re-standardized.
    distress_z = standardize((phq_z + gad_z + pss_z) / 3),

    # Complete cases on every M1-M5 variable: the hierarchical-model sample.
    model_sample = complete.cases(age, gender, education, income, religion_binary, distress_z,
                                  loneliness_z, social_support_z, pcl_z, ace_z, lec_direct_z, oct7_z)
  )

saveRDS(list(participants = participants, screening = screening, value_labels = value_labels), analysis_file)
