# 03_descriptives.R
# R1 descriptives: prevalence of AI use, participant characteristics
# with unadjusted associations (Table 1), and the experiences and
# topics reported by emotional-support users (Figure 1, Table S1).
# Output: Descriptives.xlsx.

source(if (file.exists("01_Scripts/01_prepare_data.R")) "01_Scripts/01_prepare_data.R" else "01_prepare_data.R")

groups <- list(
  Overall = analysis_dat,
  `Non-users` = filter(analysis_dat, ai_support_binary == 0),
  Users = filter(analysis_dat, ai_support_binary == 1)
)
users <- groups$Users


# ---- Prevalence of AI use ----------------------------------------

prevalence <- bind_rows(
  bind_cols(measure = "Any general AI use", prop_ci(analysis_dat$general_ai_user)),
  bind_cols(measure = "Any AI emotional-support use", prop_ci(analysis_dat$ai_support_binary)),
  bind_cols(measure = "AI emotional-support use among general AI users",
            prop_ci(analysis_dat$ai_support_binary[analysis_dat$general_ai_user == 1]))
)

frequency <- map_dfr(c(general = "ai_general_frequency", emotional_support = "ai_support_frequency"), function(v) {
  map_dfr(levels(analysis_dat[[v]]), function(level) {
    bind_cols(category = level, prop_ci(as.integer(analysis_dat[[v]] == level)))
  })
}, .id = "use")

use_crosstab <- count(analysis_dat, general = ai_general_frequency, emotional_support = ai_support_frequency)


# ---- Table 1: characteristics by emotional-support use -----------

continuous_vars <- c("age", "distress_z", "loneliness", "social_support", "pcl", "ace", "lec_direct", "oct7_total_14")
categorical_vars <- c("gender_full", "education", "income", "religion_full", "ai_general_frequency", "iran_relocated")

table1_summaries <- imap_dfr(groups, function(d, group) {
  continuous <- map_dfr(continuous_vars, function(v) {
    tibble(variable = v, level = NA_character_, n = sum(!is.na(d[[v]])), pct = NA_real_,
           mean = mean(d[[v]], na.rm = TRUE), sd = sd(d[[v]], na.rm = TRUE))
  })
  categorical <- map_dfr(categorical_vars, function(v) {
    x <- if (is.factor(d[[v]])) d[[v]] else factor(d[[v]])
    tibble(variable = v, level = levels(x), n = as.vector(table(x)),
           pct = 100 * as.vector(table(x)) / nrow(d), mean = NA_real_, sd = NA_real_)
  })
  bind_rows(continuous, categorical) %>% mutate(group = group, .before = 1)
})

# One univariable logistic regression per characteristic, in the
# metric of the adjusted models. General AI-use frequency is not
# modelled: emotional-support use is nested within general AI use.
or_specs <- c(
  age_decades = "Age, per 10 years", gender_model = "Gender (vs female)",
  education = "Education (vs secondary)", income = "Income (vs about average)",
  religion_table = "Religious affiliation (vs Jewish)", distress_z = "Psychological distress, per SD",
  loneliness_z = "Loneliness, per SD", social_support_z = "Perceived social support, per SD",
  pcl_z = "PCL-5, per SD", ace_z = "ACE-10, per SD", lec_direct_z = "LEC-5 direct, per SD",
  oct7_z = "Oct7_Total_14, per SD", iran_relocated = "Iran relocation (yes vs no)"
)
or_dat <- mutate(analysis_dat, age_decades = age / 10, religion_table = relevel(religion_full, "Jewish"))

unadjusted_ors <- imap_dfr(or_specs, function(label, v) {
  d <- or_dat[!is.na(or_dat[[v]]), ]
  odds_ratios(fit_logit(d, v)) %>%
    transmute(variable = v, label, level = if_else(term == v, NA_character_, sub(v, "", term, fixed = TRUE)),
              n = nrow(d), events = sum(d$ai_support_binary), OR, CI_lower, CI_upper, p_value)
})


# ---- AI experiences among emotional-support users ----------------

if (anyNA(users[c("ai_helpfulness", q04_items, q05_items)])) {
  stop("Follow-up items are missing for some emotional-support users.")
}

helpfulness_labels <- c("Did not help at all", "Helped a little", "Helped moderately",
                        "Helped a lot", "Helped a very great deal")
q04_labels <- c(
  "It helped me calm down or cope with distress", "It helped me understand my feelings or gain insight",
  "I shared things I couldn't tell people", "I used it instead of reaching out to people",
  "I found myself relying on it repeatedly for comfort",
  "The responses were inaccurate, unhelpful, or frustrating",
  "It made me feel worse, more confused, or misunderstood"
)
q05_labels <- c(
  "Anxiety / worries / panic attacks", "Stress / overload / burnout", "Low mood / depression",
  "Overwhelm or difficulty with emotional regulation", "Sleep difficulties",
  "Loneliness / need for conversation and support", "Interpersonal relationships and conflicts",
  "Trauma, mourning, or difficult memories", "Body image / eating or harmful habits",
  "Personal dilemmas / values / decision making", "Regarding professional help (before / after)", "Other"
)

item_prevalence <- function(vars, labels, family) {
  map2_dfr(vars, labels, ~ bind_cols(family = family, variable = .x, item = .y, prop_ci(users[[.x]]))) %>%
    arrange(desc(pct))
}

helpfulness <- map_dfr(0:4, ~ bind_cols(code = .x, item = helpfulness_labels[.x + 1],
                                        prop_ci(as.integer(users$ai_helpfulness == .x))))

ai_experiences <- bind_rows(
  bind_cols(family = "Helpfulness", variable = "MH_AI_Q03", item = "At least moderately helpful",
            prop_ci(as.integer(users$ai_helpfulness >= 2))),
  bind_cols(family = "Helpfulness", variable = "MH_AI_Q03", item = "Helped a lot or very great deal",
            prop_ci(as.integer(users$ai_helpfulness >= 3))),
  item_prevalence(q04_items, q04_labels, "Experiences"),
  item_prevalence(q05_items, q05_labels, "Topics")
)

write_analysis(
  list(
    Prevalence = prevalence,
    Use_Frequency = frequency,
    Use_Crosstab = use_crosstab,
    Table1_Summaries = table1_summaries,
    Unadjusted_ORs = unadjusted_ors,
    Helpfulness = helpfulness,
    AI_Experiences = ai_experiences
  ),
  "Descriptives.xlsx"
)
