# 03_descriptives.R
# Prevalence and frequency of AI use, participant characteristics by
# emotional-support use (Table 1), and the experiences and topics
# reported by emotional-support users (Figure 1, Table S1).
# Output: Descriptives.xlsx.

source("01_Scripts/00_setup.R")

analysis_dat <- readRDS(analysis_file)$participants
groups <- list(
  Overall = analysis_dat,
  `Non-users` = filter(analysis_dat, ai_support_binary == 0),
  Users = filter(analysis_dat, ai_support_binary == 1)
)
users <- groups$Users

# Proportion with a logit-transformed 95% CI (equal-probability design,
# so the standard error is the usual binomial one).
prop_ci <- function(indicator) {
  indicator <- indicator[!is.na(indicator)]
  design <- svydesign(ids = ~1, probs = ~1, data = data.frame(indicator))
  ci <- confint(svyciprop(~indicator, design, method = "logit"))
  tibble(
    n_valid = length(indicator),
    n_selected = sum(indicator == 1),
    pct = 100 * mean(indicator),
    CI_lower = 100 * max(0, ci[1]),
    CI_upper = 100 * min(1, ci[2])
  )
}


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


# ---- Table 1: characteristics by emotional-support use -----------

continuous_vars <- c("age", "distress_z", "loneliness", "social_support", "pcl", "ace", "lec_direct", "oct7_total_14")
categorical_vars <- c("gender", "education", "income", "religion", "ai_general_frequency")

table1_summaries <- imap_dfr(groups, function(d, group) {
  continuous <- map_dfr(continuous_vars, function(v) {
    tibble(variable = v, level = NA_character_, n = sum(!is.na(d[[v]])), pct = NA_real_,
           mean = mean(d[[v]], na.rm = TRUE), sd = sd(d[[v]], na.rm = TRUE),
           min = min(d[[v]], na.rm = TRUE), max = max(d[[v]], na.rm = TRUE))
  })
  categorical <- map_dfr(categorical_vars, function(v) {
    tibble(variable = v, level = levels(d[[v]]), n = as.vector(table(d[[v]])),
           pct = 100 * as.vector(table(d[[v]])) / nrow(d))
  })
  bind_rows(continuous, categorical) %>% mutate(group = group, .before = 1)
})


# ---- AI experiences among emotional-support users ----------------

item_prevalence <- function(vars, family) {
  map_dfr(vars, ~ bind_cols(family = family, variable = .x, item = item_labels[[.x]], prop_ci(users[[.x]]))) %>%
    arrange(desc(pct))
}

ai_experiences <- bind_rows(
  bind_cols(family = "Helpfulness", variable = "MH_AI_Q03", item = "At least moderately helpful",
            prop_ci(as.integer(users$ai_helpfulness >= 2))),
  bind_cols(family = "Helpfulness", variable = "MH_AI_Q03", item = "Helped a lot or very great deal",
            prop_ci(as.integer(users$ai_helpfulness >= 3))),
  item_prevalence(q04_items, "Experiences"),
  item_prevalence(q05_items, "Topics")
)

write_analysis(
  list(
    Prevalence = prevalence,
    Use_Frequency = frequency,
    Table1_Summaries = table1_summaries,
    AI_Experiences = ai_experiences
  ),
  "Descriptives.xlsx"
)
