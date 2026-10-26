# 05_intensive_users.R
# R5 (exploratory): daily ("almost every day / every day") versus less
# frequent AI emotional-support use among emotional-support users with
# daily general AI use (111 vs 295), which holds overall AI engagement
# constant:
#   - one adjusted model of age, gender and the M5 psychological,
#     interpersonal and trauma predictors (the 2 users reporting another
#     gender, none daily, are excluded: N = 404);
#   - all 21 reported-experience comparisons (N = 406), Holm-corrected
#     within each item family.
# Output: Intensive_Users.xlsx.

source(if (file.exists("01_Scripts/01_prepare_data.R")) "01_Scripts/01_prepare_data.R" else "01_prepare_data.R")
use_packages("car")

users <- filter(analysis_dat, ai_support_binary == 1)
restricted <- users %>%
  filter(ai_general_raw == 4) %>%
  mutate(daily = ai_support_daily, female = as.integer(gender_full == "Female"))

group_sizes <- count(users, daily = ai_support_daily, daily_general_ai_use = ai_general_raw == 4)


# ---- Adjusted model ----------------------------------------------

exposure_terms <- c("ace_z", "lec_direct_z", "oct7_z")
model_terms <- c("age", "female", "distress_z", "pcl_z", exposure_terms, "loneliness_z", "social_support_z")
model_dat <- filter(restricted, gender_full != "Other")

if (nrow(restricted) != 406 || sum(restricted$daily) != 111 ||
    nrow(model_dat) != 404 || sum(model_dat$daily) != 111) {
  stop("Intensive-user samples do not reproduce 406 (111 vs 295) and 404 (111 vs 293).")
}

model <- fit_logit(model_dat, model_terms, outcome = "daily")
events <- sum(model_dat$daily)
fitted_rank <- rank(fitted(model))

model_check <- tibble(
  item = c("N", "Daily users (events)", "Less frequent users", "Slope parameters",
           "Events per slope parameter", "Excluded: gender 'Other' (none daily)",
           "c-statistic", "Maximum VIF", "Maximum SE", "Converged"),
  value = c(
    nrow(model_dat), events, nrow(model_dat) - events, length(model_terms),
    events / length(model_terms), nrow(restricted) - nrow(model_dat),
    (sum(fitted_rank[model_dat$daily == 1]) - events * (events + 1) / 2) / (events * (nrow(model_dat) - events)),
    max(vif_table(model_dat, model_terms, "daily")$GVIF),
    max(sqrt(diag(vcov(model)))), model$converged
  )
)


# ---- Experience comparisons --------------------------------------

restricted <- mutate(restricted,
                     help_moderate = as.integer(ai_helpfulness >= 2),
                     help_a_lot = as.integer(ai_helpfulness >= 3))

items <- tibble(
  family = rep(c("Helpfulness", "Experiences", "Topics"), c(2, 7, 12)),
  variable = c("help_moderate", "help_a_lot", q04_items, q05_items),
  label = c(
    "At least moderately helpful", "Helped a lot or a very great deal",
    "It helped me calm down or cope with distress", "It helped me understand my feelings or gain insight",
    "I shared things I couldn't tell people", "I used it instead of reaching out to people",
    "I found myself relying on it repeatedly for comfort",
    "The responses were inaccurate, unhelpful, or frustrating",
    "It made me feel worse, more confused, or misunderstood",
    "Anxiety / worries / panic attacks", "Stress / overload / burnout", "Low mood / depression",
    "Overwhelm or difficulty with emotional regulation", "Sleep difficulties",
    "Loneliness / need for conversation and support", "Interpersonal relationships and conflicts",
    "Trauma, mourning, or difficult memories", "Body image / eating or harmful habits",
    "Personal dilemmas / values / decision making", "Regarding professional help (before / after)", "Other"
  )
)

compare <- function(v) {
  x <- restricted[[v]]
  daily <- restricted$daily == 1
  k <- c(sum(x[daily]), sum(x[!daily]))
  n <- c(sum(daily), sum(!daily))
  ci <- suppressWarnings(prop.test(k, n, correct = FALSE)$conf.int)  # unadjusted Wald 95% CI
  tibble(
    k_daily = k[1], n_daily = n[1], pct_daily = 100 * k[1] / n[1],
    k_less = k[2], n_less = n[2], pct_less = 100 * k[2] / n[2],
    difference_pct = 100 * (k[1] / n[1] - k[2] / n[2]),
    difference_CI_lower = 100 * ci[1], difference_CI_upper = 100 * ci[2],
    fisher_p = fisher.test(matrix(c(k, n - k), 2))$p.value
  )
}

comparisons <- items %>%
  bind_cols(map_dfr(items$variable, compare)) %>%
  group_by(family) %>%
  mutate(holm_p = p.adjust(fisher_p, method = "holm")) %>%
  ungroup()

write_analysis(
  list(
    Group_Sizes = group_sizes,
    Model = odds_ratios(model),
    Model_Check = model_check,
    Model_Joint_Test = wald_test(model, exposure_terms, "Trauma exposure (ACE-10, LEC-5, October 7 index), joint"),
    Experience_Comparisons = comparisons
  ),
  "Intensive_Users.xlsx"
)
