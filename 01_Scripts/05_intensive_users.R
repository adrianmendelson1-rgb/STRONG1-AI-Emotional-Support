# 05_intensive_users.R
# Exploratory: daily ("almost every day / every day") versus less frequent
# AI emotional-support use among emotional-support users with daily
# general AI use (111 vs 295), which restricts the comparison to daily
# general AI users:
#   - one adjusted model of age, gender and the M5 psychological,
#     interpersonal and trauma predictors (the 2 users reporting another
#     gender, none daily, are excluded: N = 404);
#   - all 21 reported-experience comparisons (N = 406), Holm-corrected
#     within each item family.
# Output: Intensive_Users.xlsx.

source("01_Scripts/00_setup.R")

users <- filter(readRDS(analysis_file)$participants, ai_support_binary == 1)
restricted <- users %>%
  filter(general_ai_daily == 1) %>%
  mutate(daily = ai_support_daily, female = as.integer(gender == "Female"),
         help_moderate = as.integer(ai_helpfulness >= 2), help_a_lot = as.integer(ai_helpfulness >= 3))

group_sizes <- count(users, daily = ai_support_daily, daily_general_ai_use = general_ai_daily == 1)


# ---- Adjusted model ----------------------------------------------

exposure_terms <- c("ace_z", "lec_direct_z", "oct7_z")
model_terms <- c("age", "female", "distress_z", "pcl_z", exposure_terms, "loneliness_z", "social_support_z")
model_dat <- filter(restricted, gender != "Other")
model <- fit_logit(model_dat, model_terms, outcome = "daily")

events <- sum(model_dat$daily)
fitted_rank <- rank(fitted(model))
model_check <- tibble(
  item = c("N", "Daily users (events)", "Less frequent users", "Excluded: gender 'Other' (none daily)",
           "c-statistic", "Maximum VIF"),
  value = c(
    nrow(model_dat), events, nrow(model_dat) - events, nrow(restricted) - nrow(model_dat),
    (sum(fitted_rank[model_dat$daily == 1]) - events * (events + 1) / 2) / (events * (nrow(model_dat) - events)),
    max(vif_table(model_dat, model_terms, "daily")$GVIF)
  )
)


# ---- Experience comparisons --------------------------------------

items <- tibble(
  family = rep(c("Helpfulness", "Experiences", "Topics"), c(2, 7, 12)),
  variable = c("help_moderate", "help_a_lot", q04_items, q05_items),
  label = c("At least moderately helpful", "Helped a lot or a very great deal", unname(item_labels[c(q04_items, q05_items)]))
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
