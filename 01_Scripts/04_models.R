# 04_models.R
# Logistic regressions of any AI emotional-support use. All continuous
# predictors are linear on the log-odds scale (age per year, the rest
# per sample SD).
#   R2/R3  Five-step hierarchical model on one complete-case sample:
#          M1 demographics, M2 + distress, M3 + loneliness and social
#          support (the original psychological model), M4 + PCL-5,
#          M5 + the four trauma exposure windows.
#   R4     One secondary model: M5 + general AI-use frequency among
#          general AI users.
#   S2     Sensitivity: distress components (M3); without the 15
#          inconsistent responders (M3, M5); M5 among participants
#          administered the PCL-5.
# Output: Models.xlsx.

source(if (file.exists("01_Scripts/01_prepare_data.R")) "01_Scripts/01_prepare_data.R" else "01_prepare_data.R")
use_packages("car")

blocks <- list(
  M1 = c("age", "gender_model", "education", "income", "religion_binary"),
  M2 = "distress_z",
  M3 = c("loneliness_z", "social_support_z"),
  M4 = "pcl_z",
  M5 = c("ace_z", "lec_direct_z", "oct7_z", "iran_z")
)
block_labels <- c(
  M1 = "Demographic covariates", M2 = "Psychological distress",
  M3 = "Loneliness and perceived social support", M4 = "PTSD symptoms (PCL-5)",
  M5 = "Trauma exposure windows (ACE-10, LEC-5, October 7, Iran war)"
)
model_terms <- lapply(seq_along(blocks), function(i) unlist(blocks[1:i], use.names = FALSE))
names(model_terms) <- names(blocks)
focal_terms <- unlist(blocks[-1], use.names = FALSE)

# Overall tests for the multi-category covariates and the exposure block.
overall_sets <- list(Gender = "gender_model", Education = "education", Income = "income",
                     `Trauma exposure windows (joint)` = blocks$M5)
overall_tests <- function(model, terms) {
  sets <- Filter(function(s) all(s %in% terms), overall_sets)
  imap_dfr(sets, ~ wald_test(model, .x, .y))
}

primary_dat <- analysis_dat[model_complete, ]


# ---- Hierarchical model M1-M5 ------------------------------------

models <- lapply(model_terms, function(terms) fit_logit(primary_dat, terms))

block_tests <- imap_dfr(blocks, function(terms, m) {
  wald_test(models[[m]], terms, paste0(m, " adds: ", block_labels[[m]])) %>%
    mutate(model = m, n = nrow(primary_dat), events = sum(primary_dat$ai_support_binary), .before = 1)
})
coefficients <- imap_dfr(models, ~ mutate(odds_ratios(.x), model = .y, .before = 1))
global_tests <- imap_dfr(models, ~ mutate(overall_tests(.x, model_terms[[.y]]), model = .y, .before = 1))

diagnostics <- bind_rows(
  imap_dfr(models, ~ tibble(model = .y, item = c("coefficients", "converged", "maximum SE"),
                            value = c(length(coef(.x)), .x$converged, max(sqrt(diag(vcov(.x))))))),
  tibble(model = "M5", item = "correlation of Oct7_Total_14 with Iran relocation",
         value = cor(primary_dat$oct7_total_14, primary_dat$iran_relocated))
)
vif <- bind_rows(
  mutate(vif_table(primary_dat, model_terms$M3), model = "M3", .before = 1),
  mutate(vif_table(primary_dat, model_terms$M5), model = "M5", .before = 1)
)


# ---- Secondary model: general AI users ---------------------------

secondary_dat <- primary_dat %>%
  filter(general_ai_user == 1) %>%
  mutate(general_ai_frequency = droplevels(relevel(ai_general_frequency, "A few times")))
secondary_terms <- c(model_terms$M5, "general_ai_frequency")
secondary <- fit_logit(secondary_dat, secondary_terms)

secondary_coefficients <- odds_ratios(secondary)
secondary_tests <- bind_rows(
  overall_tests(secondary, secondary_terms),
  wald_test(secondary, "general_ai_frequency", "General AI-use frequency")
)
secondary_sample <- secondary_dat %>%
  count(general_ai_frequency, name = "n") %>%
  mutate(users = as.vector(tapply(secondary_dat$ai_support_binary, secondary_dat$general_ai_frequency, sum)))
secondary_vif <- vif_table(secondary_dat, secondary_terms)


# ---- Sensitivity checks ------------------------------------------

component_terms <- c(blocks$M1, "phq_z", "gad_z", "pss_z", blocks$M3)
component_model <- fit_logit(primary_dat, component_terms)
components <- odds_ratios(component_model, c("phq_z", "gad_z", "pss_z", blocks$M3))
component_joint <- wald_test(component_model, c("phq_z", "gad_z", "pss_z"), "PHQ-9, GAD-7 and PSS-10 jointly")
component_correlations <- as_tibble(cor(primary_dat[c("phq", "gad", "pss")]), rownames = "measure")

refit <- function(label, data, terms) {
  odds_ratios(fit_logit(data, terms), focal_terms) %>%
    mutate(check = label, n = nrow(data), events = sum(data$ai_support_binary), .before = 1)
}
consistent <- filter(primary_dat, ai_logic_inconsistent == 0)

sensitivity <- bind_rows(
  coefficients %>% filter(model %in% c("M3", "M5"), term %in% focal_terms) %>%
    transmute(check = paste0(model, ", main"), n = nrow(primary_dat),
              events = sum(primary_dat$ai_support_binary), term, OR, CI_lower, CI_upper, p_value),
  refit("M3, inconsistent responders excluded", consistent, model_terms$M3),
  refit("M5, inconsistent responders excluded", consistent, model_terms$M5),
  refit("M5, administered the PCL-5 only", filter(primary_dat, pcl_routed == 0), model_terms$M5)
)

write_analysis(
  list(
    Block_Tests = block_tests,
    Coefficients = coefficients,
    Global_Tests = global_tests,
    Diagnostics = diagnostics,
    VIF = vif,
    Secondary_Coefficients = secondary_coefficients,
    Secondary_Tests = secondary_tests,
    Secondary_Sample = secondary_sample,
    Secondary_VIF = secondary_vif,
    Components = components,
    Components_Joint_Test = component_joint,
    Components_Correlations = component_correlations,
    Sensitivity = sensitivity
  ),
  "Models.xlsx"
)
