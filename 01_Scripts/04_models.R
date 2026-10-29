# 04_models.R
# Logistic regressions of any AI emotional-support use. Continuous
# predictors are linear on the log-odds scale (age per year, the rest per
# sample SD).
#   Primary     Five-step hierarchical model on one complete-case sample:
#               M1 demographics, M2 + distress, M3 + loneliness and social
#               support, M4 + PCL-5, M5 + trauma exposure (ACE-10, LEC-5
#               direct, October 7 index).
#   Secondary   M5 + general AI-use frequency among general AI users.
#   Sensitivity Distress components (M3); without the inconsistent
#               responders (M3, M5); M5 among participants administered
#               the PCL-5.
# Output: Models.xlsx.

source("01_Scripts/00_setup.R")

primary_dat <- filter(readRDS(analysis_file)$participants, model_sample)

blocks <- list(
  M1 = c("age", "gender", "education", "income", "religion_binary"),
  M2 = "distress_z",
  M3 = c("loneliness_z", "social_support_z"),
  M4 = "pcl_z",
  M5 = c("ace_z", "lec_direct_z", "oct7_z")
)
block_labels <- c(
  M1 = "Demographic covariates", M2 = "Psychological distress",
  M3 = "Loneliness and perceived social support", M4 = "PTSD symptoms (PCL-5)",
  M5 = "Trauma exposure (ACE-10, LEC-5 direct, October 7 index)"
)
model_terms <- setNames(lapply(seq_along(blocks), function(i) unlist(blocks[1:i], use.names = FALSE)), names(blocks))
focal_terms <- unlist(blocks[-1], use.names = FALSE)


# ---- Hierarchical model M1-M5 ------------------------------------

models <- lapply(model_terms, function(terms) fit_logit(primary_dat, terms))

block_tests <- imap_dfr(blocks, function(terms, m) {
  wald_test(models[[m]], terms, paste0(m, " adds: ", block_labels[[m]])) %>%
    mutate(model = m, n = nrow(primary_dat), events = sum(primary_dat$ai_support_binary), .before = 1)
})
coefficients <- imap_dfr(models, ~ mutate(odds_ratios(.x), model = .y, .before = 1))
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

secondary_tests <- bind_rows(
  wald_test(secondary, blocks$M5, "Trauma exposure (joint)"),
  wald_test(secondary, "general_ai_frequency", "General AI-use frequency")
)
secondary_sample <- secondary_dat %>%
  group_by(general_ai_frequency) %>%
  summarise(n = n(), users = sum(ai_support_binary))
vif <- bind_rows(vif, mutate(vif_table(secondary_dat, secondary_terms), model = "Secondary", .before = 1))


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
    VIF = vif,
    Secondary_Coefficients = odds_ratios(secondary),
    Secondary_Tests = secondary_tests,
    Secondary_Sample = secondary_sample,
    Components = components,
    Components_Joint_Test = component_joint,
    Components_Correlations = component_correlations,
    Sensitivity = sensitivity
  ),
  "Models.xlsx"
)
