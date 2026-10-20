# 06_tables_figures.R
# Publication outputs, built only from the aggregate workbooks written
# by scripts 03-05:
#   main        Table 1, Table 2, Figure 1, Figure 2
#   supplement  Tables S1-S4
# plus one source-data workbook for each.

source(if (file.exists("01_Scripts/00_setup.R")) "01_Scripts/00_setup.R" else "00_setup.R")
use_packages("tidyr", "ggplot2", "patchwork", "officer", "flextable")

# Refuse to build from results older than the code that produced them.
inputs <- c(Descriptives.xlsx = "03_descriptives.R", Models.xlsx = "04_models.R",
            Intensive_Users.xlsx = "05_intensive_users.R")
for (f in names(inputs)) {
  path <- file.path(analysis_dir, f)
  sources <- file.path("01_Scripts", c(inputs[[f]], "00_setup.R", "01_prepare_data.R"))
  if (!file.exists(path) || any(file.mtime(sources) > file.mtime(path))) {
    stop(f, " is missing or older than its scripts; run the pipeline in order.")
  }
}
read_result <- function(file, sheet) read_excel(file.path(analysis_dir, file), sheet = sheet)

desc <- function(sheet) read_result("Descriptives.xlsx", sheet)
mod <- function(sheet) read_result("Models.xlsx", sheet)
intensive <- function(sheet) read_result("Intensive_Users.xlsx", sheet)

for (d in file.path(c(main_dir, main_dir, main_dir, supplement_dir, supplement_dir),
                    c("tables", "figures/previews", "source_data", "tables", "source_data"))) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}


# ---- Number formatting -------------------------------------------

f2 <- function(x, digits = 2) {
  mapply(function(v, d) formatC(v, digits = d, format = "f"), x, digits, USE.NAMES = FALSE)
}
comma <- function(n) formatC(n, format = "d", big.mark = ",")

# p in APA style. A p value just below .05 that would round to .050 is
# shown to four decimals so it cannot be misread as non-significant.
fp <- function(p) {
  vapply(p, function(x) {
    if (x < .001) return("< .001")
    sub("^0", "", formatC(x, digits = if (x >= .0495 && x < .05) 4 else 3, format = "f"))
  }, character(1))
}
fp_inline <- function(p) ifelse(startsWith(fp(p), "<"), paste("p", fp(p)), paste("p =", fp(p)))

# OR [95% CI]; three decimals when a bound rounds to 1.00 at two.
f_or <- function(or, lo, hi, digits = 2) {
  digits <- ifelse(f2(lo) == "1.00" | f2(hi) == "1.00", 3, digits)
  paste0(f2(or, digits), " [", f2(lo, digits), ", ", f2(hi, digits), "]")
}
f_or_p <- function(or, lo, hi, p, digits = 2) paste0(f_or(or, lo, hi, digits), "\n", fp_inline(p))
f_wald <- function(F, df1, df2) paste0("F(", df1, ", ", round(df2), ") = ", f2(F))
f_ci1 <- function(est, lo, hi) {
  z <- function(x) f2(ifelse(round(x, 1) == 0, 0, x), 1)
  paste0(z(est), " [", z(lo), ", ", z(hi), "]")
}


# ---- Word tables -------------------------------------------------

pub_table <- function(data, widths, align, size, sections = integer(0), indent = integer(0)) {
  ft <- flextable(data) %>%
    theme_booktabs() %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = size, part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "header") %>%
    valign(valign = "center", part = "all") %>%
    padding(padding.top = 3.5, padding.bottom = 3.5, padding.left = 5, padding.right = 5, part = "all") %>%
    set_table_properties(layout = "fixed", width = 1, align = "center") %>%
    width(j = seq_along(widths), width = widths)
  for (j in seq_along(align)) ft <- align(ft, j = j, align = align[j], part = "body")
  for (i in sections) ft <- merge_at(ft, i = i, j = seq_along(widths), part = "body")
  if (length(sections) > 0) {
    ft <- ft %>% bg(i = sections, bg = "#E9EFF5", part = "body") %>% bold(i = sections, part = "body") %>%
      align(i = sections, j = 1, align = "left", part = "body")
  }
  if (length(indent) > 0) ft <- padding(ft, i = indent, j = 1, padding.left = 18, part = "body")
  ft
}

text_par <- function(text, size, bold = FALSE, italic = FALSE, top = 0, bottom = 0) {
  fpar(ftext(text, fp_text(font.family = "Times New Roman", font.size = size, bold = bold, italic = italic)),
       fp_p = fp_par(padding.top = top, padding.bottom = bottom))
}

# One Word document: heading, one or more (optionally titled) tables, note.
write_table_doc <- function(path, number, title, tables, note, landscape = FALSE) {
  margin <- if (landscape) c(0.42, 0.55) else c(0.65, 0.7)
  doc <- read_docx() %>%
    body_set_default_section(prop_section(
      page_size = page_size(orient = if (landscape) "landscape" else "portrait"),
      page_margins = page_mar(top = margin[1], bottom = margin[1], left = margin[2], right = margin[2]),
      type = "continuous")) %>%
    body_add_fpar(text_par(number, 11, bold = TRUE, bottom = 2)) %>%
    body_add_fpar(text_par(title, 11, italic = TRUE, bottom = 5))
  panels <- names(tables) %||% rep("", length(tables))
  for (i in seq_along(tables)) {
    if (nzchar(panels[i])) doc <- body_add_fpar(doc, text_par(panels[i], 10, bold = TRUE, top = 6, bottom = 3))
    doc <- body_add_flextable(doc, tables[[i]], align = "center")
  }
  doc <- body_add_fpar(doc, fpar(
    ftext("Note. ", fp_text(font.family = "Times New Roman", font.size = 9, italic = TRUE)),
    ftext(note, fp_text(font.family = "Times New Roman", font.size = 9)),
    fp_p = fp_par(padding.top = 5)))
  print(doc, target = path)
}

section_row <- function(label, columns) {
  as_tibble(setNames(as.list(c(label, rep("", length(columns) - 1))), columns)) %>%
    mutate(row_type = "section", .before = 1)
}


# ---- Table 1 -----------------------------------------------------

summaries <- desc("Table1_Summaries")
unadjusted <- desc("Unadjusted_ORs")
group_n <- summaries %>% filter(variable == "gender_full") %>% group_by(group) %>% summarise(n = sum(n))
group_names <- c("Overall", "Non-users", "Users")
t1_cols <- c("Characteristic", "Overall", "Non_users", "Users", "Unadjusted_OR")

t1_or <- function(variable, level = NA) {
  row <- unadjusted[unadjusted$variable == variable & (if (is.na(level)) is.na(unadjusted$level) else unadjusted$level %in% level), ]
  stopifnot(nrow(row) == 1)
  f_or(row$OR, row$CI_lower, row$CI_upper)
}
t1_cells <- function(variable, level = NA) {
  s <- summaries[summaries$variable == variable & (if (is.na(level)) is.na(summaries$level) else summaries$level %in% level), ]
  s <- s[match(group_names, s$group), ]
  if (is.na(level)) {
    mean <- ifelse(round(s$mean, 2) == 0, 0, s$mean)
    paste0(f2(mean), " (", f2(s$sd), ")")
  } else {
    paste0(s$n, " (", f2(s$pct, 1), "%)")
  }
}
t1_row <- function(label, cells, or, type = "regular") {
  tibble(row_type = type, Characteristic = label, Overall = cells[1], Non_users = cells[2], Users = cells[3],
         Unadjusted_OR = or)
}
t1_continuous <- function(variable, label, or_variable) t1_row(label, t1_cells(variable), t1_or(or_variable))
t1_categories <- function(variable, levels, or_variable = NULL, reference = NULL) {
  map_dfr(levels, function(l) {
    or <- if (is.null(or_variable)) "" else if (l == reference) "1.00 (reference)" else t1_or(or_variable, l)
    t1_row(if (identical(l, reference)) paste0(l, " (reference)") else l, t1_cells(variable, l), or, "category")
  })
}

table1_rows <- bind_rows(
  section_row("Demographics", t1_cols),
  t1_continuous("age", "Age, years", "age_decades"),
  section_row("Gender", t1_cols),
  t1_categories("gender_full", c("Female", "Male", "Other"), "gender_model", "Female"),
  section_row("Education", t1_cols),
  t1_categories("education", c("Primary", "Secondary", "Post-secondary non-academic", "Bachelor's degree",
                               "Master's degree or higher"), "education", "Secondary"),
  section_row("Income", t1_cols),
  t1_categories("income", c("Much below average", "Below average", "About average", "Above average",
                            "Much above average"), "income", "About average"),
  section_row("Religious affiliation", t1_cols),
  t1_categories("religion_full", c("Jewish", "Muslim", "Christian", "Druze", "Atheist / no religion", "Other"),
                "religion_table", "Jewish"),
  section_row("General AI-use frequency, past 3 months", t1_cols),
  t1_categories("ai_general_frequency", c("Not at all", "A few times", "Several times a month",
                                          "Several times a week", "Almost every day / every day")),
  section_row("Psychological and interpersonal", t1_cols),
  t1_continuous("distress_z", "Psychological distress, composite z score", "distress_z"),
  t1_continuous("loneliness", "Loneliness, UCLA-3 (3-9)", "loneliness_z"),
  t1_continuous("social_support", "Perceived social support, MOS-SS (0-100)", "social_support_z"),
  section_row("PTSD symptoms and trauma exposure", t1_cols),
  t1_continuous("pcl", "PTSD symptoms, PCL-5 (0-80)", "pcl_z"),
  t1_continuous("ace", "Childhood adversity, ACE-10 (0-10)", "ace_z"),
  t1_continuous("lec_direct", "Adulthood exposure, LEC-5 direct events (0-17)", "lec_direct_z"),
  t1_continuous("oct7_total_14", "Since October 7, Oct7_Total_14 (0-14)", "oct7_z"),
  t1_row("Iran war: relocated from home", t1_cells("iran_relocated", "1"), t1_or("iran_relocated"))
)
table1 <- select(table1_rows, -row_type)

table1_ft <- pub_table(table1, c(2.4, 1.15, 1.15, 1.15, 1.2), c("left", rep("center", 4)), 8.5,
                       which(table1_rows$row_type == "section"), which(table1_rows$row_type == "category")) %>%
  padding(padding.top = 2, padding.bottom = 2, part = "all") %>%
  set_header_labels(
    Characteristic = "Characteristic",
    Overall = paste0("Overall (N = ", comma(group_n$n[group_n$group == "Overall"]), ")"),
    Non_users = paste0("No AI emotional-support use (n = ", comma(group_n$n[group_n$group == "Non-users"]), ")"),
    Users = paste0("AI emotional-support use (n = ", comma(group_n$n[group_n$group == "Users"]), ")"),
    Unadjusted_OR = "Unadjusted OR [95% CI]"
  )

write_table_doc(
  file.path(main_dir, "tables", "Table_1_Participant_Characteristics.docx"), "Table 1",
  "Participant Characteristics and Unadjusted Associations With AI Emotional-Support Use",
  list(table1_ft),
  paste0(
    "Values are mean (SD) or n (%). Unadjusted ORs come from univariable logistic regressions of AI ",
    "emotional-support use on each characteristic: per 10 years for age, per SD for scales and exposure ",
    "counts (SDs in the Overall column), and versus the stated reference category otherwise. They are ",
    "unadjusted associations, not independent predictors. Loneliness was available for ",
    comma(unadjusted$n[unadjusted$variable == "loneliness_z"]), " participants; all other characteristics ",
    "were complete. General AI-use frequency is shown descriptively without ORs, because emotional-support ",
    "use is nested within general AI use. The 533 participants who reported no index trauma skipped the ",
    "PCL-5 and are scored 0. Oct7_Total_14 counts 14 exposure indicators, including relocation during the Iran war."
  )
)


# ---- Table 2 -----------------------------------------------------

coefficients <- mod("Coefficients")
block_tests <- mod("Block_Tests")
model_names <- paste0("M", 1:5)
t2_cols <- c("Predictor", model_names)

t2_row <- function(term, label, digits = 2) {
  cells <- map_chr(model_names, function(m) {
    r <- coefficients[coefficients$model == m & coefficients$term == term, ]
    if (nrow(r) == 0) "" else if (digits == 3) {
      paste0(paste0(f2(r$OR, 3), " [", f2(r$CI_lower, 3), ", ", f2(r$CI_upper, 3), "]"), "\n", fp_inline(r$p_value))
    } else f_or_p(r$OR, r$CI_lower, r$CI_upper, r$p_value)
  })
  tibble(row_type = "regular", Predictor = label, !!!setNames(as.list(cells), model_names))
}
t2_model_row <- function(label, cells) {
  tibble(row_type = "block", Predictor = label, !!!setNames(as.list(cells), model_names))
}

table2_rows <- bind_rows(
  section_row("Demographics", t2_cols),
  t2_row("age", "Age, per year", digits = 3),
  t2_row("gender_modelMale", "Male (vs female)"),
  t2_row("gender_modelOther", "Other gender (vs female)"),
  t2_row("educationPrimary", "Education: primary (vs secondary)"),
  t2_row("educationPost-secondary non-academic", "Education: post-secondary non-academic"),
  t2_row("educationBachelor's degree", "Education: bachelor's degree"),
  t2_row("educationMaster's degree or higher", "Education: master's degree or higher"),
  t2_row("incomeMuch below average", "Income: much below average (vs about average)"),
  t2_row("incomeBelow average", "Income: below average"),
  t2_row("incomeAbove average", "Income: above average"),
  t2_row("incomeMuch above average", "Income: much above average"),
  t2_row("religion_binaryNon-Jewish", "Non-Jewish (vs Jewish)"),
  section_row("Psychological and interpersonal (OR per SD)", t2_cols),
  t2_row("distress_z", "Psychological distress"),
  t2_row("loneliness_z", "Loneliness"),
  t2_row("social_support_z", "Perceived social support"),
  section_row("PTSD symptoms and trauma exposure (OR per SD)", t2_cols),
  t2_row("pcl_z", "PTSD symptoms (PCL-5)"),
  t2_row("ace_z", "Childhood adversity (ACE-10)"),
  t2_row("lec_direct_z", "Adulthood exposure (LEC-5 direct)"),
  t2_row("oct7_z", "Since October 7 (Oct7_Total_14)"),
  t2_row("iran_z", "Iran war (relocated)"),
  section_row("Model", t2_cols),
  t2_model_row("Block added at this step (Wald test)",
               paste0(f_wald(block_tests$F, block_tests$df_num, block_tests$df_den), "\n", fp_inline(block_tests$p_value))),
  t2_model_row("N (events)", paste0(comma(block_tests$n), " (", comma(block_tests$events), ")"))
)
table2 <- select(table2_rows, -row_type)

table2_ft <- pub_table(table2, c(2.35, rep(1.48, 5)), c("left", rep("center", 5)), 8,
                       which(table2_rows$row_type == "section")) %>%
  padding(padding.top = 1.5, padding.bottom = 1.5, part = "all") %>%
  set_header_labels(Predictor = "Predictor", M1 = "M1\nDemographics", M2 = "M2\n+ Distress",
                    M3 = "M3\n+ Loneliness, social support", M4 = "M4\n+ PCL-5", M5 = "M5\n+ Trauma exposure")

write_table_doc(
  file.path(main_dir, "tables", "Table_2_Hierarchical_Models.docx"), "Table 2",
  paste("Hierarchical Logistic Regression of AI Emotional-Support Use on Distress,",
        "Interpersonal Factors, PTSD Symptoms, and Trauma Exposure"),
  list(table2_ft),
  paste0(
    "Hierarchical logistic regression on the same complete-case sample at every step; M1-M3 form the ",
    "original psychological model and M4-M5 the trauma extension. Cells give adjusted OR [95% CI] and p. ",
    "All continuous predictors are linear on the log-odds scale: age per year, all other continuous ",
    "predictors per SD. The block row tests the terms added at each step (for M1, all demographic terms). ",
    "Participants who skipped the PCL-5 (no index event; n = 532 in this sample) are scored 0. ",
    "Oct7_Total_14 includes the Iran-relocation item, so in M5 the Iran OR is the association of ",
    "relocation beyond its contribution to the October 7 count."
  ),
  landscape = TRUE
)


# ---- Figures -----------------------------------------------------
# Shared design: one sans-serif type scale, slate text, light
# vertical gridlines, bold left-aligned family headings, and the
# validated blue/orange pair (colour-blind safe, distinct in grey via
# marker shape).

blue <- "#2C6FAE"
orange <- "#D9772B"
ink <- "#263238"
muted <- "#56636B"

short_labels <- c(
  help_moderate = "At least moderately helpful", help_a_lot = "Helped a lot or a great deal",
  MH_AI_Q04_R00 = "Helped calm down or cope", MH_AI_Q04_R01 = "Helped understand feelings",
  MH_AI_Q04_R02 = "Shared what I couldn't tell people", MH_AI_Q04_R03 = "Used it instead of reaching out",
  MH_AI_Q04_R04 = "Relied on it repeatedly for comfort", MH_AI_Q04_R05 = "Responses inaccurate or unhelpful",
  MH_AI_Q04_R06 = "Felt worse or misunderstood",
  MH_AI_Q05_R01 = "Anxiety, worry, panic", MH_AI_Q05_R02 = "Stress, overload, burnout",
  MH_AI_Q05_R03 = "Low mood, depression", MH_AI_Q05_R04 = "Overwhelm, emotion regulation",
  MH_AI_Q05_R05 = "Sleep difficulties", MH_AI_Q05_R06 = "Loneliness, need for support",
  MH_AI_Q05_R07 = "Relationships and conflicts", MH_AI_Q05_R08 = "Trauma, grief, difficult memories",
  MH_AI_Q05_R09 = "Body image, eating, harmful habits", MH_AI_Q05_R10 = "Personal dilemmas, decisions",
  MH_AI_Q05_R11 = "Professional help (before or after)"
)
family_labels <- c(Helpfulness = "Perceived helpfulness", Experiences = "Experiences of AI emotional support",
                   Topics = "Topics discussed with AI")

figure_theme <- theme_classic(base_size = 8.5) +
  theme(
    panel.grid.major.x = element_line(colour = "#E6EBEF", linewidth = 0.35),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text = element_text(colour = ink),
    axis.title.x = element_text(colour = muted, size = 8, margin = margin(t = 5)),
    plot.title = element_text(face = "bold", size = 8.8, colour = ink, margin = margin(b = 3)),
    plot.title.position = "plot",
    legend.text = element_text(size = 8, colour = ink),
    plot.margin = margin(4, 6, 4, 4)
  )
caption_theme <- theme(plot.caption = element_text(size = 7, colour = muted, hjust = 0, lineheight = 1.15))
hide_x <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank())

save_figure <- function(plot, name, width, height) {
  ggsave(file.path(main_dir, "figures", "previews", paste0(name, ".png")), plot, width = width, height = height, dpi = 600)
  # Quartz (macOS) and Cairo both embed fonts in the PDF.
  device <- if (capabilities("aqua")) function(filename, ...) grDevices::quartz(file = filename, type = "pdf", ...) else grDevices::cairo_pdf
  ggsave(file.path(main_dir, "figures", paste0(name, ".pdf")), plot, width = width, height = height, device = device)
}

# Rows ordered by size within each family (largest at the top).
order_items <- function(d, value) {
  d %>% group_by(family) %>% arrange({{ value }}, .by_group = TRUE) %>% ungroup() %>%
    mutate(item = factor(item, levels = unique(item)))
}


# Figure 1: experiences and topics among all emotional-support users.
experiences <- desc("AI_Experiences")
figure1_data <- experiences %>%
  filter(family %in% c("Experiences", "Topics"), variable %in% names(short_labels)) %>%
  mutate(family = factor(family_labels[family], family_labels[c("Experiences", "Topics")]),
         item = short_labels[variable]) %>%
  order_items(pct)
n_users <- unique(experiences$n_valid)
stopifnot(length(n_users) == 1, nrow(figure1_data) == 18)

figure1_row <- function(fam, bottom) {
  d <- droplevels(filter(figure1_data, family == fam))
  ggplot(d, aes(y = item)) +
    geom_segment(aes(x = CI_lower, xend = CI_upper, yend = item), colour = blue, linewidth = 0.55, alpha = 0.55) +
    geom_point(aes(x = pct), colour = blue, size = 2.3) +
    geom_text(aes(x = 69, label = paste0(f2(pct, 1), "%")), hjust = 1, size = 2.5, colour = muted) +
    scale_x_continuous(limits = c(0, 69), breaks = seq(0, 60, 20), expand = expansion(mult = c(0.01, 0))) +
    labs(x = if (bottom) "Emotional-support users reporting the item (%), 95% CI" else NULL, y = NULL, title = fam) +
    figure_theme + (if (bottom) theme() else hide_x)
}
figure1 <- wrap_plots(
  map2(levels(figure1_data$family), c(FALSE, TRUE), figure1_row),
  ncol = 1, heights = as.numeric(table(figure1_data$family)) + 1.2
) +
  plot_annotation(
    caption = paste0(
      "AI emotional-support users (n = ", comma(n_users), "); multiple responses allowed. Points are observed ",
      "percentages with unadjusted logit-based 95% CIs.\nTopics: all response options except the residual ‘other’ ",
      "category (Table S1)."
    ),
    theme = caption_theme
  )
save_figure(figure1, "Figure_1_AI_Experiences_and_Topics", 7.2, 5.6)


# Figure 2: daily vs less frequent users among daily general AI users.
comparisons <- intensive("Experience_Comparisons")
figure2_data <- comparisons %>%
  filter(variable %in% names(short_labels)) %>%
  mutate(family = factor(family_labels[family], family_labels), item = short_labels[variable],
         holm_significant = factor(holm_p < .05, c(TRUE, FALSE))) %>%
  order_items(difference_pct)
n_daily <- unique(comparisons$n_daily)
n_less <- unique(comparisons$n_less)
stopifnot(length(n_daily) == 1, length(n_less) == 1, nrow(figure2_data) == 20)

group_labels <- c(Daily = paste0("Daily (n = ", n_daily, ")"), `Less frequent` = paste0("Less frequent (n = ", n_less, ")"))

figure2_row <- function(fam, bottom, top) {
  d <- droplevels(filter(figure2_data, family == fam))
  points <- bind_rows(transmute(d, item, group = "Daily", pct = pct_daily),
                      transmute(d, item, group = "Less frequent", pct = pct_less))
  left <- ggplot(d, aes(y = item)) +
    geom_segment(aes(x = pct_less, xend = pct_daily, yend = item), colour = "#C9D2D9", linewidth = 1.5, lineend = "round") +
    geom_point(data = points, aes(x = pct, colour = group, shape = group), size = 2.3, stroke = 0.9) +
    scale_colour_manual(values = c(Daily = blue, `Less frequent` = orange), labels = group_labels, name = NULL) +
    scale_shape_manual(values = c(Daily = 16, `Less frequent` = 18), labels = group_labels, name = NULL) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = if (bottom) "A.  Users reporting the item (%)" else NULL, y = NULL, title = fam) +
    figure_theme + (if (bottom) theme() else hide_x) + (if (top) theme() else guides(colour = "none", shape = "none"))
  right <- ggplot(d, aes(y = item)) +
    geom_vline(xintercept = 0, colour = "#8A979F", linewidth = 0.4) +
    geom_segment(aes(x = difference_CI_lower, xend = difference_CI_upper, yend = item), colour = blue, linewidth = 0.55) +
    geom_point(aes(x = difference_pct, fill = holm_significant), shape = 21, size = 2.2, colour = blue, stroke = 0.8) +
    geom_text(aes(x = 66, label = fp(holm_p)), hjust = 1, size = 2.5, colour = muted) +
    scale_fill_manual(values = c(`TRUE` = blue, `FALSE` = "white"), drop = FALSE, guide = "none") +
    scale_x_continuous(limits = c(-20, 66), breaks = c(-20, 0, 20, 40)) +
    labs(x = if (bottom) "B.  Difference, daily minus less frequent\n(percentage points, unadjusted 95% CI)" else NULL,
         y = NULL, title = if (top) "Holm p" else " ") +
    figure_theme + theme(axis.text.y = element_blank(), plot.title = element_text(hjust = 1, size = 8, colour = muted)) +
    (if (bottom) theme() else hide_x)
  list(left, right)
}
families2 <- levels(figure2_data$family)
figure2 <- wrap_plots(
  unlist(pmap(list(families2, seq_along(families2) == 3, seq_along(families2) == 1), figure2_row), recursive = FALSE),
  ncol = 2, widths = c(1.45, 1), heights = as.numeric(table(figure2_data$family)) + 1.2
) +
  plot_layout(guides = "collect") +
  plot_annotation(
    caption = paste0(
      "AI emotional-support users with daily general AI use. Items: all comparisons in Table S4 except the residual ",
      "‘other’ topic.\nPanel B: 95% CIs are not adjusted for multiplicity; printed p values are Holm-adjusted within ",
      "each item family (filled markers, p < .05; open, p ≥ .05).\nCross-sectional, descriptive group differences; not causal effects."
    ),
    theme = caption_theme
  ) &
  theme(legend.position = "bottom", legend.justification = "left")
save_figure(figure2, "Figure_2_Intensive_Emotional_Support_Users", 7.2, 7.0)


# ---- Table S1: AI experiences among emotional-support users ------

s1_labels <- c(
  MH_AI_Q04_R00 = "Helped calm down or cope with distress", MH_AI_Q04_R01 = "Helped understand feelings or gain insight",
  MH_AI_Q04_R02 = "Shared things I couldn't tell other people", MH_AI_Q04_R03 = "Used AI instead of reaching out to someone",
  MH_AI_Q04_R04 = "Repeatedly relied on AI for comfort", MH_AI_Q04_R05 = "Responses were inaccurate or unhelpful",
  MH_AI_Q04_R06 = "Felt worse, confused, or misunderstood"
)
s1_sections <- c(Helpfulness = "Perceived helpfulness", Experiences = "Experiences", Topics = "Topics discussed")
s1_cols <- c("Domain", "Item", "n", "percentage_ci")
tableS1_rows <- map_dfr(names(s1_sections), function(fam) {
  d <- filter(experiences, family == fam)
  bind_rows(
    section_row(s1_sections[[fam]], s1_cols),
    tibble(row_type = "regular", Domain = "", Item = coalesce(s1_labels[d$variable], d$item),
           n = as.character(d$n_selected), percentage_ci = f_ci1(d$pct, d$CI_lower, d$CI_upper))
  )
})
tableS1 <- select(tableS1_rows, -row_type)

write_table_doc(
  file.path(supplement_dir, "tables", "Table_S1_AI_Experience_Descriptives.docx"), "Table S1",
  "Descriptive Characteristics of AI Emotional-Support Use",
  list(pub_table(tableS1, c(1.75, 4.65, 0.75, 2.15), c("left", "left", "center", "center"), 8.9,
                 which(tableS1_rows$row_type == "section")) %>%
         set_header_labels(percentage_ci = "% (95% CI)")),
  paste0("For perceived helpfulness, experiences, and topics, N = ", comma(n_users),
         ". The n column reports the observed number of participants."),
  landscape = TRUE
)


# ---- Table S2: sensitivity checks --------------------------------

term_labels <- c(
  phq_z = "Depression (PHQ-9)", gad_z = "Anxiety (GAD-7)", pss_z = "Perceived stress (PSS-10)",
  distress_z = "Psychological distress", loneliness_z = "Loneliness", social_support_z = "Perceived social support",
  pcl_z = "PTSD symptoms (PCL-5)", ace_z = "Childhood adversity (ACE-10)",
  lec_direct_z = "Adulthood exposure (LEC-5 direct)", oct7_z = "Since October 7 (Oct7_Total_14)",
  iran_z = "Iran war (relocated)"
)
sensitivity <- mod("Sensitivity")

wide_panel <- function(checks) {
  d <- filter(sensitivity, check %in% names(checks)) %>%
    mutate(column = checks[check], Predictor = term_labels[term], cell = f_or_p(OR, CI_lower, CI_upper, p_value))
  terms <- names(term_labels)[names(term_labels) %in% d$term]
  cells <- d %>% select(Predictor, column, cell) %>% pivot_wider(names_from = column, values_from = cell) %>%
    slice(match(term_labels[terms], Predictor))
  n_row <- distinct(d, column, n, events)
  bind_rows(cells, tibble(Predictor = "N (events)",
                          !!!setNames(as.list(paste0(comma(n_row$n), " (", comma(n_row$events), ")")), n_row$column))) %>%
    select(Predictor, all_of(unname(checks))) %>%
    mutate(across(everything(), ~ replace_na(.x, "")))
}

components <- mod("Components")
s2a <- tibble(Predictor = term_labels[components$term],
              `OR per SD [95% CI]` = f_or(components$OR, components$CI_lower, components$CI_upper),
              p = fp(components$p_value))
s2b <- wide_panel(c(`M3, main` = "M3, all", `M3, inconsistent responders excluded` = "M3, excluded",
                    `M5, main` = "M5, all", `M5, inconsistent responders excluded` = "M5, excluded"))
s2c <- wide_panel(c(`M5, main` = "Main (stored scoring)", `M5, administered the PCL-5 only` = "Administered PCL-5 only"))

write_table_doc(
  file.path(supplement_dir, "tables", "Table_S2_Sensitivity_Analyses.docx"), "Table S2",
  "Sensitivity Checks for the Hierarchical Logistic Regression",
  list(
    `Panel A. Distress components entered separately in M3` =
      pub_table(s2a, c(3.2, 2.2, 0.9), c("left", "center", "center"), 9.5),
    `Panel B. Excluding logically inconsistent responders (M3 and M5)` =
      pub_table(s2b, c(2.3, rep(1.2, 4)), c("left", rep("center", 4)), 8.5),
    `Panel C. PCL-5 routing (M5)` =
      pub_table(s2c, c(2.9, 1.8, 1.8), c("left", "center", "center"), 8.5)
  ),
  paste0(
    "OR = odds ratio; CI = confidence interval; all predictors per SD. Panel A replaces the distress composite ",
    "in M3 with its three components (joint Wald test p ", fp(mod("Components_Joint_Test")$p_value),
    "); covariates as in M3. Panel B refits M3 and M5 without the 15 respondents reporting emotional-support use ",
    "but no general AI use. Panel C refits M5 among participants administered the PCL-5, excluding those routed ",
    "past it because they reported no index event (scored 0 in the main analysis)."
  )
)


# ---- Table S3: secondary model among general AI users ------------

secondary <- mod("Secondary_Coefficients")
secondary_tests <- mod("Secondary_Tests")
secondary_sample <- mod("Secondary_Sample")
s3_cols <- c("Predictor", "Estimate", "p")
s3_or <- function(term, label) {
  r <- secondary[secondary$term == term, ]
  tibble(row_type = "regular", Predictor = label, Estimate = f_or(r$OR, r$CI_lower, r$CI_upper), p = fp(r$p_value))
}
s3_wald <- function(test, label) {
  r <- secondary_tests[secondary_tests$test == test, ]
  tibble(row_type = "regular", Predictor = label, Estimate = f_wald(r$F, r$df_num, r$df_den), p = fp(r$p_value))
}
tableS3_rows <- bind_rows(
  section_row("General AI-use frequency (vs a few times)", s3_cols),
  s3_wald("General AI-use frequency", "Overall"),
  s3_or("general_ai_frequencySeveral times a month", "Several times a month"),
  s3_or("general_ai_frequencySeveral times a week", "Several times a week"),
  s3_or("general_ai_frequencyAlmost every day / every day", "Almost every day / every day"),
  section_row("Psychological and interpersonal (OR per SD)", s3_cols),
  s3_or("distress_z", "Psychological distress"),
  s3_or("loneliness_z", "Loneliness"),
  s3_or("social_support_z", "Perceived social support"),
  section_row("PTSD symptoms and trauma exposure (OR per SD)", s3_cols),
  s3_or("pcl_z", "PTSD symptoms (PCL-5)"),
  s3_or("ace_z", "Childhood adversity (ACE-10)"),
  s3_or("lec_direct_z", "Adulthood exposure (LEC-5 direct)"),
  s3_or("oct7_z", "Since October 7 (Oct7_Total_14)"),
  s3_or("iran_z", "Iran war (relocated)"),
  s3_wald("Trauma exposure windows (joint)", "Exposure windows, joint test")
)
tableS3 <- select(tableS3_rows, -row_type)

write_table_doc(
  file.path(supplement_dir, "tables", "Table_S3_General_AI_Users_Model.docx"), "Table S3",
  "Secondary Model Among General AI Users, Additionally Adjusted for General AI-Use Frequency",
  list(pub_table(tableS3, c(3.4, 1.9, 0.8), c("left", "center", "center"), 9.5,
                 which(tableS3_rows$row_type == "section")) %>%
         set_header_labels(Estimate = "OR [95% CI] or Wald test")),
  paste0(
    "N = ", comma(sum(secondary_sample$n)), " participants reporting any general AI use (",
    comma(sum(secondary_sample$users)), " emotional-support users). The model contains every term of the final ",
    "hierarchical model (M5) plus general AI-use frequency; continuous predictors per SD; demographic estimates ",
    "are in the source data. It is conditional on general AI use and is a subset of the primary sample, not an ",
    "independent replication."
  )
)


# ---- Table S4: intensive emotional-support users -----------------

intensive_model <- intensive("Model")
check <- intensive("Model_Check")
check_value <- function(item) check$value[check$item == item]
group_sizes <- intensive("Group_Sizes")

s4a <- tibble(
  Predictor = c(age = "Age, per year", female = "Female (vs male)", distress_z = "Psychological distress, per SD",
                pcl_z = "PTSD symptoms (PCL-5), per SD", ace_z = "Childhood adversity (ACE-10), per SD",
                loneliness_z = "Loneliness, per SD", social_support_z = "Perceived social support, per SD")[intensive_model$term],
  `Adjusted OR [95% CI]` = f_or(intensive_model$OR, intensive_model$CI_lower, intensive_model$CI_upper),
  p = fp(intensive_model$p_value)
)
s4b <- comparisons %>%
  transmute(
    Family = family, Item = label,
    `Daily, n/N (%)` = paste0(k_daily, "/", n_daily, " (", f2(pct_daily, 1), ")"),
    `Less frequent, n/N (%)` = paste0(k_less, "/", n_less, " (", f2(pct_less, 1), ")"),
    `Difference, pp [95% CI]` = f_ci1(difference_pct, difference_CI_lower, difference_CI_upper),
    `Fisher p` = fp(fisher_p),
    `Holm p` = fp(holm_p)
  )

write_table_doc(
  file.path(supplement_dir, "tables", "Table_S4_Intensive_Emotional_Support_Users.docx"), "Table S4",
  "Daily Versus Less Frequent AI Emotional-Support Use Among Users With Daily General AI Use (Exploratory)",
  setNames(
    list(pub_table(s4a, c(3.2, 2.0, 0.9), c("left", "center", "center"), 9),
         pub_table(s4b, c(0.95, 2.75, 1.15, 1.15, 1.35, 0.75, 0.75), c("left", "left", rep("center", 5)), 8)),
    c(paste0("Panel A. Adjusted logistic regression of daily emotional-support use (N = ", check_value("N"), "; ",
             check_value("Daily users (events)"), " daily vs ", check_value("Less frequent users"), " less frequent)"),
      paste0("Panel B. Reported helpfulness, experiences, and topics: all 21 exploratory comparisons (N = ",
             n_daily + n_less, "; ", n_daily, " daily vs ", n_less, " less frequent)"))
  ),
  paste0(
    "Exploratory analyses. Of all ", comma(sum(group_sizes$n)), " emotional-support users, ",
    sum(group_sizes$n[group_sizes$daily == 1]), " reported daily ('almost every day / every day') use; ", n_daily,
    " of them also used AI daily in general, so inference is restricted to users with daily general AI use. ",
    "Panel A: one model of psychological and interpersonal characteristics (", check_value("Slope parameters") + 1,
    " parameters with the intercept; ", f2(check_value("Events per slope parameter"), 1),
    " events per slope parameter; c-statistic ", f2(check_value("c-statistic"), 3), "); the ",
    check_value("Excluded: gender 'Other' (none daily)"), " users reporting another gender (none daily) are ",
    "excluded from the model only. Panel B: percentage-point differences (daily minus less frequent) with ",
    "unadjusted Wald 95% CIs; Fisher exact p, and Holm-adjusted p within each item family (helpfulness 2, ",
    "experiences 7, topics 12). Descriptive and cross-sectional; not causal effects."
  ),
  landscape = TRUE
)


# ---- Source data and checks --------------------------------------

stopifnot(
  nrow(table1) == 41, nrow(table2) == 26, nrow(tableS1) == 24, nrow(s2a) == 5, nrow(s2b) == 9,
  nrow(s2c) == 9, nrow(tableS3) == 16, nrow(s4a) == 7, nrow(s4b) == 21
)

write_xlsx(list(Table_1 = table1, Table_2 = table2, Figure_1_Data = figure1_data, Figure_2_Data = figure2_data),
           file.path(main_dir, "source_data", "MAIN_Results_Tables.xlsx"))
write_xlsx(list(Table_S1 = tableS1, Table_S2_A = s2a, Table_S2_B = s2b, Table_S2_C = s2c, Table_S3 = tableS3,
                Table_S4_A = s4a, Table_S4_B = s4b),
           file.path(supplement_dir, "source_data", "SUPPLEMENT_Results_Tables.xlsx"))

message("Publication outputs written to ", main_dir, " and ", supplement_dir)
