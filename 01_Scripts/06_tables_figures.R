# 06_tables_figures.R
# Publication outputs, built only from the aggregate workbooks written
# by scripts 03-05:
#   main        Table 1, Table 2, Figure 1, Figure 2 (Holm-significant
#               comparisons only)
#   supplement  Tables S1-S4
# plus one source-data workbook for each.

source(if (file.exists("01_Scripts/00_setup.R")) "01_Scripts/00_setup.R" else "00_setup.R")
use_packages("tidyr", "ggplot2", "ggtext", "patchwork", "officer", "flextable")

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

# Negative values take a true minus sign (U+2212).
f2 <- function(x, digits = 2) {
  sub("^-", "\u2212", mapply(function(v, d) formatC(v, digits = d, format = "f"), x, digits, USE.NAMES = FALSE))
}
comma <- function(n) formatC(n, format = "d", big.mark = ",")

# p with a leading zero, as in the manuscript (blank when missing). A p value just below 0.05
# that would round to 0.050 is shown to four decimals so it cannot be misread as non-significant.
fp <- function(p) {
  vapply(p, function(x) {
    if (is.na(x)) return("")
    if (x < .001) return("<0.001")
    formatC(x, digits = if (x >= .0495 && x < .05) 4 else 3, format = "f")
  }, character(1))
}

# OR [95% CI]; three decimals when a bound rounds to 1.00 at two.
f_or <- function(or, lo, hi, digits = 2) {
  digits <- ifelse(f2(lo) == "1.00" | f2(hi) == "1.00", 3, digits)
  paste0(f2(or, digits), " [", f2(lo, digits), ", ", f2(hi, digits), "]")
}
f_wald <- function(F, df1, df2) paste0("F(", df1, ", ", round(df2), ") = ", f2(F))
f_ci1 <- function(est, lo, hi) {
  z <- function(x) f2(ifelse(round(x, 1) == 0, 0, x), 1)
  paste0(z(est), " [", z(lo), ", ", z(hi), "]")
}


# ---- Word tables -------------------------------------------------
# Row types: "section" (shaded heading across the table), "group" (a
# categorical variable's heading, with its reference category),
# "category" (indented under its group) and "regular".

pub_table <- function(data, widths, align, size, row_type = NULL) {
  row_type <- row_type %||% rep("regular", nrow(data))
  ft <- flextable(data) %>%
    theme_booktabs() %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = size, part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "header") %>%
    valign(valign = "center", part = "all") %>%
    padding(padding.top = 2, padding.bottom = 2, padding.left = 4, padding.right = 4, part = "all") %>%
    # Rows never split across pages; each table is kept with the
    # paragraph that follows it (the next panel or the note).
    set_table_properties(layout = "fixed", width = 1, align = "center",
                         opts_word = list(split = FALSE, keep_with_next = TRUE)) %>%
    width(j = seq_along(widths), width = widths)
  for (j in seq_along(align)) ft <- align(ft, j = j, align = align[j], part = "body")
  ft <- align(ft, j = 1, align = "left", part = "header")
  sections <- which(row_type == "section")
  groups <- which(row_type == "group")
  for (i in c(sections, groups)) ft <- merge_at(ft, i = i, j = seq_along(widths), part = "body")
  if (length(sections) > 0) {
    ft <- ft %>% bg(i = sections, bg = "#E9EFF5", part = "body") %>% bold(i = sections, part = "body") %>%
      align(i = sections, j = 1, align = "left", part = "body")
  }
  if (length(groups) > 0) ft <- ft %>% italic(i = groups, part = "body") %>% align(i = groups, j = 1, align = "left", part = "body")
  categories <- which(row_type == "category")
  if (length(categories) > 0) ft <- padding(ft, i = categories, j = 1, padding.left = 16, part = "body")
  ft
}

# Bold the p values below .05 in column j.
bold_p <- function(ft, j, p) {
  rows <- which(!is.na(p) & p < .05)
  if (length(rows) > 0) ft <- bold(ft, i = rows, j = j, part = "body")
  ft
}

# APA 7 italics for statistical symbols in table cells, panel titles
# and notes: N, n, M, SD, OR, F, p, c (c-statistic) and z (z score);
# a plural "s" stays upright.
stat_symbol <- "(?<![[:alnum:]])(N|n|M|SD|OR|F|p)(?=s?(?![[:alnum:]]))|(?<![[:alnum:]])c(?=[-\u2011]statistic)|(?<![[:alnum:]])z(?= scores?)"
stat_pieces <- function(text) {
  m <- gregexpr(stat_symbol, text, perl = TRUE)[[1]]
  if (m[1] == -1) return(list(list(text = text, italic = FALSE)))
  starts <- as.integer(m); ends <- starts + attr(m, "match.length") - 1
  cuts <- sort(unique(c(1, starts, ends + 1, nchar(text) + 1)))
  lapply(head(seq_along(cuts), -1), function(k) {
    list(text = substr(text, cuts[k], cuts[k + 1] - 1), italic = cuts[k] %in% starts)
  })
}
stat_ftexts <- function(text, props) {
  lapply(stat_pieces(text), function(piece) ftext(piece$text, update(props, italic = props$italic || piece$italic)))
}
italicize_symbols <- function(ft) {
  for (part in c("header", "body")) {
    cells <- ft[[part]]$content$content$data
    for (j in seq_len(ncol(cells))) for (i in seq_len(nrow(cells))) {
      value <- paste(cells[[i, j]]$txt, collapse = "")
      if (is.na(value) || !grepl(stat_symbol, value, perl = TRUE)) next
      chunks <- lapply(stat_pieces(value), function(piece) if (piece$italic) as_i(piece$text) else as_chunk(piece$text))
      ft <- compose(ft, i = i, j = j, part = part, value = do.call(as_paragraph, chunks))
    }
  }
  ft
}

text_par <- function(text, size, bold = FALSE, italic = FALSE, top = 0, bottom = 0) {
  props <- fp_text(font.family = "Times New Roman", font.size = size, bold = bold, italic = italic)
  do.call(fpar, c(stat_ftexts(text, props),
                  list(fp_p = fp_par(padding.top = top, padding.bottom = bottom, keep_with_next = TRUE))))
}

# One Word document: heading, one or more (optionally titled) tables, note.
write_table_doc <- function(path, number, title, tables, note, landscape = FALSE) {
  margin <- if (landscape) c(0.6, 0.55) else c(0.75, 0.7)
  doc <- read_docx() %>%
    body_set_default_section(prop_section(
      page_size = page_size(orient = if (landscape) "landscape" else "portrait"),
      page_margins = page_mar(top = margin[1], bottom = margin[1], left = margin[2], right = margin[2]),
      type = "continuous")) %>%
    body_add_fpar(text_par(number, 11, bold = TRUE, bottom = 2)) %>%
    body_add_fpar(text_par(title, 11, italic = TRUE, bottom = 5))
  panels <- names(tables) %||% rep("", length(tables))
  for (i in seq_along(tables)) {
    if (nzchar(panels[i])) doc <- body_add_fpar(doc, text_par(panels[i], 9.5, bold = TRUE, top = if (i > 1) 14 else 2, bottom = 3))
    doc <- body_add_flextable(doc, italicize_symbols(tables[[i]]), align = "center")
  }
  # Justified notes: keep "n = 532", "p values" and
  # "c-statistic" from breaking across lines.
  note <- gsub(" ([=<]) ", " \\1 ", note)
  note <- gsub("p values", "p values", note, fixed = TRUE)
  note <- gsub("c-statistic", "c‑statistic", note, fixed = TRUE)
  doc <- body_add_fpar(doc, do.call(fpar, c(
    list(ftext("Note. ", fp_text(font.family = "Times New Roman", font.size = 9, italic = TRUE))),
    stat_ftexts(note, fp_text(font.family = "Times New Roman", font.size = 9)),
    list(fp_p = fp_par(text.align = "justify", padding.top = 5, line_spacing = 1.1)))))
  print(doc, target = path)
}

section_row <- function(label, columns, type = "section") {
  as_tibble(setNames(as.list(c(label, rep("", length(columns) - 1))), columns)) %>%
    mutate(row_type = type, .before = 1)
}
group_row <- function(label, columns) section_row(label, columns, "group")

oct7_label <- "October 7 and war-related exposure (14-item index)"


# ---- Table 1 -----------------------------------------------------

summaries <- desc("Table1_Summaries")
group_n <- summaries %>% filter(variable == "gender_full") %>% group_by(group) %>% summarise(n = sum(n))
group_names <- c("Overall", "Non-users", "Users")
t1_cols <- c("Characteristic", "Overall", "Non_users", "Users")

t1_summary <- function(variable, level = NA) {
  s <- summaries[summaries$variable == variable & (if (is.na(level)) is.na(summaries$level) else summaries$level %in% level), ]
  s[match(group_names, s$group), ]
}
t1_row <- function(label, cells, type = "regular") {
  tibble(row_type = type, Characteristic = label, Overall = cells[1], Non_users = cells[2], Users = cells[3])
}
# Continuous: M (SD); categorical: n (%).
t1_continuous <- function(variable, label, digits = 2) {
  s <- t1_summary(variable)
  mean <- ifelse(round(s$mean, digits) == 0, 0, s$mean)
  t1_row(label, paste0(f2(mean, digits), " (", f2(s$sd, digits), ")"))
}
t1_categories <- function(heading, variable, levels, labels = levels) {
  bind_rows(
    group_row(heading, t1_cols),
    map2_dfr(levels, labels, function(l, label) {
      s <- t1_summary(variable, l)
      t1_row(label, paste0(comma(s$n), " (", f2(s$pct, 1), ")"), "category")
    })
  )
}

table1_rows <- bind_rows(
  section_row("Demographics", t1_cols),
  t1_continuous("age", "Age (years)", 1),
  t1_categories("Gender", "gender_full", c("Female", "Male", "Other")),
  t1_categories("Education", "education", c("Primary", "Secondary", "Post-secondary non-academic",
                                            "Bachelor's degree", "Master's degree or higher")),
  t1_categories("Income", "income", c("Much below average", "Below average", "About average",
                                      "Above average", "Much above average")),
  t1_categories("Religious affiliation", "religion_full",
                c("Jewish", "Muslim", "Christian", "Druze", "Atheist / no religion", "Other"),
                c("Jewish", "Muslim", "Christian", "Druze", "Atheist or no religion", "Other")),
  section_row("AI use", t1_cols),
  t1_categories("General AI use, past 3 months", "ai_general_frequency",
                c("Not at all", "A few times", "Several times a month", "Several times a week",
                  "Almost every day / every day"),
                c("Not at all", "A few times", "Several times a month", "Several times a week",
                  "Almost every day or every day")),
  section_row("Psychological and interpersonal", t1_cols),
  t1_continuous("distress_z", "Psychological distress (composite z score)"),
  t1_continuous("loneliness", "Loneliness (UCLA-3, 3–9)"),
  t1_continuous("social_support", "Perceived social support (mMOS-SS, 0–100)", 1),
  section_row("PTSD symptoms and trauma exposure", t1_cols),
  t1_continuous("pcl", "PTSD symptoms (PCL-5, 0–80)"),
  t1_continuous("ace", "Childhood adversity (ACE-10, 0–10)"),
  t1_continuous("lec_direct", "Direct trauma exposure (LEC-5, 0–17)"),
  t1_continuous("oct7_total_14", oct7_label)
)
table1 <- select(table1_rows, -row_type)

loneliness_missing <- group_n$n - t1_summary("loneliness")$n[match(group_n$group, group_names)]
names(loneliness_missing) <- group_n$group

table1_ft <- pub_table(table1, c(3.1, 1.3, 1.3, 1.4), c("left", rep("center", 3)), 9, table1_rows$row_type) %>%
  set_header_labels(
    Characteristic = "Characteristic",
    Overall = paste0("Overall\n(N = ", comma(group_n$n[group_n$group == "Overall"]), ")"),
    Non_users = paste0("Non-users\n(n = ", comma(group_n$n[group_n$group == "Non-users"]), ")"),
    Users = paste0("AI emotional-support users\n(n = ", comma(group_n$n[group_n$group == "Users"]), ")")
  )

write_table_doc(
  file.path(main_dir, "tables", "Table_1_Participant_Characteristics.docx"), "Table 1",
  "Participant Characteristics by AI Emotional-Support Use",
  list(table1_ft),
  paste0(
    "Values are M (SD) or n (%). Non-users reported no AI emotional-support use in the past 3 months. ",
    "Psychological distress is the standardized mean of the PHQ-9, GAD-7, and PSS-10 z scores. The 533 ",
    "participants who reported no index trauma were not administered the PCL-5 and are scored 0. Loneliness was ",
    "missing for ", loneliness_missing[["Overall"]], " participants (both non-users); all other characteristics were complete."
  )
)
stopifnot(loneliness_missing[["Overall"]] == 2, loneliness_missing[["Users"]] == 0)


# ---- Table 2 -----------------------------------------------------

coefficients <- mod("Coefficients")
block_tests <- mod("Block_Tests")
model_names <- paste0("M", 1:5)
t2_cols <- c("Predictor", paste0(rep(model_names, each = 2), c("_OR", "_p")))

# One row: OR [95% CI] and p for each model; the p values are kept
# alongside for bolding.
t2_row <- function(term, label, type = "regular", digits = 2) {
  r <- coefficients[coefficients$term == term, ][match(model_names, coefficients$model[coefficients$term == term]), ]
  or <- ifelse(is.na(r$OR), "", if (digits == 3) {
    paste0(f2(r$OR, 3), " [", f2(r$CI_lower, 3), ", ", f2(r$CI_upper, 3), "]")
  } else f_or(r$OR, r$CI_lower, r$CI_upper))
  p <- fp(r$p_value)
  tibble(row_type = type, Predictor = label,
         !!!setNames(as.list(c(rbind(or, p))), t2_cols[-1]),
         !!!setNames(as.list(r$p_value), paste0(model_names, "_pvalue")))
}
t2_categories <- function(heading, prefix, levels) {
  bind_rows(group_row(heading, t2_cols), map_dfr(levels, ~ t2_row(paste0(prefix, .x), .x, "category")))
}

table2_rows <- bind_rows(
  section_row("Demographic covariates", t2_cols),
  t2_row("age", "Age (years)", digits = 3),
  t2_categories("Gender (reference: female)", "gender_model", c("Male", "Other")),
  t2_categories("Education (reference: secondary)", "education",
                c("Primary", "Post-secondary non-academic", "Bachelor's degree", "Master's degree or higher")),
  t2_categories("Income (reference: about average)", "income",
                c("Much below average", "Below average", "Above average", "Much above average")),
  t2_categories("Religious affiliation (reference: Jewish)", "religion_binary", "Non-Jewish"),
  section_row("Psychological and interpersonal (per SD)", t2_cols),
  t2_row("distress_z", "Psychological distress"),
  t2_row("loneliness_z", "Loneliness"),
  t2_row("social_support_z", "Perceived social support"),
  section_row("PTSD symptoms and trauma exposure (per SD)", t2_cols),
  t2_row("pcl_z", "PTSD symptoms (PCL-5)"),
  t2_row("ace_z", "Childhood adversity (ACE-10)"),
  t2_row("lec_direct_z", "Direct trauma exposure (LEC-5)"),
  t2_row("oct7_z", oct7_label),
  section_row("Model", t2_cols),
  tibble(row_type = "block", Predictor = "Block test (Wald F)",
         !!!setNames(as.list(c(rbind(f_wald(block_tests$F, block_tests$df_num, block_tests$df_den),
                                     fp(block_tests$p_value)))), t2_cols[-1]),
         !!!setNames(as.list(block_tests$p_value), paste0(block_tests$model, "_pvalue"))),
  tibble(row_type = "n", Predictor = "N (events)",
         !!!setNames(as.list(c(rbind(paste0(comma(block_tests$n), " (", comma(block_tests$events), ")"), ""))), t2_cols[-1]))
) %>%
  mutate(across(all_of(t2_cols), ~ replace_na(.x, "")))
table2 <- select(table2_rows, all_of(t2_cols))

table2_ft <- pub_table(table2, c(2.45, rep(c(1.1, 0.39), 5)), c("left", rep(c("center", "center"), 5)), 8,
                       table2_rows$row_type) %>%
  set_header_labels(values = setNames(as.list(c("Predictor", rep(c("OR [95% CI]", "p"), 5))), t2_cols)) %>%
  add_header_row(values = c("", "M1: Demographics", "M2: + Distress", "M3: + Loneliness, social support",
                            "M4: + PCL-5", "M5: + Trauma exposure"), colwidths = c(1, rep(2, 5))) %>%
  hline(i = 1, j = 2:11, border = officer::fp_border(width = 0.75), part = "header") %>%
  padding(j = seq(3, 11, 2), padding.left = 1, padding.right = 3, part = "all")
n_row <- which(table2_rows$row_type == "n")
for (k in seq_along(model_names)) {
  table2_ft <- bold_p(table2_ft, 2 * k + 1, table2_rows[[paste0(model_names[k], "_pvalue")]])
  table2_ft <- merge_at(table2_ft, i = n_row, j = 2 * k + 0:1, part = "body")
}

write_table_doc(
  file.path(main_dir, "tables", "Table_2_Hierarchical_Models.docx"), "Table 2",
  paste("Hierarchical Logistic Regression of AI Emotional-Support Use on Distress,",
        "Interpersonal Factors, PTSD Symptoms, and Trauma Exposure"),
  list(table2_ft),
  paste0(
    "Adjusted odds ratios (ORs) with 95% CIs from logistic regressions fitted to the same complete-case sample at ",
    "every step; M1–M3 form the original psychological model and M4–M5 the trauma extension. Age is per year; ",
    "other continuous predictors are per SD. Block tests are Wald tests of the terms added at each step (M1: all ",
    "demographic terms). Participants who reported no index trauma (n = 532) were not administered the PCL-5 and ",
    "are scored 0. The October 7 and war-related exposure index includes relocation during the Iran war. Bold: p<0.05."
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
caption_theme <- theme(plot.caption = ggtext::element_markdown(size = 7, colour = muted, hjust = 0, lineheight = 1.15))
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
      "AI emotional-support users (*n* = ", comma(n_users), "); multiple responses allowed. Points are observed ",
      "percentages with unadjusted logit-based 95% CIs.<br>Topics: all response options except the residual ‘other’ ",
      "category (Table S1)."
    ),
    theme = caption_theme
  )
save_figure(figure1, "Figure_1_AI_Experiences_and_Topics", 7.2, 5.6)


# Figure 2: daily vs less frequent users among daily general AI users,
# Holm-significant comparisons only (all 21 are in Table S4).
comparisons <- intensive("Experience_Comparisons")
n_daily <- unique(comparisons$n_daily)
n_less <- unique(comparisons$n_less)
stopifnot(length(n_daily) == 1, length(n_less) == 1)

figure2_labels <- replace(short_labels, "help_a_lot", "Helped a lot or a very great deal")
figure2_data <- comparisons %>%
  filter(holm_p < .05) %>%
  mutate(family = factor(family_labels[family], family_labels), item = figure2_labels[variable]) %>%
  order_items(difference_pct) %>%
  select(family, variable, item, n_daily, pct_daily, n_less, pct_less, holm_p)
stopifnot(nrow(figure2_data) == 9, !anyNA(figure2_data$item))

group_labels <- c(Daily = paste0("Daily: almost every day or every day (*n* = ", n_daily, ")"),
                  `Less frequent` = paste0("Less frequent (*n* = ", n_less, ")"))
x_p <- 114

figure2_row <- function(fam, top, bottom) {
  d <- droplevels(filter(figure2_data, family == fam))
  points <- bind_rows(transmute(d, item, group = "Daily", pct = pct_daily, side = sign(pct_daily - pct_less)),
                      transmute(d, item, group = "Less frequent", pct = pct_less, side = sign(pct_less - pct_daily)))
  headers <- if (top) {
    annotate(ggtext::GeomRichText, x = x_p, y = nrow(d) + 0.95, label = "**Holm *p***", hjust = 1, size = 2.6,
             colour = muted, fill = NA, label.colour = NA, label.padding = unit(0, "pt"))
  }
  ggplot(d, aes(y = item)) +
    geom_segment(aes(x = pct_less, xend = pct_daily, yend = item), colour = "#C9D2D9", linewidth = 2.2, lineend = "round") +
    geom_point(data = points, aes(x = pct, colour = group, shape = group), size = 2.6) +
    # Each percentage sits on the outer side of its marker.
    geom_text(data = points, aes(x = pct + 2.6 * side, label = paste0(f2(pct, 1), "%"), hjust = (1 - side) / 2),
              size = 2.5, colour = ink) +
    geom_text(aes(x = x_p, label = fp(holm_p)), hjust = 1, size = 2.5, colour = ink) +
    headers +
    scale_colour_manual(values = c(Daily = blue, `Less frequent` = orange), labels = group_labels, name = NULL) +
    scale_shape_manual(values = c(Daily = 16, `Less frequent` = 18), labels = group_labels, name = NULL) +
    scale_x_continuous(limits = c(-10, x_p), breaks = seq(0, 100, 20), expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(clip = "off") +
    labs(x = if (bottom) "Users reporting the item (%)" else NULL, y = NULL, title = fam) +
    figure_theme + theme(axis.title.x = element_text(hjust = 0.48), axis.line.x = element_blank()) +
    (if (bottom) theme() else hide_x) + (if (top) theme() else guides(colour = "none", shape = "none"))
}
families2 <- levels(droplevels(figure2_data$family))
stopifnot(identical(families2, unname(family_labels)))
figure2 <- wrap_plots(
  pmap(list(families2, seq_along(families2) == 1, seq_along(families2) == length(families2)), figure2_row),
  ncol = 1, heights = as.numeric(table(figure2_data$family)) + 0.9
) +
  plot_layout(guides = "collect") +
  plot_annotation(
    caption = paste0(
      "AI emotional-support users with daily general AI use (*n* = ", n_daily + n_less, "). Only the ",
      c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")[nrow(figure2_data)],
      " comparisons significant after Holm correction (*p*<0.05) are shown;<br>Table S4 reports all 21 comparisons."
    ),
    theme = caption_theme
  ) &
  theme(legend.position = "bottom", legend.justification = "left", legend.margin = margin(0, 0, 0, 0),
        legend.text = ggtext::element_markdown(size = 8, colour = ink))
save_figure(figure2, "Figure_2_Intensive_Emotional_Support_Users", 7.2, 4.6)


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
                 tableS1_rows$row_type) %>%
         # Layout of the approved Table S1, unchanged.
         padding(padding.top = 3.5, padding.bottom = 3.5, padding.left = 5, padding.right = 5, part = "all") %>%
         align(align = "center", part = "header") %>%
         set_header_labels(percentage_ci = "% (95% CI)")),
  paste0("n = ", comma(n_users), " AI emotional-support users. Multiple responses were permitted for experiences ",
         "and topics. Percentages are shown with logit-based 95% CIs."),
  landscape = TRUE
)


# ---- Table S2: sensitivity analyses ------------------------------

term_labels <- c(
  phq_z = "Depression (PHQ-9)", gad_z = "Anxiety (GAD-7)", pss_z = "Perceived stress (PSS-10)",
  distress_z = "Psychological distress", loneliness_z = "Loneliness", social_support_z = "Perceived social support",
  pcl_z = "PTSD symptoms (PCL-5)", ace_z = "Childhood adversity (ACE-10)",
  lec_direct_z = "Direct trauma exposure (LEC-5)", oct7_z = oct7_label
)
sensitivity <- mod("Sensitivity")
components <- mod("Components")
component_joint <- mod("Components_Joint_Test")

or_cells <- function(d) tibble(OR = f_or(d$OR, d$CI_lower, d$CI_upper), p = fp(d$p_value), p_value = d$p_value)
sensitivity_check <- function(check) {
  d <- filter(sensitivity, check == !!check)
  d <- d[match(intersect(names(term_labels), d$term), d$term), ]
  bind_cols(Predictor = term_labels[d$term], or_cells(d))
}
sample_note <- function(check) {
  d <- filter(sensitivity, check == !!check)
  paste0(if (unique(d$n) == unique(sensitivity$n[sensitivity$check == "M3, main"])) "N" else "n",
         " = ", comma(unique(d$n)), "; ", comma(unique(d$events)), " users")
}
or_header <- c(OR = "OR [95% CI]", p = "p")

s2a <- bind_rows(
  bind_cols(Predictor = term_labels[components$term], or_cells(components)),
  tibble(Predictor = "Distress components, joint test",
         OR = f_wald(component_joint$F, component_joint$df_num, component_joint$df_den),
         p = fp(component_joint$p_value), p_value = component_joint$p_value)
)
b3 <- sensitivity_check("M3, inconsistent responders excluded")
b5 <- sensitivity_check("M5, inconsistent responders excluded")
s2b <- tibble(Predictor = b5$Predictor) %>%
  left_join(select(b3, Predictor, M3_OR = OR, M3_p = p, M3_p_value = p_value), by = "Predictor") %>%
  left_join(select(b5, Predictor, M5_OR = OR, M5_p = p, M5_p_value = p_value), by = "Predictor") %>%
  mutate(across(c(M3_OR, M3_p), ~ replace_na(.x, "")))
s2c <- sensitivity_check("M5, administered the PCL-5 only")

s2_table <- function(d, widths) {
  pub_table(select(d, Predictor, OR, p), widths, c("left", "center", "center"), 9) %>%
    set_header_labels(values = c(Predictor = "Predictor (per SD)", or_header)) %>%
    bold_p(3, d$p_value)
}
s2b_ft <- pub_table(select(s2b, Predictor, M3_OR, M3_p, M5_OR, M5_p), c(2.9, 1.45, 0.6, 1.45, 0.6),
                    c("left", rep("center", 4)), 9) %>%
  set_header_labels(values = c(Predictor = "Predictor (per SD)", M3_OR = "OR [95% CI]", M3_p = "p",
                               M5_OR = "OR [95% CI]", M5_p = "p")) %>%
  add_header_row(values = c("", "M3", "M5"), colwidths = c(1, 2, 2)) %>%
  hline(i = 1, j = 2:5, border = officer::fp_border(width = 0.75), part = "header") %>%
  bold_p(3, s2b$M3_p_value) %>%
  bold_p(5, s2b$M5_p_value)

n_pcl_excluded <- unique(sensitivity$n[sensitivity$check == "M5, main"]) -
  unique(sensitivity$n[sensitivity$check == "M5, administered the PCL-5 only"])

write_table_doc(
  file.path(supplement_dir, "tables", "Table_S2_Sensitivity_Analyses.docx"), "Table S2",
  "Sensitivity Analyses for the Hierarchical Logistic Regression",
  setNames(
    list(s2_table(s2a, c(4.2, 2.0, 0.8)), s2b_ft, s2_table(s2c, c(4.2, 2.0, 0.8))),
    c(paste0("A. Distress components entered separately (M3; ", sample_note("M3, main"), ")"),
      paste0("B. Excluding ", comma(unique(sensitivity$n[sensitivity$check == "M3, main"]) -
                                      unique(sensitivity$n[sensitivity$check == "M3, inconsistent responders excluded"])),
             " logically inconsistent responders (", sample_note("M3, inconsistent responders excluded"), ")"),
      paste0("C. Excluding ", comma(n_pcl_excluded), " participants routed past the PCL-5 (M5; ",
             sample_note("M5, administered the PCL-5 only"), ")"))
  ),
  paste0(
    "Adjusted ORs per SD with 95% CIs; each model includes all covariates of the indicated hierarchical step. ",
    "Main-analysis estimates are in Table 2. A: PHQ-9, GAD-7, and PSS-10 replace the distress composite. ",
    "B: respondents reporting AI emotional-support use but no general AI use are excluded. C: participants who ",
    "reported no index trauma (scored 0 on the PCL-5 in the main analysis) are excluded. Users = AI ",
    "emotional-support users. Bold: p<0.05."
  )
)


# ---- Table S3: secondary model among general AI users ------------

secondary <- mod("Secondary_Coefficients")
secondary_tests <- mod("Secondary_Tests")
secondary_sample <- mod("Secondary_Sample")
s3_cols <- c("Predictor", "Estimate", "p")
s3_or <- function(term, label) {
  r <- secondary[secondary$term == term, ]
  stopifnot(nrow(r) == 1)
  tibble(row_type = "regular", Predictor = label, Estimate = f_or(r$OR, r$CI_lower, r$CI_upper),
         p = fp(r$p_value), p_value = r$p_value)
}
s3_wald <- function(test, label) {
  r <- secondary_tests[secondary_tests$test == test, ]
  stopifnot(nrow(r) == 1)
  tibble(row_type = "regular", Predictor = label, Estimate = f_wald(r$F, r$df_num, r$df_den),
         p = fp(r$p_value), p_value = r$p_value)
}
tableS3_rows <- bind_rows(
  section_row("General AI use (reference: a few times)", s3_cols),
  s3_wald("General AI-use frequency", "Overall test"),
  s3_or("general_ai_frequencySeveral times a month", "Several times a month"),
  s3_or("general_ai_frequencySeveral times a week", "Several times a week"),
  s3_or("general_ai_frequencyAlmost every day / every day", "Almost every day or every day"),
  section_row("Psychological and interpersonal (per SD)", s3_cols),
  s3_or("distress_z", "Psychological distress"),
  s3_or("loneliness_z", "Loneliness"),
  s3_or("social_support_z", "Perceived social support"),
  section_row("PTSD symptoms and trauma exposure (per SD)", s3_cols),
  s3_or("pcl_z", "PTSD symptoms (PCL-5)"),
  s3_or("ace_z", "Childhood adversity (ACE-10)"),
  s3_or("lec_direct_z", "Direct trauma exposure (LEC-5)"),
  s3_or("oct7_z", oct7_label),
  s3_wald("Trauma exposure (joint)", "Trauma exposure indices, joint test")
)
tableS3 <- select(tableS3_rows, all_of(s3_cols))

# Every term of the secondary model, for the source data.
all_term_labels <- c(
  age = "Age (years)", gender_modelMale = "Gender: male (vs. female)", gender_modelOther = "Gender: other (vs. female)",
  educationPrimary = "Education: primary (vs. secondary)",
  `educationPost-secondary non-academic` = "Education: post-secondary non-academic (vs. secondary)",
  `educationBachelor's degree` = "Education: bachelor's degree (vs. secondary)",
  `educationMaster's degree or higher` = "Education: master's degree or higher (vs. secondary)",
  `incomeMuch below average` = "Income: much below average (vs. about average)",
  `incomeBelow average` = "Income: below average (vs. about average)",
  `incomeAbove average` = "Income: above average (vs. about average)",
  `incomeMuch above average` = "Income: much above average (vs. about average)",
  `religion_binaryNon-Jewish` = "Religious affiliation: non-Jewish (vs. Jewish)",
  term_labels[c("distress_z", "loneliness_z", "social_support_z", "pcl_z", "ace_z", "lec_direct_z", "oct7_z")],
  `general_ai_frequencySeveral times a month` = "General AI use: several times a month (vs. a few times)",
  `general_ai_frequencySeveral times a week` = "General AI use: several times a week (vs. a few times)",
  `general_ai_frequencyAlmost every day / every day` = "General AI use: almost every day or every day (vs. a few times)"
)
stopifnot(setequal(names(all_term_labels), secondary$term))
tableS3_all <- tibble(Predictor = all_term_labels[secondary$term],
                      `OR [95% CI]` = f_or(secondary$OR, secondary$CI_lower, secondary$CI_upper),
                      p = fp(secondary$p_value))

write_table_doc(
  file.path(supplement_dir, "tables", "Table_S3_General_AI_Users_Model.docx"), "Table S3",
  "Secondary Model Among General AI Users, Additionally Adjusted for General AI-Use Frequency",
  list(pub_table(tableS3, c(3.9, 1.9, 0.8), c("left", "center", "center"), 9.5, tableS3_rows$row_type) %>%
         set_header_labels(Estimate = "OR [95% CI] or Wald F") %>%
         bold_p(3, tableS3_rows$p_value)),
  paste0(
    "n = ", comma(sum(secondary_sample$n)), " participants reporting any general AI use (",
    comma(sum(secondary_sample$users)), " AI emotional-support users). The model includes all M5 terms, including ",
    "the demographic covariates (age, gender, education, income, religious affiliation), plus general AI-use ",
    "frequency. Coefficients for all terms are in the supplementary source data. Bold: p<0.05."
  )
)


# ---- Table S4: intensive emotional-support users -----------------

intensive_model <- intensive("Model")
check <- intensive("Model_Check")
check_value <- function(item) check$value[check$item == item]
group_sizes <- intensive("Group_Sizes")

intensive_joint <- intensive("Model_Joint_Test")
s4a <- bind_rows(
  tibble(
    Predictor = c(age = "Age (years)", female = "Female (vs. male)", distress_z = "Psychological distress (per SD)",
                  pcl_z = "PTSD symptoms (PCL-5; per SD)", ace_z = "Childhood adversity (ACE-10; per SD)",
                  lec_direct_z = "Direct trauma exposure (LEC-5; per SD)",
                  oct7_z = "October 7 and war-related exposure (14-item index; per SD)",
                  loneliness_z = "Loneliness (per SD)", social_support_z = "Perceived social support (per SD)")[intensive_model$term],
    `Adjusted OR [95% CI]` = f_or(intensive_model$OR, intensive_model$CI_lower, intensive_model$CI_upper),
    p = fp(intensive_model$p_value), p_value = intensive_model$p_value
  ),
  tibble(Predictor = "Trauma exposure indices, joint test",
         `Adjusted OR [95% CI]` = f_wald(intensive_joint$F, intensive_joint$df_num, intensive_joint$df_den),
         p = fp(intensive_joint$p_value), p_value = intensive_joint$p_value)
)
stopifnot(!anyNA(s4a$Predictor))
s4b_cols <- c("Item", "Daily", "Less_frequent", "Difference", "Holm_p")
s4b_rows <- map_dfr(c("Helpfulness", "Experiences", "Topics"), function(fam) {
  d <- filter(comparisons, family == fam)
  bind_rows(
    section_row(fam, s4b_cols),
    tibble(row_type = "regular", Item = d$label, Daily = f2(d$pct_daily, 1), Less_frequent = f2(d$pct_less, 1),
           Difference = f_ci1(d$difference_pct, d$difference_CI_lower, d$difference_CI_upper),
           Holm_p = fp(d$holm_p), holm_p = d$holm_p)
  )
})
s4b <- select(s4b_rows, all_of(s4b_cols))

write_table_doc(
  file.path(supplement_dir, "tables", "Table_S4_Intensive_Emotional_Support_Users.docx"), "Table S4",
  "Daily Versus Less Frequent AI Emotional-Support Use Among Users With Daily General AI Use (Exploratory)",
  setNames(
    list(pub_table(select(s4a, -p_value), c(4.3, 2.0, 0.8), c("left", "center", "center"), 9) %>%
           set_header_labels(`Adjusted OR [95% CI]` = "Adjusted OR [95% CI] or Wald F") %>%
           bold_p(3, s4a$p_value),
         pub_table(s4b, c(3.35, 0.9, 0.95, 1.3, 0.6), c("left", rep("center", 4)), 9, s4b_rows$row_type) %>%
           set_header_labels(Item = "Item", Daily = paste0("Daily\n(n = ", n_daily, "), %"),
                             Less_frequent = paste0("Less frequent\n(n = ", n_less, "), %"),
                             Difference = "Difference, pp\n[unadjusted 95% CI]", Holm_p = "Holm p") %>%
           bold_p(5, s4b_rows$holm_p)),
    c(paste0("A. Adjusted logistic regression of daily use (n = ", check_value("N"), "; ",
             check_value("Daily users (events)"), " daily, ", check_value("Less frequent users"), " less frequent)"),
      paste0("B. Helpfulness, experiences, and topics: all 21 comparisons (n = ", n_daily + n_less, "; ",
             n_daily, " daily, ", n_less, " less frequent)"))
  ),
  paste0(
    "Daily = almost every day or every day. Both analyses are restricted to emotional-support users with daily ",
    "general AI use, which restricts the comparison to daily general AI users and retains ", n_daily, " of the ",
    sum(group_sizes$n[group_sizes$daily == 1]), " daily emotional-support users. A: age, gender, and all M5 ",
    "psychological, interpersonal, and trauma predictors; the ", check_value("Excluded: gender 'Other' (none daily)"),
    " users reporting another gender (none daily) are excluded; c-statistic = ", f2(check_value("c-statistic")),
    ". B: differences are daily minus less frequent, in percentage points (pp), with Wald 95% CIs not adjusted for ",
    "multiplicity. p values are from Fisher's exact tests, Holm-adjusted within each item family (helpfulness, 2 ",
    "items; experiences, 7; topics, 12). Bold: p<0.05."
  )
)


# ---- Source data and checks --------------------------------------

stopifnot(
  nrow(table1) == 41, nrow(table2) == 29, nrow(tableS1) == 24, nrow(s2a) == 6, nrow(s2b) == 7,
  nrow(s2c) == 7, nrow(tableS3) == 15, nrow(tableS3_all) == 22, nrow(s4a) == 10,
  nrow(s4b) == 24, sum(s4b_rows$row_type == "regular") == 21
)

write_xlsx(list(Table_1 = table1, Table_2 = table2, Figure_1_Data = figure1_data, Figure_2_Data = figure2_data),
           file.path(main_dir, "source_data", "MAIN_Results_Tables.xlsx"))
write_xlsx(list(Table_S1 = tableS1, Table_S2_A = select(s2a, -p_value),
                Table_S2_B = select(s2b, -ends_with("p_value")), Table_S2_C = select(s2c, -p_value),
                Table_S3 = tableS3, Table_S3_All_Terms = tableS3_all, Table_S4_A = select(s4a, -p_value), Table_S4_B = s4b),
           file.path(supplement_dir, "source_data", "SUPPLEMENT_Results_Tables.xlsx"))

message("Publication outputs written to ", main_dir, " and ", supplement_dir)
