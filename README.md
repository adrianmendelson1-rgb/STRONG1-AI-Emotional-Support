# Who Turns to AI for Emotional Support?

Analysis code for a cross-sectional study of who turns to general-purpose large language models for emotional support in the STRONG-1 baseline (T1) cohort: psychological distress, lifetime trauma exposure, PTSD symptoms, loneliness, and perceived social support.

The primary outcome is any self-reported use of AI for emotional support or coping with mental distress in the preceding three months. The primary analysis is one five-step hierarchical logistic regression: demographic covariates (M1), psychological distress (M2), loneliness and perceived social support (M3), PTSD symptoms (M4), and trauma exposure (M5: childhood adversity, adulthood trauma, and exposure since October 7). M1 to M3 are the original psychological model; M4 and M5 are the trauma extension added in the 2026 revision. One secondary model refits the final specification among participants reporting any general AI use, additionally adjusting for general AI-use frequency. A limited exploratory analysis compares daily with less frequent emotional-support users among participants who use AI daily in general. Descriptive analyses report how users experience AI support. All data are cross-sectional, so the estimates describe associations rather than effects.

STRONG-1 is the population-based longitudinal arm of the STRONG study (Stress and Trauma Resilience: Opportunities for National Growth), a nationally quota-sampled cohort of Israeli adults assessed after the October 7, 2023 attack.

---

## Data availability and privacy

This repository contains **analysis code only**: no participant rows, identifiers, free-text responses, or derived participant-level files. STRONG-1 participant-level data contain sensitive mental health information, and access is governed by the STRONG study team under the study's approved privacy and data-sharing procedures.

The pipeline never writes participant-level data to disk. `01_prepare_data.R` builds the analysis dataset in memory each time it is sourced; every exported file is aggregate (counts, percentages, model estimates, validation checks).

STRONG-1 was approved by the Ethics Committee of the Faculty of Social Welfare and Health Sciences, University of Haifa (Approval No. 045/26). Requests for data access are handled by the STRONG study team; see [Contact](#contact).

---

## Requirements

R 4.5.1 or later with `dplyr`, `purrr`, `tibble`, `tidyr`, `readxl`, `writexl`, `survey`, `car`, `psych`, `ggplot2`, `ggtext`, `patchwork`, `officer` and `flextable`. Missing packages are installed on first use from the configured repository; every analysis workbook records the R and package versions of the run in its `Run_Info` sheet.

On macOS, run R in a UTF-8 locale (for example `LANG=en_US.UTF-8`) when the project path contains non-ASCII characters.

---

## Placing the data

An authorized researcher places the STRONG-1 workbook at `data/STRONG1Data.xlsx`, or points the pipeline at a file held elsewhere, for example on an encrypted volume:

```r
Sys.setenv(STRONG1_DATA = "/secure/location/STRONG1Data.xlsx")
```

The workbook must contain the sheets `T1_Values_200826` (numeric coding), `T1_Labels_200826` (value labels) and `T1_Filtering_200826` (central STRONG quality-control flags).

`data/` and all generated outputs are git-ignored. Enable the pre-commit guard once after cloning; it rejects participant-data formats, generated result files, and anything inside `data/` or the output directories other than their `.gitkeep` files. Do not bypass it with `git commit --no-verify`.

```bash
git config core.hooksPath .githooks
```

**Survey weights are deferred.** The workbook's `TemporaryWeights` column is a placeholder (every value is 1). All analyses are unweighted.

---

## Repository structure

```
.
├── README.md
├── LICENSE
├── .githooks/pre-commit         Guard against committing data or outputs
├── 01_Scripts/                  Analysis pipeline
│   ├── run_all.R
│   ├── 00_setup.R
│   ├── 01_prepare_data.R
│   ├── 02_data_checks.R
│   ├── 03_descriptives.R
│   ├── 04_models.R
│   ├── 05_intensive_users.R
│   └── 06_tables_figures.R
├── 02_Analysis_Outputs/         Aggregate analysis workbooks (generated, git-ignored)
├── 03_Main_Outputs/             Tables 1-2, Figures 1-2 (generated, git-ignored)
├── 04_Supplement/               Tables S1-S4, Supplementary Methods S1 (git-ignored)
└── data/                        STRONG-1 source data (never committed)
```

---

## Running the pipeline

From the project root:

```bash
Rscript 01_Scripts/run_all.R
```

`run_all.R` runs scripts 02 to 06 in order, each in its own R process, and stops at the first failure, so no object left over from one step can satisfy a dependency in the next. A full run takes under a minute. Scripts can also be run individually from the project root, in order.

| Script | What it does | Output |
| --- | --- | --- |
| `00_setup.R` | Locale, paths, packages, and the shared helpers: proportions with logit CIs, model fitting, odds ratios, Wald tests, VIFs, workbook export. Sourced by every script | |
| `01_prepare_data.R` | Validates raw response codes and score ranges, then builds the analysis dataset in memory: recoding, scoring, standardization, distress composite, trauma exposure, PCL-5 and routing flags. Sourced by 02 to 05 | |
| `02_data_checks.R` | Central QC participant flow, scale reliability, supplied scale scores checked against their items, response coding, every analytic sample with its number of emotional-support users, the reason for the model exclusions, PCL-5 routing profile, missingness | `Data_Checks.xlsx` |
| `03_descriptives.R` | Prevalence and frequency of AI use, Table 1 summaries by emotional-support use, helpfulness, experiences and topics among emotional-support users | `Descriptives.xlsx` |
| `04_models.R` | Hierarchical model M1 to M5 (block and global Wald tests, diagnostics, VIFs), secondary model among general AI users, sensitivity checks | `Models.xlsx` |
| `05_intensive_users.R` | Daily versus less frequent emotional-support users among daily general AI users: one adjusted model and 21 experience comparisons | `Intensive_Users.xlsx` |
| `06_tables_figures.R` | All publication tables and figures, built only from `Descriptives.xlsx`, `Models.xlsx` and `Intensive_Users.xlsx`; it never reads participant-level data | Tables, figures, source data |

Analysis workbooks are written to `02_Analysis_Outputs/`; `Data_Checks.xlsx` is the data-integrity record and feeds no table. Script 06 refuses to run if any workbook it reads is missing or older than the code that produced it, so publication outputs cannot be built from stale results.

### Built-in validation

The pipeline asserts its audited values and stops with an explicit error if any is not reproduced:

- `01`: allowed response codes (including the Iran-relocation item of the October 7 index) and score ranges; the PCL-5 total equal to the sum of its items; retained sample 3,602 and hierarchical-model sample 3,600 with all 1,059 emotional-support users, the 2 excluded participants lacking only a loneliness score; 9 participants reporting another gender; 115 daily emotional-support users; 533 participants routed past the PCL-5, 532 in the model sample, all scored 0; the distribution of the 14-item October 7 index.
- `02`: central QC flow of 4,979 records: 1,377 excluded and 3,602 retained. The 1,377 exclusions include 57 records with no coded QC flag that were excluded for completion in under 10 minutes; no retained participant completed in under 10 minutes. Also Cronbach's alpha for PHQ-9, GAD-7, PSS-10, UCLA-3 and mMOS-SS; supplied scale scores identical to the item-based scores; every analytic sample size and its number of emotional-support users (below).
- `03`: no missing follow-up responses among emotional-support users.
- `05`: intensive-user samples of 406 (111 vs 295) and 404 (111 vs 293).

---

## Variables and estimation

- **Outcome:** any AI emotional-support use in the past three months (`MH_AI_Q02 > 0`).
- **Psychological distress:** mean of standardized PHQ-9, GAD-7 and PSS-10 totals, re-standardized. **Loneliness:** UCLA-3 (the technical code -97 is treated as missing). **Perceived social support:** mMOS-SS (0-100).
- **Demographics:** age, gender (reference female), education (reference secondary), income (reference about average), Jewish versus non-Jewish religious affiliation. This validated coding reproduces the original M1 to M3 results exactly; it differs from Arab-sector coding for 32 participants (atheist / no religion and other).

**Samples** (all verified in `Data_Checks.xlsx`, sheets `Sample_Flow` and `Model_Exclusions`):

| Sample | N | Emotional-support users |
| --- | --- | --- |
| Retained T1 sample: prevalence, Table 1, Figure 1 | 3,602 | 1,059 |
| Hierarchical models M1 to M5 (one complete-case sample at every step) | 3,600 | 1,059 |
| Secondary model: general AI users, complete cases (of 2,785) | 2,784 | 1,044 |
| Sensitivity: without the 15 inconsistent responders | 3,585 | 1,044 |
| Sensitivity: administered the PCL-5 (532 routed past it excluded) | 3,068 | 955 |
| Intensive users: 21 comparisons (111 daily vs 295 less frequent) | 406 | 406 |
| Intensive users: adjusted model (111 vs 293; another gender excluded) | 404 | 404 |

N = 3,600 rather than 3,602 because two retained participants have no loneliness score (the UCLA-3 total is not computable: technical code -97); every other model variable is complete for all 3,602. Both are non-users, so all 1,059 emotional-support users are in the model sample. Table 1 reports all 3,602, with loneliness for 3,600. In the intensive-user analysis, the 21 comparisons use all 406 emotional-support users with daily general AI use (111 daily vs 295 less frequent), and the adjusted model uses 404 (111 vs 293), excluding the 2 users reporting another gender.

All estimates are **unweighted**. Models are logistic regressions fitted with `survey::svyglm` on an equal-probability design (`ids = ~1`, `probs = ~1`), which supplies design-based (robust) standard errors and `regTermTest` multi-parameter Wald tests; it does not imply weighting. Every continuous predictor is linear on the log-odds scale: age per year, all other continuous predictors per sample SD (`sd()`, divisor n - 1), matching the SDs in Table 1. Each hierarchical step is evaluated with the Wald test of the block it adds; collinearity is checked with generalized VIFs. Prevalence CIs are logit-transformed. In the intensive-user comparisons, each p value comes from Fisher's exact test and is then Holm-adjusted within its item family (helpfulness 2, experiences 7, topics 12); `Intensive_Users.xlsx` keeps both, but publication outputs report only the Holm-adjusted p. Difference CIs are unadjusted Wald intervals and are labelled as such. Figure 2 shows only the comparisons with Holm-adjusted p < .05; Table S4 reports all 21. "Daily" means "almost every day / every day" throughout.

### Trauma exposure and PTSD symptoms (M4, M5)

Three exposure predictors, entered simultaneously in M5: childhood adversity to age 18 (ACE-10 total), direct trauma exposure (LEC-5 count of event types experienced directly; "not sure" excluded), and October 7 and war-related exposure (the 14-item October 7 index, `Oct7_Total_14`; labelled "October 7 and war-related exposure (14-item index)" in all tables).

The October 7 index is the STRONG team's count, used exactly as defined: 14 binary indicators (missile attack, missile nearby, Gaza envelope, grave danger, injured, ever evacuated, currently displaced, graphic videos, military service, relocation from the permanent residence during the Iran war (`Iran_Q01`), and a close other in danger, injured, killed, or kidnapped). The evacuation items are nested (currently displaced participants score 2). Iran relocation enters the models only through this index; it is not a separate predictor.

PTSD symptoms are the PCL-5 total. The PCL-5 is anchored to the participant's index event (`TraumaRank`); the 533 participants who reported no index event were routed past it, and the STRONG data team stored their items as 0. This is a structural zero, not an observed absence of symptoms. One routed participant lacks a loneliness score, so 532 are in the model sample. The main models keep the stored scoring; a sensitivity check refits M5 among participants administered the PCL-5.

### Analyses

| Analysis | Script | Role |
| --- | --- | --- |
| Hierarchical model M1 to M3 | `04` | Original psychological model (R2) |
| Hierarchical model M4 to M5 | `04` | PTSD symptoms and trauma exposure (R3) |
| General AI users: M5 + general AI-use frequency (reference "a few times") | `04` | Single secondary model (R4); conditional, not an independent replication |
| Distress components entered separately (M3) | `04` | Sensitivity: composite construction |
| Without the 15 inconsistent responders (M3, M5) | `04` | Sensitivity: outcome coding |
| M5 among participants administered the PCL-5 | `04` | Sensitivity: structural PCL-5 zeros |
| Helpfulness, experiences and topics | `03` | Descriptive (R1) |
| Daily vs less frequent users among daily general AI users | `05` | Exploratory (R5): one adjusted model of age, gender and all M5 psychological, interpersonal and trauma predictors (distress, PCL-5, ACE-10, LEC-5, October 7 index, loneliness, social support), with a joint Wald test of the three exposure indices (N = 404, excluding 2 users reporting another gender, none daily); 21 experience comparisons (N = 406; 111 vs 295) with Holm correction within item family |

---

## Outputs

### Analysis workbooks, `02_Analysis_Outputs/`

Exhaustive, unformatted and aggregate-only, so that every reported number can be traced to the model or check that produced it: `Data_Checks.xlsx`, `Descriptives.xlsx`, `Models.xlsx`, `Intensive_Users.xlsx`.

### Publication outputs

| Output | Contents | Results |
| --- | --- | --- |
| `03_Main_Outputs/tables/Table_1_Participant_Characteristics.docx` | Characteristics by emotional-support use (descriptive) | R1 |
| `03_Main_Outputs/tables/Table_2_Hierarchical_Models.docx` | M1 to M5: adjusted ORs and block tests | R2, R3 |
| `03_Main_Outputs/figures/Figure_1_AI_Experiences_and_Topics.pdf` | Experiences and topics among emotional-support users | R1 |
| `03_Main_Outputs/figures/Figure_2_Intensive_Emotional_Support_Users.pdf` | Daily vs less frequent users: percentages for the 9 Holm-significant comparisons, with Holm-adjusted p | R5 |
| `04_Supplement/tables/Table_S1_AI_Experience_Descriptives.docx` | Helpfulness, experiences and topics, % with 95% CI | R1 |
| `04_Supplement/tables/Table_S2_Sensitivity_Analyses.docx` | The three sensitivity analyses (main estimates are in Table 2) | R2, R3 |
| `04_Supplement/tables/Table_S3_General_AI_Users_Model.docx` | Secondary model among general AI users | R4 |
| `04_Supplement/tables/Table_S4_Intensive_Emotional_Support_Users.docx` | Intensive-user model and all 21 comparisons (Holm-adjusted p) | R5 |

PNG previews are in `03_Main_Outputs/figures/previews/`. The formatted table contents and figure data are also written to `03_Main_Outputs/source_data/MAIN_Results_Tables.xlsx` and `04_Supplement/source_data/SUPPLEMENT_Results_Tables.xlsx`; the latter also holds every coefficient of the secondary model (sheet `Table_S3_All_Terms`), including the demographic covariates not shown in Table S3.

Supplementary Methods S1 (`04_Supplement/`) gives the exact wording, response options and survey routing of the study-developed AI items analyzed in the article, in Hebrew, English and Arabic. It is a document, not a pipeline output, and is numbered separately from the supplementary tables. Two administered items about current mental health treatment are not analyzed and are omitted from it; the administered questionnaire and the raw data are unchanged.

Generated outputs are deliberately excluded from this repository. If a frozen aggregate release is needed, publish it separately under an explicitly reviewed data-release process rather than weakening the repository safeguards.

---

## Citation

> [Author list]. (2026). *Who turns to AI for emotional support? Psychological distress, loneliness, and perceived social support in the STRONG-1 cohort.* [Journal or repository]. [DOI]

STRONG study protocol:

> Moallem, D., Maaravi-Hesseg, R., Panitz, D., Pietrzak, R. H., & Ben-Zion, Z. (2026). *The STRONG study: A multi-tiered, multimodal investigation of resilience and recovery following prolonged collective adversity* [Preprint]. medRxiv. https://doi.org/10.64898/2026.08.17.26360579

---

## Contact

Adrian Mendelson  
Ben-Zion Resilience & Recovery Lab  
University of Haifa  
School of Public Health, Faculty of Social Welfare and Health Sciences, University of Haifa, Haifa, Israel  
Department of Psychology, Faculty of Social Sciences, University of Haifa, Haifa, Israel  
Email: Amende12@campus.haifa.ac.il

Questions about STRONG-1 data access should be directed to the STRONG study team rather than to this repository.

---

## License

Released under the MIT License. See [`LICENSE`](LICENSE).

Copyright (c) 2026 Adrian Mendelson.
