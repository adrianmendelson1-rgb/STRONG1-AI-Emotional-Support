# Who Turns to AI for Emotional Support?

Analysis code for a cross-sectional study of AI emotional-support use in the baseline (T1) wave of STRONG-1, the population-based longitudinal cohort of the STRONG study (Stress and Trauma Resilience: Opportunities for National Growth). The primary outcome is any use of AI for emotional support or coping with mental distress in the preceding three months. The analyses are a five-step hierarchical logistic regression (M1 demographics, M2 psychological distress, M3 loneliness and perceived social support, M4 PTSD symptoms, M5 trauma exposure), one secondary model among general AI users, three sensitivity analyses, an exploratory comparison of daily with less frequent emotional-support users, and descriptive analyses of users' experiences.

## Data and privacy

This repository contains code only. STRONG-1 participant-level data contain sensitive mental health information; access is governed by the STRONG study team. STRONG-1 was approved by the Ethics Committee of the Faculty of Social Welfare and Health Sciences, University of Haifa (No. 045/26).

`data/` and all generated outputs are git-ignored. After cloning, enable the pre-commit guard, which rejects participant data and generated outputs:

```bash
git config core.hooksPath .githooks
```

## Requirements

R 4.5.1 or later with these CRAN packages:

```r
install.packages(c("dplyr", "purrr", "readxl", "writexl", "survey", "car",
                   "ggplot2", "ggtext", "patchwork", "officer", "flextable"))
```

The outputs were verified with `officer` 0.7.7 and `flextable` 0.10.3. Each analysis workbook records the R and package versions of its run in a `Run_Info` sheet.

## Input data

An authorized researcher places the STRONG-1 workbook at `data/STRONG1Data.xlsx`, with the sheets `T1_Values_200826` (responses), `T1_Labels_200826` (response labels) and `T1_Filtering_200826` (central quality-control records). The workbook is only read, never modified.

`01_prepare_data.R` writes the cleaned analysis dataset `data/STRONG1_T1_analysis_data.rds`, the only data source of the later scripts. It holds the 3,602 retained participants with every item and supplied score of the questionnaires analyzed (PHQ-9, GAD-7, PSS-10, UCLA-3, mMOS-SS, PCL-5 with its routing item, ACE-10, LEC-5, the October 7 exposure items and the study's AI items), the demographic, administration and design variables, and the derived analysis variables; plus the 4,979 central QC records and the response labels of the coded items.

## Running the pipeline

From the project root:

```bash
Rscript 01_Scripts/run_all.R
```

Each script runs in its own R process, in order, and the pipeline stops at the first failure.

| Script | Contents | Output |
| --- | --- | --- |
| `00_setup.R` | Paths, packages and the model helpers; sourced by every script | |
| `01_prepare_data.R` | Variable selection, response-code checks, scoring and derived variables | `data/STRONG1_T1_analysis_data.rds` |
| `02_data_checks.R` | QC flow, item codes and scale scores, reliability, routing, analytic samples, missingness | `Data_Checks.xlsx` |
| `03_descriptives.R` | Prevalence and frequency of AI use, Table 1 summaries, experiences and topics | `Descriptives.xlsx` |
| `04_models.R` | Hierarchical models M1–M5, secondary model, sensitivity analyses | `Models.xlsx` |
| `05_intensive_users.R` | Daily vs less frequent users: adjusted model and 21 Holm-corrected comparisons | `Intensive_Users.xlsx` |
| `06_tables_figures.R` | Publication tables and figures, built only from the workbooks of 03–05 | Tables, figures, source data |

`02_data_checks.R` stops if the audited QC counts, scale scores, routing or analytic sample sizes are not reproduced.

## Analysis notes

- All estimates are unweighted (`TemporaryWeights` is a placeholder of 1s). Models are logistic regressions fitted with `survey::svyglm` on an equal-probability design, giving design-based standard errors and Wald tests. Age is per year; other continuous predictors are per sample SD.
- The models use the 3,600 complete cases; the two excluded participants lack only a loneliness score (UCLA-3 not computable).
- The 533 participants who reported no index trauma were routed past the PCL-5 and are scored 0 (532 in the model sample); a sensitivity analysis excludes them.
- The October 7 and war-related exposure index is the STRONG team's 14-item count, including relocation during the Iran war.

## Outputs

| Output | Contents |
| --- | --- |
| `02_Analysis_Outputs/` | Aggregate analysis workbooks, with every reported estimate |
| `03_Main_Outputs/tables/` | Table 1 (participant characteristics), Table 2 (hierarchical models) |
| `03_Main_Outputs/figures/` | Figure 1 (experiences and topics), Figure 2 (daily vs less frequent users); PNG previews in `previews/` |
| `04_Supplement/tables/` | Tables S1 (AI experiences), S2 (sensitivity), S3 (general AI users), S4 (intensive users) |
| `*/source_data/` | Formatted table contents and figure data, including every coefficient of the Table S3 model |

## Citation

> [Author list]. (2026). *Who turns to AI for emotional support? Psychological distress, loneliness, and perceived social support in a large Israeli cohort.* [Journal]. [DOI]

STRONG study protocol: Moallem, D., Maaravi-Hesseg, R., Panitz, D., Pietrzak, R. H., & Ben-Zion, Z. (2026). *The STRONG study: A multi-tiered, multimodal investigation of resilience and recovery following prolonged collective adversity* [Preprint]. medRxiv. https://doi.org/10.64898/2026.08.17.26360579

## Contact and license

Adrian Mendelson, Ben-Zion Resilience & Recovery Lab, University of Haifa (Amende12@campus.haifa.ac.il). Questions about data access go to the STRONG study team.

Released under the MIT License; see [`LICENSE`](LICENSE).
