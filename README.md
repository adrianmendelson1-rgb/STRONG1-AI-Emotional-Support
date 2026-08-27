# Who Turns to AI for Emotional Support?

Analysis code for a cross-sectional study of the associations of psychological distress, loneliness, and perceived social support with the use of general-purpose large language models for emotional support, in the STRONG-1 baseline (T1) cohort.

The primary outcome is any self-reported use of AI for emotional support or coping with mental distress in the preceding three months. The primary hypotheses are tested in a hierarchical binary logistic regression: sociodemographic covariates, then psychological distress, then loneliness and perceived social support. All data are cross-sectional, so the estimates describe associations rather than effects. Secondary analyses examine general AI users separately, model the five-category use-frequency outcome, and describe how users experience AI support and how it relates to concurrent mental health treatment.

STRONG-1 is the population-based longitudinal arm of the STRONG study (Stress and Trauma Resilience: Opportunities for National Growth), a nationally quota-sampled cohort of Israeli adults assessed after the October 7, 2023 attack.

---

## What this repository contains

This repository contains **analysis code only**. It is the complete, ordered pipeline that produces every table, figure, and reported statistic in the manuscript, together with the quality-assurance outputs used to validate the analysis.

It contains **no participant data of any kind**: no rows, no identifiers, no free-text responses, no derived participant-level files.

---

## Data availability and privacy

STRONG-1 participant-level data are **not** included in this repository. The dataset contains sensitive mental health information collected from a national cohort, and data sharing and access are governed by the STRONG study team under the study's approved privacy and data-sharing procedures.

The pipeline does not write derived participant-level data to disk at any stage, and exports aggregate outputs only:

- `04_prepare_analysis_data.R` builds the analysis dataset as an in-memory R object. No participant-level or derived participant-level file is written to disk at any point in the pipeline.
- Scripts `01` through `10` export only aggregate SAFE/QA workbooks: counts, percentages, model coefficients, and validation checks.
- `01_make_dictionary.R` applies explicit safety rules when building the data dictionary. Variables that look like identifiers are excluded, and code-to-label mappings are exported only for numeric-coded variables with 20 or fewer distinct values, which prevents free-text responses from being exported.
- The repository contains no machine-specific paths. Pipeline completion messages report project-relative paths; low-level errors raised by external packages may still include the local path of the file being read.

STRONG-1 was approved by the Ethics Committee of the Faculty of Social Welfare and Health Sciences, University of Haifa (Approval No. 045/26). Requests for data access are handled by the STRONG study team under the study's approved data-sharing procedures; see [Contact](#contact).

---

## Requirements

**R version 4.5.1** (2025-06-13). The reported results were produced under this version.

### Packages

| Package | Used for |
| --- | --- |
| `readxl` | Reading the source workbook and the SAFE outputs |
| `writexl` | Writing all aggregate output workbooks |
| `dplyr`, `purrr`, `tibble` | Data manipulation |
| `stringr` | Identifier detection in the data dictionary |
| `psych` | Cronbach's alpha for scale reliability |
| `survey` | Design-based estimation (`svyglm`, `svydesign`, Wald tests) |
| `splines` | Natural cubic spline bases for functional-form checks |
| `broom` | Tidying model output |
| `car` | Variance inflation factors |
| `ggplot2`, `patchwork` | Figures 1 and 2 |
| `officer`, `flextable` | Publication-ready Word tables |

Each script declares the packages it needs and calls `ensure_packages()`, defined in `00_paths.R`, which installs any that are missing. Model SAFE workbooks record the package versions used for each run. Because missing packages are installed from the currently configured R repository, use the recorded versions when reproducing a frozen analysis. To install everything up front instead:

```r
install.packages(c(
  "readxl", "writexl", "dplyr", "purrr", "tibble", "stringr",
  "psych", "survey", "broom", "car", "ggplot2", "patchwork",
  "officer", "flextable"
))
```

`splines` ships with R and needs no installation.

---

## Repository structure

```
.
├── README.md
├── LICENSE
├── .gitignore
├── .githooks/                    Local safety guard against committing data or outputs
│   └── pre-commit
├── 01_Scripts/                  Analysis pipeline (run in order)
│   ├── 00_paths.R
│   ├── 01_make_dictionary.R
│   ├── 02_analysis_mapping.R
│   ├── 03_preanalysis_checks.R
│   ├── 04_prepare_analysis_data.R
│   ├── 05_descriptives.R
│   ├── 06_primary_models.R
│   ├── 07_general_AI_users_model.R
│   ├── 08_ordinal_frequency_model.R
│   ├── 09_AI_experience_descriptives.R
│   ├── 10_robustness_sensitivity.R
│   └── 11_results_tables_and_figures.R
├── 02_SAFE_Outputs/             SAFE/QA workbooks (generated, git-ignored)
├── 03_Main_Outputs/             Main publication outputs (generated, git-ignored)
│   ├── tables/                  Table 1 and Table 2 as formatted Word files
│   ├── figures/                 PDF figure masters
│   │   └── previews/            PNG figure previews
│   └── source_data/             Aggregate figure/table source workbook
├── 04_Supplement/               Supplementary outputs (generated, git-ignored)
│   ├── tables/                  Tables S1-S3 as formatted Word files
│   └── source_data/             Aggregate supplementary source workbook
└── data/                        STRONG-1 source data (never committed)
```

All paths are resolved from the location of `00_paths.R`, so the repository runs from any directory without editing anything.

---

## Placing the data

An authorized researcher places the STRONG-1 workbook at:

```
data/STRONG1Data.xlsx
```

Alternatively, point the pipeline at a file held elsewhere, for example on an encrypted volume, without moving it into the repository:

```r
Sys.setenv(STRONG1_DATA = "/secure/location/STRONG1Data.xlsx")
```

The workbook must contain these four sheets:

| Sheet | Contents |
| --- | --- |
| `T1_Values_200826` | Participant data, numeric coding |
| `T1_Labels_200826` | Participant data, value labels |
| `Legend` | Variable and measure metadata |
| `T1_Filtering_200826` | Central STRONG quality-control flags |

`data/` is git-ignored. Confirm with `git status` that the workbook is not staged before committing.

For the strongest protection, keep the participant workbook outside the repository and set `STRONG1_DATA` to its secure location. This repository also includes a pre-commit safety guard. Enable it once after cloning:

```bash
git config core.hooksPath .githooks
```

The guard rejects participant-data formats, generated result files, and anything inside `data/` or the three output directories other than their `.gitkeep` files. Do not bypass it with `git commit --no-verify`.

---

## Running the pipeline

From the project root:

```r
scripts <- list.files("01_Scripts", pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE)
for (script in sort(scripts)[-1]) source(script)   # 00_paths.R is sourced by each script
```

Or run the scripts one at a time in RStudio, in numerical order. Scripts 01 through 10 can also be run directly with Rscript 01_Scripts/<script-name>.R. Script 11 should be run after scripts 06 through 10 have produced their SAFE outputs, or as part of the full ordered pipeline. Each script sources 00_paths.R itself, and scripts 06 through 10 re-source 04_prepare_analysis_data.R so that each can be run independently from a clean session. 11_results_tables_and_figures.R re-sources 06_primary_models.R, because Figure 1 is generated directly from the fitted model on a dense prediction grid.
### Execution order

| Script | What it does |
| --- | --- |
| `00_paths.R` | Central path configuration and shared helpers; sourced by every other script |
| `01_make_dictionary.R` | Builds the SAFE data dictionary and codebook without exporting participant rows |
| `02_analysis_mapping.R` | Records the exact response coding of every categorical analysis variable |
| `03_preanalysis_checks.R` | Quality-control flow, score ranges, missingness, scale reliability, score validation |
| `04_prepare_analysis_data.R` | Builds the in-memory analysis dataset: scoring, recoding, standardization, distress composite |
| `05_descriptives.R` | Sample characteristics, prevalence estimates, and group comparisons |
| `06_primary_models.R` | Hierarchical logistic regression (H1 to H3), functional-form checks, adjusted predictions |
| `07_general_AI_users_model.R` | Secondary model restricted to participants reporting any general AI use |
| `08_ordinal_frequency_model.R` | Ordinal and threshold-specific models of the five-category frequency outcome |
| `09_AI_experience_descriptives.R` | Descriptive analyses for the secondary aims |
| `10_robustness_sensitivity.R` | Distress-component, logical-inconsistency, and weighting sensitivity analyses |
| `11_results_tables_and_figures.R` | Manuscript Tables 1 and 2, Figures 1 and 2, and Supplements S1 to S3 |

### Runtime validation

Scripts `03` and `04` assert their validation targets rather than only reporting them. They stop with an explicit error if the central quality-control flow, the audited reliability values, or the approved sample-flow handling are not reproduced. A clean run therefore confirms that the pipeline reproduced the audited analysis, not merely that it completed.

---

## Outputs

### SAFE/QA outputs, scripts 01 to 10, in `02_SAFE_Outputs/`

These are the internal validation and quality-assurance record. Every one is an aggregate-only workbook: sample flows, missingness tables, range checks, reliability coefficients, full model coefficient tables, diagnostics, and the raw text of every statistical test. They exist so that any reported number can be traced back to the model that produced it, and so that the absence of participant-level export can be verified.

```
SAFE_STRONG1_dictionary.xlsx           SAFE_07_general_AI_users_model.xlsx
SAFE_analysis_mapping.xlsx             SAFE_08_ordinal_frequency_model.xlsx
SAFE_preanalysis_checks.xlsx           SAFE_09_AI_experience_descriptives.xlsx
SAFE_04_preparation_check.xlsx         SAFE_10_robustness_sensitivity.xlsx
SAFE_05_descriptives.xlsx
SAFE_06_primary_models.xlsx
```

### Manuscript outputs, script 11, in `03_Main_Outputs/`

The formatted deliverables that appear in the paper, separated from their
aggregate source data.

```
tables/Table_1_Participant_Characteristics.docx
tables/Table_2_Primary_and_Secondary_Models.docx
figures/Figure_1_Adjusted_Probability_Curves.pdf
figures/Figure_2_AI_Experiences_and_Psychotherapy.pdf
figures/previews/Figure_1_Adjusted_Probability_Curves.png
figures/previews/Figure_2_AI_Experiences_and_Psychotherapy.png
source_data/MAIN_Results_Tables.xlsx
```

### Supplementary outputs, script 11, in `04_Supplement/`

```
tables/Table_S1_AI_Experience_Descriptives.docx
tables/Table_S2_Frequency_Threshold_Models.docx
tables/Table_S3_Sensitivity_Analyses.docx
source_data/SUPPLEMENT_Results_Tables.xlsx
```

The distinction matters: the SAFE workbooks are exhaustive and unformatted,
intended for verification. The two workbooks under `source_data/` contain the
curated aggregate data used for the publication outputs, but are not submission
tables. The DOCX and PDF files are the publication-facing deliverables.

Generated outputs are deliberately excluded from this repository. If a frozen aggregate release is needed later, publish it separately under an explicitly reviewed data-release process rather than weakening the repository safeguards.

---

## ⚠️ Weighting status: interim

**The current pipeline runs with unit weights.** STRONG-1 post-sampling weights were not available when these analyses were run, so the weight variable (`TemporaryWeights`) is constant at 1 throughout.

Consequences while this note stands:

- Design-based estimation via `survey::svyglm` is in place and functioning, but with unit weights it returns unweighted estimates.
- Every column labelled `weighted_pct`, `weighted_mean`, or similar in the outputs is currently an **unweighted** sample proportion or mean.
- SAFE workbooks from scripts `04` through `10` carry a `Weight_Status` sheet documenting the unit-weight status, and script `11` prints the weight status on completion. These are the checks to watch when the weights arrive.
- Any manuscript text describing the analysis should state that estimates are unweighted, so that use of the `survey` package is not read as implying design-weighted estimation.

**To update when the final weights are released**, change the weight variable name in exactly two places and re-run the pipeline from script `01`:

| File | Line | Change |
| --- | --- | --- |
| `04_prepare_analysis_data.R` | `weight_var <- "TemporaryWeights"` | Set to the final weight variable name |
| `03_preanalysis_checks.R` | `"TemporaryWeights"` in the missingness variable list | Same name, so the QA check still covers the weight |

Everything downstream reads the prepared `analysis_weight` column and needs no edit. `10_robustness_sensitivity.R` already includes a weighted-versus-unweighted comparison on the same sample, which will quantify how far the estimates move. Once the weights are in, remove this section from the README.

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
