# bayesmaDatR: Data Management for Bayesian Meta-Analysis

<img src="man/figures/bayesmaDatR_hex.png" align="right" width="175" alt="bayesmaDatR package hex logo" />

An interactive workbook manager and data-preparation toolkit for Bayesian
meta-analysis. Start with the Excel file you already use, organise study and
outcome data, then prepare arm-level or two-stage data for modelling.

## Overview

`bayesmaDatR` is designed around an **upload → manage → prepare → export**
workflow. Use the Shiny app to work with an Excel workbook, or use the same
data structures from an R script for a reproducible analysis pipeline.

## Key features

* **Excel-first workflow:** upload, edit, validate, and download a workbook.
* **Outcome-specific data:** keep each outcome's sample sizes, time point,
  summary statistics, and trial-design correlations with that outcome.
* **Multiple trial designs:** record RCT, crossover RCT, cluster RCT, and
  cluster-crossover RCT data.
* **Flexible summaries:** retain means and SDs, medians and quantiles, ranges,
  event counts, and person-time.
* **Model-ready formats:** prepare arm-level data for one-stage, multivariate,
  multiarm, or longitudinal models, or calculate two-stage `yi` and `sei`.
* **Bias assessments:** manage the supported `robbR` tools and INSPECT-SR
  assessments in the same workbook.

## Interactive workflow

```r
library(bayesmaDatR)

launch_data_entry()
```

The workbook can contain four sheets: `studies`, `outcomes`, `risk_of_bias`,
and `inspect_sr`. The original app's sheet names `study_details`,
`outcome_data`, and `rob_data` are also recognized. Start with
`template_ma_data()` to see the recommended columns:

```r
template <- template_ma_data()
write_ma_workbook(template, "bayesmaDatR_template.xlsx")
```

## R workflow

```r
dat <- read_ma_workbook("review.xlsx")
validate_ma_data(dat)

# Two-stage data: one row per study/outcome with yi and sei
es_dat <- as_bayesma(
  dat,
  outcome = "pain_24h",
  model_type = "two_stage",
  measure = "MD"
)

# Arm-level data for one-stage or multivariate models
arm_dat <- as_bayesma(
  dat,
  outcome = "pain_24h",
  model_type = "one_stage"
)
```

Supported two-stage measures include odds ratios (`OR`), risk ratios (`RR`),
incidence-rate ratios (`IRR`), mean differences (`MD`), standardized mean
differences (`SMD`), and differences in medians (`MedD`). For median-reported
outcomes, `convert_median_to_mean()` can estimate means and SDs using the Wan
or quantile-estimation (`qe`) method. These calculations use the optional
`metamedian` package.

## Risk of bias and INSPECT-SR

Risk-of-bias records identify both the tool and assessment level. Use
`rob_level = "study"` for an assessment applied across a study's outcomes, or
`rob_level = "outcome"` when a rating applies to one outcome; outcome-level
records require an `outcome_id`.

The INSPECT-SR sheet includes the 21 assessment items and the auxiliary values
used for the Carlisle, participant-number consistency, GRIM, and p-value
checks. `inspect_sr_columns()` lists the fields and `inspect_sr_labels()` gives
their display labels. INSPECT-SR judgements are recorded by the reviewer; the
package does not infer an overall judgement from the auxiliary calculations.

## Package status

`bayesmaDatR` is under active development. The workbook, validation, and data
preparation functions are being expanded alongside package-specific exporters.
The current `as_bayesma()` interface prepares the common data frame; verify
model-specific requirements before fitting an analysis.
