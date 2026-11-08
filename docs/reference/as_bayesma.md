# Export outcomes for bayesma

Checks that the workbook holds what the chosen bayesma model needs and
returns data in bayesma's input structure.

## Usage

``` r
as_bayesma(
  data,
  outcome = NULL,
  ...,
  timepoint = NULL,
  model_type = c("two_stage", "one_stage"),
  measure = NULL,
  format = c("wide", "long"),
  estimate_means = c("none", "wan", "qe"),
  design_effect = c("apply", "ignore")
)
```

## Arguments

- data:

  An `ma_data` object or outcome data frame.

- outcome:

  Outcome identifier(s), matched against `outcome_id` and then
  `outcome_name`. `NULL` keeps every outcome.

- ...:

  Unused; must be empty.

- timepoint:

  Time point(s) to keep. `NULL` keeps every time point.

- model_type:

  `"two_stage"` or `"one_stage"`, as in bayesma's `stage`.

- measure:

  Effect measure for every outcome. `NULL` uses each outcome's `measure`
  column.

- format:

  `"wide"` or `"long"`; see Description.

- estimate_means:

  For `"MD"` and `"SMD"`: `"none"` (default) keeps studies as reported,
  so studies reporting only medians are flagged as incomplete; `"wan"`
  or `"qe"` first estimates missing means and SDs from medians,
  quartiles and ranges with
  [`convert_median_to_mean()`](https://blmoran.github.io/bayesmaDatR/reference/convert_median_to_mean.md).

- design_effect:

  `"apply"` (default) or `"ignore"`; see
  [`compute_effect_size()`](https://blmoran.github.io/bayesmaDatR/reference/compute_effect_size.md).

## Value

A data frame.

## Details

- `format = "wide"`: one row per study for
  [`bayesma::bayesma()`](https://blmoran.github.io/bayesma/reference/bayesma.html),
  with the outcome's own `n_int`/`n_ctrl` and the arm summaries named as
  bayesma's defaults (`event_*`, `mean_*`, `sd_*`, `time_*`, `median_*`,
  ...). For `model_type = "two_stage"`, `yi` and `sei` are appended. Use
  `study_var = study_id` (or `author` with `year_var = year`) and, for
  `model_type = "bc_jung"`, `rob_var = rob_Overall`. The reported sample
  sizes are kept; a `design_effect` column (see
  [`compute_design_effect()`](https://blmoran.github.io/bayesmaDatR/reference/compute_design_effect.md))
  is added for `bayesma(design_effect = design_effect)`, and two-stage
  `yi`/`sei` already use the effective sample sizes.

- `format = "long"`: one row per arm for
  [`bayesma::bayesma_mv()`](https://blmoran.github.io/bayesma/reference/bayesma_mv.html)
  (several outcomes),
  [`bayesma::bayesma_long()`](https://blmoran.github.io/bayesma/reference/bayesma_mv.html)
  (several time points) or multi-arm models, with columns `arm_id`,
  `events`, `n`, `mean`, `sd` and `exposure`. Use `arm_var = arm_id`.

Studies with no summary data for an outcome (e.g. rows added by "Sync
studies" for studies that did not report it) are left out of that
outcome, with a message. Studies with partial data (e.g. a mean without
an SD) are kept with a warning naming the missing fields; `yi`/`sei` are
left empty where they cannot be calculated.

## Several outcomes

With more than one outcome (or time point) in wide format, outcomes are
placed side by side: one row per study, study details once, and each
outcome's columns suffixed with its `outcome_id` (and time point, when
an outcome has several), e.g. `outcome_id_hosp_los`, `yi_hosp_los`,
`sei_icu_los`. Studies that did not report an outcome have empty cells
for it. Each outcome uses its own `measure` unless `measure` is given.
With a single outcome, columns keep their plain names (`yi`, `sei`,
...).
