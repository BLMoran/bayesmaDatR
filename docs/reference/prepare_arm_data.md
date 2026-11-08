# Prepare arm-level data

Arm-level data for one-stage, multivariate, multi-arm and longitudinal
models. Pairwise rows are reshaped to one row per study, outcome, time
point and arm, keeping each outcome's own sample sizes, the trial design
and its correlation fields. For multi-arm studies, enter one row per
comparison and name the arms in `int_arm` and `ctrl_arm`; a control arm
shared by several comparisons appears once.

## Usage

``` r
prepare_arm_data(
  data,
  outcome = NULL,
  ...,
  timepoint = NULL,
  format = c("long", "wide")
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

- format:

  `"long"` (one row per arm) or `"wide"` (one row per comparison, as
  entered).

## Value

A data frame. In long format, the columns are the identifiers
(`study_id`, `outcome_id`, `timepoint`, `arm_id`), `arm` (`"ctrl"` or
`"int"`), the arm summaries (`n`, `event`, `mean`, `sd`, `median`, `q1`,
`q3`, `min`, `max`, `time`) and the remaining outcome-level columns.
