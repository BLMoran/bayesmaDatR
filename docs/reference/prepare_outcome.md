# Prepare a single outcome for analysis

Package-neutral preparation: arm-level data for one-stage models, or
`yi` and `sei` for two-stage models.

## Usage

``` r
prepare_outcome(
  data,
  outcome,
  ...,
  timepoint = NULL,
  model_type = c("two_stage", "one_stage"),
  measure = NULL
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

  `"two_stage"` (effect sizes) or `"one_stage"` (arm-level data).

- measure:

  Effect measure for two-stage preparation. `NULL` uses the outcome's
  `measure` column.

## Value

A data frame.
