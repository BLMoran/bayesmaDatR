# Create a Bayesian meta-analysis data object

Create a Bayesian meta-analysis data object

## Usage

``` r
new_ma_data(
  studies = data.frame(),
  outcomes = data.frame(),
  risk_of_bias = data.frame(),
  inspect_sr = data.frame()
)
```

## Arguments

- studies:

  Study-level metadata. Must include `study_id` when non-empty.

- outcomes:

  Outcome-level data, one row per study, outcome, time point and
  comparison.

- risk_of_bias:

  Optional risk-of-bias data. See
  [`add_rob()`](https://blmoran.github.io/bayesmaDatR/reference/add_rob.md).

- inspect_sr:

  Optional INSPECT-SR data, generally one row per study.

## Value

An object of class `ma_data`: a list of four data frames.
