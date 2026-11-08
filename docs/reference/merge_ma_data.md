# Combine all sections into one data frame

One row per study: the study's details first, then each outcome's
columns side by side, suffixed with the outcome (and time point, or
intervention arm, where an outcome has several), e.g.
`outcome_id_hosp_los`, `n_int_hosp_los`, `mean_ctrl_icu_los`. Outcome
values include those inherited from Study Details. Risk-of-bias ratings
(prefixed `rob_`, suffixed by outcome when they are outcome-level) and
INSPECT-SR fields (prefixed `inspect_`) come last. Columns that are
empty for every study are left out.

## Usage

``` r
merge_ma_data(data)
```

## Arguments

- data:

  An `ma_data` object.

## Value

A data frame with one row per study.
