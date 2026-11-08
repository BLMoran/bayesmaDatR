# Select outcome rows

Select outcome rows

## Usage

``` r
select_outcome(data, outcome = NULL, timepoint = NULL)
```

## Arguments

- data:

  An `ma_data` object or outcome data frame.

- outcome:

  Outcome identifier(s), matched against `outcome_id` and then
  `outcome_name`. `NULL` keeps every outcome.

- timepoint:

  Time point(s) to keep. `NULL` keeps every time point.

## Value

The matching outcome rows, with study-level fields from `studies` and
risk-of-bias ratings (prefixed `rob_`) joined on when `data` is an
`ma_data` object.
