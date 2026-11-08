# Validate managed meta-analysis data

Checks identifiers, outcome summaries, trial-design fields, risk-of-bias
assessments and INSPECT-SR fields.

## Usage

``` r
validate_ma_data(data)
```

## Arguments

- data:

  An `ma_data` object.

## Value

A tibble with one row per issue (`section`, `row`, `column`, `issue`);
zero rows means no issues were found.
