# Read a workbook into an `ma_data` object

Read a workbook into an `ma_data` object

## Usage

``` r
read_ma_workbook(path)
```

## Arguments

- path:

  Path to an `.xlsx` workbook. Recognised sheets: `studies`, `outcomes`,
  `risk_of_bias`, and `inspect_sr` (case-insensitive; the legacy names
  `study_details`, `outcome_data` and `rob_data` are also accepted).
  Unknown sheets are ignored. Missing or empty sheets are replaced by
  the blank template.

## Value

An `ma_data` object.
