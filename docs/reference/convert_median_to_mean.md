# Estimate means and standard deviations from medians and quantiles

Fills in `mean_<arm>` and `sd_<arm>` where they are missing, using
metamedian, from whichever of median, quartiles and range each study
reports. Reported means and SDs are never overwritten. The column
`mean_<arm>_method` records whether each value was `"reported"`,
estimated (`"wan"` or `"qe"`), or could not be estimated (`NA`).

## Usage

``` r
convert_median_to_mean(
  data,
  arm = c("int", "ctrl"),
  ...,
  method = c("wan", "qe")
)
```

## Arguments

- data:

  Outcome data frame in the canonical schema.

- arm:

  Arm(s) to fill: `"int"`, `"ctrl"`, or both (the default).

- ...:

  Unused; must be empty.

- method:

  `"wan"` (Hozo/Wan/Bland, via metamedian's `"hozo/wan/bland"`) or
  `"qe"` (quantile estimation).

## Value

`data` with the arms' mean, SD and method columns filled in.
