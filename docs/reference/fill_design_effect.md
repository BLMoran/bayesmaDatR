# Add the design effect column

Calculates
[`compute_design_effect()`](https://blmoran.github.io/bayesmaDatR/reference/compute_design_effect.md)
for each outcome row and stores it in `design_effect`, replacing any
earlier value. Each input (`trial_design`, `n_int`, `n_ctrl`,
`n_clusters`, `n_periods`, `rho_w`, `rho_b`, `rho_wp`) comes from the
outcome row, or from Study Details where the outcome's cell is empty
(see
[`fill_study_defaults()`](https://blmoran.github.io/bayesmaDatR/reference/fill_study_defaults.md)).
The column is for display: effect sizes and exports always recalculate
the design effect from these inputs.

## Usage

``` r
fill_design_effect(data)
```

## Arguments

- data:

  An `ma_data` object.

## Value

`data` with the `design_effect` column updated.
