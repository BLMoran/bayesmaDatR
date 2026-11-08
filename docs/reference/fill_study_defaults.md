# Fill outcome rows with their study's default values

Study Details can hold `trial_design`, `n_int`, `n_ctrl`, `n_clusters`,
`rho_w`, `rho_b` and `rho_wp` for the whole study. An outcome row uses
its own value where it has one (e.g. an outcome-specific sample size)
and the study's value where its cell is empty.

## Usage

``` r
fill_study_defaults(outcomes, studies)
```

## Arguments

- outcomes:

  Outcome data frame.

- studies:

  Study Details data frame.

## Value

`outcomes` with the study defaults filled in.
