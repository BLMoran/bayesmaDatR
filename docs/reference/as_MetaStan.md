# Export one outcome for MetaStan

Builds MetaStan's arm-based input: one row per study with the number of
arms `nd` and numbered arm columns, arm 1 being the control. Multi-arm
studies (several rows sharing a control arm, see
[`prepare_arm_data()`](https://blmoran.github.io/bayesmaDatR/reference/prepare_arm_data.md))
get one column set per arm.

## Usage

``` r
as_MetaStan(
  data,
  outcome = NULL,
  ...,
  timepoint = NULL,
  measure = NULL,
  estimate_means = c("none", "wan", "qe"),
  design_effect = c("apply", "ignore"),
  output = c("metastan", "wide")
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

- measure:

  Effect measure for every outcome. `NULL` uses each outcome's `measure`
  column.

- estimate_means:

  For `"MD"` and `"SMD"`: `"none"` (default) keeps studies as reported,
  so studies reporting only medians are flagged as incomplete; `"wan"`
  or `"qe"` first estimates missing means and SDs from medians,
  quartiles and ranges with
  [`convert_median_to_mean()`](https://blmoran.github.io/bayesmaDatR/reference/convert_median_to_mean.md).

- design_effect:

  `"apply"` (default) or `"ignore"`; see
  [`compute_effect_size()`](https://blmoran.github.io/bayesmaDatR/reference/compute_effect_size.md).

- output:

  `"metastan"` (default) returns the result of
  [`MetaStan::create_MetaStan_dat()`](https://rdrr.io/pkg/MetaStan/man/create_MetaStan_dat.html),
  ready for
  [`MetaStan::meta_stan()`](https://rdrr.io/pkg/MetaStan/man/meta_stan.html);
  `"wide"` returns the one-row-per-study data frame it is built from.

## Value

For `"metastan"`, MetaStan's list (`data_long`, `data_wide`); for
`"wide"`, a data frame. Both carry the MetaStan likelihood in
`attr(x, "likelihood")`.

## Details

|  |  |  |
|----|----|----|
| `measure` | MetaStan `likelihood` | Arm columns |
| `"OR"`, `"RR"` | `"binomial"` | `r1`, `n1`, ... (events, sample size) |
| `"MD"` | `"normal"` | `M1`, `SE1`, ... (mean, standard error) |
| `"IRR"` | `"poisson"` | `count1`, `exposure1`, ... |

Arm sizes, events and person-time are divided by the design effect (see
[`compute_design_effect()`](https://blmoran.github.io/bayesmaDatR/reference/compute_design_effect.md));
counts are then rounded, as the binomial and Poisson likelihoods need
whole numbers. The reported sample sizes are kept as `n_reported1`,
`n_reported2`, ... for forest plots.

## Examples

``` r
if (FALSE) { # \dontrun{
metastan_dat <- as_MetaStan(dat, outcome = "pain_24h", measure = "MD")
MetaStan::meta_stan(metastan_dat, likelihood = attr(metastan_dat, "likelihood"),
                    theta_prior = c(0, 1), tau_prior = 0.5)
} # }
```
