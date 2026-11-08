# Estimate study-level effect sizes

Calculates `yi` and `sei` with the same rules as bayesma's two-stage
models:

## Usage

``` r
compute_effect_size(
  data,
  measure = c("OR", "RR", "IRR", "MD", "SMD", "MedD"),
  ...,
  continuity = 0.5,
  design_effect = c("apply", "ignore")
)
```

## Arguments

- data:

  Outcome data frame in the canonical schema (see
  [`template_ma_data()`](https://blmoran.github.io/bayesmaDatR/reference/template_ma_data.md)).

- measure:

  Effect measure.

- ...:

  Unused; must be empty.

- continuity:

  Continuity correction for zero cells in binary and count outcomes.

- design_effect:

  `"apply"` (default) uses effective sample sizes; `"ignore"` uses the
  reported sizes for every study.

## Value

`data` with `design_effect`, `n_int_eff`, `n_ctrl_eff`, `yi` and `sei`
columns added.

## Details

- `"OR"`, `"RR"`: `event_*` and `n_*`, with a continuity correction for
  zero (or, for RR, full) cells.

- `"IRR"`: `event_*` and person-time `time_*`.

- `"MD"`, `"SMD"` (Hedges' g): `mean_*`, `sd_*` and `n_*`.

- `"MedD"`: difference in medians by quantile estimation (McGrath et
  al., 2020) via metamedian, from whichever of minimum/median/maximum,
  quartiles, or mean/SD each arm reports.

Ratios are on the log scale. Rows lacking the required summaries get
`NA`; for `"MedD"`, a warning names the studies that cannot be
estimated.

Clustered and crossover trials are handled with the design effect (see
[`compute_design_effect()`](https://blmoran.github.io/bayesmaDatR/reference/compute_design_effect.md)):
sample sizes, events and person-time are divided by it before `yi` and
`sei` are calculated. The reported `n_int` and `n_ctrl` are kept, so
forest plots can show them; the effective sizes are added as `n_int_eff`
and `n_ctrl_eff`. The design effect is always calculated from the design
inputs (missing correlations give 1); a `design_effect` column already
in `data` is replaced.
