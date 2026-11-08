# Design effect for clustered and crossover trials

Computes each study's design effect from its trial design. Effect sizes
divide counts and sample sizes by it to give effective sample sizes; the
reported sizes are kept unchanged in the data.

## Usage

``` r
compute_design_effect(
  trial_design,
  ...,
  n_clusters = NA_real_,
  n_periods = NA_real_,
  n_total = NA_real_,
  cluster_size = NA_real_,
  rho_w = NA_real_,
  rho_b = NA_real_,
  rho_wp = NA_real_,
  labels = NULL
)
```

## Arguments

- trial_design:

  Trial design of each study.

- ...:

  Unused; must be empty.

- n_clusters:

  Number of clusters, \\k\\ above.

- n_periods:

  Number of periods, \\p\\ above.

- n_total:

  Total sample size, `n_int + n_ctrl`, used with `n_clusters`.

- cluster_size:

  Mean participants per cluster (per cluster-period for cluster
  crossover trials), `m` above. Overrides the value derived from
  `n_clusters`.

- rho_w:

  Within-period intracluster correlation.

- rho_b:

  Between-period intracluster correlation.

- rho_wp:

  Within-person correlation across periods.

- labels:

  Study labels used in messages.

## Value

A numeric vector the same length as `trial_design`.

## Details

|                |                                 |
|----------------|---------------------------------|
| `trial_design` | Design effect                   |
| `"rct"`        | \\1\\                           |
| `"c_rct"`      | \\1 + (m - 1)\rho_w\\           |
| `"xo_rct"`     | \\(1 - \rho\_{wp}) / 2\\        |
| `"cxo_rct"`    | \\1 + (m - 1)\rho_w - m\rho_b\\ |

where \\m\\ is the mean cluster size (per cluster-period for cluster
crossover trials). It is taken from `cluster_size` when given, and
otherwise derived as \\m = n\_{total} / (k \times p)\\, where \\k\\ is
`n_clusters` and \\p\\ is `n_periods` (1 for cluster trials and 2 for
cluster crossover trials when `n_periods` is missing).

Missing inputs give a design effect of 1: a cluster crossover trial
without `rho_b` uses \\\rho_b = 0\\, and any design whose formula still
cannot be evaluated (e.g. no `rho_w` or no `cluster_size`) is treated
as 1. bayesma's names (`"parallel"`, `"cluster"`, `"crossover"`,
`"cluster_crossover"`) are also accepted, and a missing design is
treated as `"rct"`.

For crossover trials the formula assumes that `n_ctrl + n_int` is the
number of participants, so a trial of 40 participants is entered as 20
and 20.

## Examples

``` r
compute_design_effect(c("rct", "c_rct", "xo_rct", "cxo_rct"),
                      n_clusters = c(NA, 10, NA, 10),
                      n_total = c(100, 200, 40, 400),
                      rho_w = c(NA, 0.05, NA, 0.05),
                      rho_wp = c(NA, NA, 0.6, NA))
#> [1] 1.00 1.95 0.20 1.95
```
