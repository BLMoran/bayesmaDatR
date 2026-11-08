# Add risk-of-bias assessments

Appends assessments for one tool to the `risk_of_bias` section,
recording the tool and whether each assessment applies to the whole
study or to one outcome. Study-level ratings are later joined to every
outcome from that study; outcome-level ratings only to their own
`outcome_id`.

## Usage

``` r
add_rob(data, rob, rob_tool, ..., rob_level = c("study", "outcome"))
```

## Arguments

- data:

  An `ma_data` object.

- rob:

  A data frame with `study_id`, the tool's domain columns (see
  [`rob_domains()`](https://blmoran.github.io/bayesmaDatR/reference/rob_domains.md))
  and, for `rob_level = "outcome"`, `outcome_id`.

- rob_tool:

  Tool identifier from
  [`rob_tools()`](https://blmoran.github.io/bayesmaDatR/reference/rob_tools.md).

- ...:

  Unused; must be empty.

- rob_level:

  `"study"` or `"outcome"`.

## Value

`data` with the assessments appended. Judgements are standardised to the
tool's levels (see
[`rob_judgements()`](https://blmoran.github.io/bayesmaDatR/reference/rob_judgements.md)).
